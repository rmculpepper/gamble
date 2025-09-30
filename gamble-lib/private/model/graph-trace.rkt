;; Copyright (c) 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/id-table
                     syntax/stx
                     syntax/parse/experimental/template
                     "analysis.rkt"
                     "known-functions.rkt")
         racket/match
         racket/class
         racket/stxparam
         "../addr.rkt"
         "../base.rkt"
         (only-in "../dist.rkt" dist-pdf)
         (submod "../dist.rkt" meta)
         (only-in "../util/dnum.rkt" dnum->logspace-real)
         "instrument.rkt")
(provide (all-defined-out))

;; If true, all non-model functions are considered constant-folding.
(define ALL-CONSTANT-FOLDING? #t)

;; Summary of re-evaluation restrictions: Re-evaluation must not change
;; - which branch of an `if` expression is taken
;; - the closure/procedure value of an application expression
;;   - but the values in a closure's environment are allowed to change
;; - the tag value of a call to `sample`
;; - the closure/procedure value of a call to `mem`
;; - the argument values in an application of a memoized function
;; - the model value of a call to `run-model`
;; - the value of an expression wrapped with `structural`


;; Memoization issue: if update to memoized computation happens, must update
;; all dependent computations. Possible restrictions:
;;  1. arguments must always be same (both on misses and on hits)
;;  2. arguments must be same for misses, may vary for hits
;;     but then all hits must depend on some loc for updates
;; Implement #1 for now.

;; IDEA: `structural` hint, produces node:same, treat value as const
;;   (define n (structural (sample (binomial-dist 10 1/2))))
;;   (for/sum ([i n]) (sample (uniform-dist 0 1)))
;; Without `structural`, there is a separate node:same-if for each iteration
;; of the for/sum loop; each comparison takes a location, etc.

;; IDEA: track `box` contents by location
;;   (define vs (for/list ([i 10]) (box (sample (uniform-dist 0 1)))))
;;   (unbox (list-ref vs 4))
;; If one element changes, no need to update entire list.
;; But `set-box!` not allowed! (Even if tracked, not safe to time travel
;; while sharing one mutable location! Same problem with `set!`.)
;;
;; Another problem: can't use a single actual Racket box in multiple threads
;; simultaneously. Maybe better have new `cell`, `cell-ref`?
;; Or maybe don't mutate box, just store #<opaque> in it, track identity.
;;
;;   (cell v) = (let ([x v]) (lambda () x))
;;   (cell-ref c) = (c)

;; ============================================================

(define-syntax-parameter GRAPH
  (lambda (stx) (wrong-syntax stx "used out of context")))

;; ================================================================================

;; instrument/graph-top : Expr[X] -> Expr[Graph Address -> X]
(define-syntax (instrument/graph-top stx)
  ;; Note: must be kept in sync with `with-ctx` and `model` expressions.
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ (#%plain-lambda (graph)
          (let-values ctx-bindings body:expr)))
     #'(#%plain-lambda (graph addr)
         (let-values ctx-bindings
           (syntax-parameterize ((ADDR (make-rename-transformer
                                        (quote-syntax addr)))
                                 (GRAPH (make-rename-transformer
                                         (quote-syntax graph))))
             (letrec-syntaxes ([(instrument)
                                (make-instrument/graph (quote-syntax instrument))])
               (instrument body)))))]))

(define-syntax (declare-local-variables stx)
  (syntax-parse stx
    [(_ vars ...)
     (for ([vars (in-list (syntax->list #'(vars ...)))])
       (add-local-vars! vars))
     (case (syntax-local-context)
       [(expression) #'(void)]
       [else #'(begin)])]))

(begin-for-syntax
  ;; local-variables : FreeIdTable[ #t ]
  (define local-variables (make-free-id-table))
  (define (add-local-vars! formals)
    (let loop ([formals formals])
      (cond [(stx-pair? formals)
             (free-id-table-set! local-variables (stx-car formals) #t)
             (loop (stx-cdr formals))]
            [(identifier? formals)
             (free-id-table-set! local-variables formals #t)]
            [else (void)])))

  ;; ----------------------------------------
  ;; Graph-slicing Instrumenter

  ;; make-instrument/graph : Id -> (Syntax[ExpandedForm] -> Syntax[Form])
  ;; PRE: syntax is fully-expanded expression, tagged, and analyzed
  ;; PRE: result is used in context of binding of ADDR, CALL-SITE-BASE, GRAPH
  (define (make-instrument/graph instrument-id)
    (define/with-syntax instrument instrument-id)
    (define (instrumenter istx)
      (define stx (syntax-case istx () [(_ ee) #'ee]))
      (define/with-syntax ee stx)
      (define result
        (syntax-parse stx
          #:literal-sets (kernel-literals)
          #:literals (apply void values variable-reference-from-unsafe?)
          ;; ----------------------------------------
          ;; Special patterns
          [(if (#%plain-app variable-reference-from-unsafe? (#%variable-reference)) e2 e3)
           #'(instrument e3)]
          ;; ----------------------------------------
          ;; Expressions
          [var:id
           (cond [(free-id-table-ref local-variables #'var #f) #'var]
                 [else #'(result:value var)])]
          [(#%plain-lambda ~! (var:id ...) e ...)
           #'(result:value
              (model-closure
               (#%plain-lambda (addr var ...)
                 (declare-local-variables (var ...))
                 (syntax-parameterize ((ADDR (make-rename-transformer
                                              (quote-syntax addr))))
                   (instrument e) ...))))]
          [(case-lambda ~! [(var ...) e ...] ...)
           #'(result:value
              (model-closure
               (case-lambda
                 [(addr var ...)
                  (declare-local-variables (var ...))
                  (syntax-parameterize ((ADDR (make-rename-transformer
                                               (quote-syntax addr))))
                    (instrument e) ...)]
                 ...)))]
          [(if ~! e1 e2 e3)
           #'(if (graph-if GRAPH (instrument e1))
                 (instrument e2)
                 (instrument e3))]
          [(begin ~! e ...)
           #'(begin (instrument e) ...)]
          [(begin0 ~! e0 e ...)
           #'(begin0 (instrument e0) (instrument e) ...)]
          [(let-values ~! ([vars rhs] ...) body ...)
           #'(let-values ([vars (instrument rhs)] ...)
               (declare-local-variables vars ...)
               (instrument body) ...)]
          [(letrec-values ~! ([vars rhs] ...) body ...)
           #'(letrec-values ([() (begin (declare-local-variables vars ...) (values))]
                             [vars (instrument rhs)] ...)
               (instrument body) ...)]
          [(set! ~! var e)
           (cond [(free-id-table-ref local-variables #'var #f)
                  #'(set! var (instrument e))]
                 [else (raise-syntax-error #f "unsupported in model with tracing" stx)])]
          [(quote ~! d)
           #'(result:value ee)]
          [(quote-syntax . _)
           #'(result:value ee)]
          [(with-continuation-mark ~! e1 e2 e3)
           #'(with-continuation-mark
               (result->value (instrument e1))
               (result->value (instrument e2))
               (instrument e3))]
          [(#%top . _) #'(result:value ee)]
          [(#%variable-reference . _) #'(result:value ee)]
          [(#%expression ~! e)
           #'(#%expression (instrument e))]
          ;; --------------------
          ;; Applications
          [(#%plain-app)
           #'(result:value null)]
          [(#%plain-app void arg:expr ...)
           #'(begin (void (instrument arg)) ... (result:value (void)))]
          [(#%plain-app values arg:expr ...)
           #'(values (instrument arg) ...)]
          [(#%plain-app apply fun:id arg:expr ...)
           #:when (constant-folding-procedure-id? #'fun)
           #'(graph-apply-cf GRAPH fun (instrument arg) ...)]
          [(#%plain-app fun:id arg:expr ...)
           #:when (constant-folding-procedure-id? #'fun)
           #'(graph-app-cf GRAPH fun (instrument arg) ...)]
          [(#%plain-app fun:expr arg:expr ...)
           (define/with-syntax cs (CALL-SITE stx))
           (define/with-syntax addr-expr #'(addr-add-call ADDR (+ CSBASE cs)))
           #'(graph-app GRAPH addr-expr (instrument fun) (instrument arg) ...)]
          ;; ----------------------------------------
          [_ (raise-syntax-error #f "unhandled syntax in instrumenter" stx)]))
      (let ([result (relocate result stx)])
        (cond [(eq? result stx) result]
              [(stx-pair? stx) (syntax-track-origin result stx (stx-car stx))]
              [else result])))
    instrumenter))

;; ============================================================

(struct model-function ()
  #:property prop:procedure
  (lambda (self . args)
    (error 'application "model function applied out of context\n  given: ~e" self)))

(struct model-closure model-function (proc))
(struct model-memoized model-function (proc args=>result addr))

;; A Node is one of
;; - (node:same-if Boolean result)                  -- enforce same branch
;; - (node:same String Any Result)                  -- enforce same proc/closure/etc
;; - (node:app Location Procedure (Listof Result))  -- app w/ single result value
;; - (node:app-mv (Listof Location) Procedure (Listof Result))
(struct node:same-if (branch result) #:prefab)
(struct node:same (kind val result) #:prefab)
(struct node:app (loc fun argrs) #:prefab)
(struct node:app-mv (locs fun argrs) #:prefab)
(struct node:sample (loc addr distr tagr) #:prefab)
(struct node:dscore (argr) #:prefab)
(struct node:lscore (argr) #:prefab)
(struct node:observe (distr valr) #:prefab)
(struct node:fail (argr) #:prefab)

;; node-locations : Node -> (values (Listof Location) (Listof Location))
;; Returns reads-locations and writes-locations.
(define (node-locations node)
  (define (get-locs rs)
    (for/list ([r (in-list rs)] #:when (result:location? r))
      (result:location-location r)))
  (match node
    [(node:same-if branch result)
     (values (get-locs (list result)) null)]
    [(node:same kind val result)
     (values (get-locs (list result)) null)]
    [(node:app loc proc argrs)
     (values (get-locs argrs) (list loc))]
    [(node:app-mv locs proc argrs)
     (values (get-locs argrs) locs)]
    [(node:sample loc addr distr tagr)
     (values (get-locs (list distr tagr)) (list loc))]
    [(node:dscore argr) (values (get-locs (list argr)) null)]
    [(node:lscore argr) (values (get-locs (list argr)) null)]
    [(node:observe distr valr) (values (get-locs (list distr valr)) null)]
    [(node:fail argr) (values (get-locs argr) null)]
    ))

;; node->expr : Node (Hash Location Nat) -> Expr
(define (node->expr node [loc=>index (hasheq)])
  (define (loc-index loc)
    (hash-ref! loc=>index loc (lambda () (hash-count loc=>index))))
  (define (loc-ref loc) `(fetch ,(loc-index loc)))
  (define (loc-set! loc rhs) `(store! ,(loc-index loc) ,rhs))
  (define (result->expr result)
    (match result
      [(result:location loc) (loc-ref loc)]
      [(result:value val) `(quote ,val)]))
  (match node
    [(node:same-if branch result)
     `(unless (eq? (quote ,branch) (and ,(result->expr result) #t))
        (error-structural "if branch"))]
    [(node:same kind val result)
     `(unless (equal? (quote ,val) ,(result->expr result))
        (error-structural (quote ,kind)))]
    [(node:app loc proc argrs)
     (loc-set! loc `(#%app (quote ,proc) ,@(map result->expr argrs)))]
    [(node:app-mv locs proc argrs)
     `(store! ,(map loc-index locs) (#%app (quote ,proc) ,@(map result->expr argrs)))]
    [(node:sample loc addr distr tagr)
     (loc-set! loc `(sample ,(result->expr distr)
                            ,(and tagr (result->expr tagr))
                            (quote ,addr)))]
    [(node:dscore argr)
     `(dscore ,(result->expr argr))]
    [(node:lscore argr)
     `(lscore ,(result->expr argr))]
    [(node:observe distr valr)
     `(observe ,(result->expr distr) ,(result->expr valr))]
    [(node:fail argr)
     `(fail ,(result->expr argr))]
    ))

;; exec-nodes! : Symbol/#f (Vectorof Node) StochasticCtx -> Void
;; Perform node effects.
(define (exec-nodes! who nodev ctx)
  (for ([node (in-vector nodev)]) (exec-node! who node ctx)))

;; exec-node! : Symbol/#f Node StochasticCtx -> Void
;; Perform node effect.
(define (exec-node! who node ctx)
  (match node
    [(node:same-if branch result)
     (define new-branch (and (result->value result) #t))
     (unless (eq? new-branch branch)
       (error-structural 'evaluate-model who "if branch"))]
    [(node:same kind val result)
     (define new-val (result->value result))
     (unless (equal? new-val val)
       (error-structural 'evaluate-model who kind))]
    [(node:app loc proc argrs)
     (store! loc (apply proc (results->values argrs)))]
    [(node:app-mv locs proc argrs)
     (call-with-values
      (lambda () (apply proc (results->values argrs)))
      (lambda vs
        (unless (= (length vs) (length locs))
          (error-structural 'evaluate-model #f "function result arity"))
        (for ([loc (in-list locs)] [v (in-list vs)])
          (store! loc v))))]
    [(node:sample loc addr distr tagr)
     (let ([dist (result->value distr)]
           [tag (and tagr (result->value tagr))])
       (store! loc (send ctx sample dist tag addr)))]
    [(node:dscore argr)
     (send ctx dscore (result->value argr))]
    [(node:lscore argr)
     (send ctx lscore (result->value argr))]
    [(node:observe distr valr)
     (send ctx observe (result->value distr) (result->value valr))]
    [(node:fail argr)
     (send ctx fail (result->value argr))]
    ))

;; exec-stochastic-nodes! : (Vectorof Node) -> (Values Real Real)
;; Replay only sample/observe nodes to calculate priors and likelihoods of given slice.
(define (exec-stochastic-nodes! nodes)
  (define sumlprs 0.0)
  (define sumlobs 0.0)
  (for ([node (in-vector nodes)])
    (match node
      [(node:sample loc addr distr tagr)
       (set! sumlprs
             (+ sumlprs (dist-pdf (result->value distr) (fetch loc) #t)))]
      [(node:dscore argr)
       (set! sumlobs
             (+ sumlobs (dnum->logspace-real (result->value argr))))]
      [(node:lscore argr)
       (set! sumlobs
             (+ sumlobs (result->value argr)))]
      [(node:observe distr valr)
       (set! sumlobs
             (+ sumlobs (dist-pdf (result->value distr) (result->value valr) #t)))]
      [_ (void)]))
  (values sumlprs sumlobs))


;; ============================================================
;; Result

;; A Result is one of
;; - (result:location Location)
;; - (result:value Any)         -- constant given branch choices
(struct result:location (location) #:prefab)
(struct result:value (value) #:prefab)

;; ============================================================
;; Store

(define (next-location) (box #f))
(define (store! loc val) (set-box! loc val))
(define (fetch loc) (unbox loc))

;; result->value : Result -> Any
(define (result->value r)
  (match r
    [(result:location loc) (fetch loc)]
    [(result:value val) val]))
(define (results->values rs)
  (for/list ([r (in-list rs)]) (result->value r)))

;; ============================================================

(define (graph-if graph branchr)
  (define branch (and (result->value branchr) #t))
  (send graph do! (node:same-if branch branchr))
  branch)

(define (graph-app-cf graph fun . argrs)
  ;; All constant-folding functions are also single-valued.
  (cond [(andmap result:value? argrs)
         (result:value (apply fun (results->values argrs)))]
        [else
         (define loc (box #f))
         (send graph do! (node:app loc fun argrs))
         (result:location loc)]))

(define (graph-apply-cf graph fun . argrs)
  ;; All constant-folding functions are also single-valued.
  (cond [(andmap result:value? argrs)
         (result:value (apply apply fun (results->values argrs)))]
        [else
         (define loc (box #f))
         (send graph do! (node:app loc apply (cons (result:value fun) argrs)))
         (result:location loc)]))

;; graph-extract-function : ... -> (Address (Result X) ... -> (Result Y))
(define (graph-extract-function graph funr)
  (define fun (result->value funr))
  (send graph do! (node:same "application" fun funr))
  (match fun
    [(model-closure proc) proc]
    [_ (lambda (addr . argrs) (graph-app* graph addr fun argrs))]))

;; graph-app : Graph Address Result (Listof Result) -> Result
(define (graph-app graph addr funr . argrs)
  (define fun (result->value funr))
  (send graph do! (node:same "application" fun funr))
  (graph-app* graph addr fun argrs))

;; graph-app* : Graph Address ModelFunction (Listof Result) -> Result
(define (graph-app* graph addr fun argrs)
  (match fun
    [(model-closure proc)
     (apply proc addr argrs)]
    [(model-memoized mfun args=>result addr)
     (define args (results->values argrs))
     (for ([arg (in-list args)] [argr (in-list argrs)])
       (send graph do! (node:same "memoized function argument" arg argr)))
     ;; Memo key *must* be actual argument values, not result wrappers:
     ;; because program could compute same value in two locations.
     (hash-ref! args=>result args
                (lambda ()
                  (define addr* (addr-add-mem addr args))
                  (graph-app* graph addr* mfun argrs)))]
    [(== structural)
     (define args (results->values argrs))
     (for ([arg (in-list args)] [argr (in-list argrs)])
       (send graph do! (node:same "declared structural" arg argr)))
     (apply values (map result:value args))]
    [proc ;; procedure, or else let racket raise non-proc app error
     (define args (results->values argrs))
     (cond [(and ALL-CONSTANT-FOLDING? (andmap result:value? argrs))
            (call-with-values
             (lambda () (apply proc args))
             (case-lambda
               [(v) (result:value v)]
               [vs (apply values (map result:value vs))]))]
           [else
            (call-with-values
             (lambda () (apply proc args))
             (case-lambda
               [(v)
                (define loc (box v))
                (send graph add! (node:app loc proc argrs))
                (result:location loc)]
               [vs
                (define locs (map box vs))
                (send graph add! (node:app-mv locs proc argrs))
                (apply values (map result:location locs))]))])]))

;; ============================================================
;; Graph

;; IDEA: use custom context whose `get-functions` returns model closures?
;; (or new type, "model primitives"?)

(define graph%
  (class object%
    (init-field [init-addr init-hash-addr]
                [ctx (new scoring-stochastic-ctx%)])
    (super-new)

    (define loc=>nodeids (make-hasheqv))    ;; Location => (Listof NodeID), references
    (define nodeid=>node (make-hasheqv))    ;; NodeID => Node
    (define nodeid=>reach (make-hasheqv))   ;; NodeID => Reach
    (define key=>nodeid (make-hash))        ;; DBKey => NodeID
    (define final-result #f)                ;; Result, mutated

    (define/public (show [expr? #t])
      (parameterize ((print-reader-abbreviations #t))
        (define loc=>index (make-hasheq))
        (printf "Node trace (! reaches stochastic effect or structural check):\n")
        (for ([nodeid (in-range 0 nodeid-counter)])
          (when (hash-has-key? nodeid=>node nodeid)
            (define node (hash-ref nodeid=>node nodeid))
            (define expr (node->expr node loc=>index))
            (printf "  ~s ~a ~s\n" nodeid
                    (if (hash-ref nodeid=>reach nodeid #f) "!" ":")
                    (if expr? expr node))))
        (printf "Store (location to value mapping):\n")
        (define index=>loc (make-vector (hash-count loc=>index)))
        (for ([(loc index) (in-hash loc=>index)])
          (vector-set! index=>loc index loc))
        (for ([loc (in-vector index=>loc)] [index (in-naturals)])
          (printf "  ~s : ~e\n" index (unbox loc)))
        (printf "Address to location mapping:\n")
        (for ([(key nodeid) (in-hash key=>nodeid)])
          (printf "  ~s : ~s\n" key nodeid))))

    ;; ----------------------------------------
    ;; Run

    (define/public (eval-top who mdl)
      (match mdl
        [(model _ gproc _)
         (match (send ctx run-top (lambda () (gproc this init-addr)))
           [(list result)
            (set! final-result result)
            (calculate-reach!)
            (result->value result)]
           [#f (error who "failed to build trace graph")])]))

    ;; ----------------------------------------
    ;; Context functions

    (define/public (get-functions)
      (define (trace:sample addr distr [tagr #f])
        (define loc (box #f))
        (when tagr
          (do! (node:same "sample tag" (result->value tagr) tagr)))
        (do! (node:sample loc addr distr tagr))
        (result:location loc))
      (define (trace:dscore addr dr)
        (do! (node:dscore dr))
        (result:value (void)))
      (define (trace:lscore addr llr)
        (do! (node:lscore llr))
        (result:value (void)))
      (define (trace:observe addr distr valr)
        (do! (node:observe distr valr))
        (result:value (void)))
      (define (trace:fail addr [reasonr (result:value #f)])
        (do! (node:fail reasonr))
        (result:value (void)))
      (define (trace:mem addr funr)
        (define fun (result->value funr))
        (unless (or (model-function? fun) (procedure? fun))
          (raise-argument-error 'mem "(or/c model-function? procedure?)" fun))
        (do! (node:same "function for memoize" fun funr))
        (result:value (model-memoized fun (make-hash) addr)))
      (define (trace:run-model addr mdlr)
        (define mdl (result->value mdlr))
        (match mdl
          [(model _ gproc _)
           (do! (node:same "model" mdl mdlr))
           (gproc this addr)]
          [(? model?)
           (error 'run-model "~a\n  model: ~e"
                  "non-tracing model called from tracing model" mdl)]
          [_ (raise-argument-error 'run-model "model?" mdl)]))
      (values (model-closure trace:sample)
              (model-closure trace:dscore)
              (model-closure trace:lscore)
              (model-closure trace:observe)
              (model-closure trace:fail)
              (model-closure trace:mem)
              (model-closure trace:run-model)
              #f))

    ;; ----------------------------------------
    ;; Node trace, nodes

    (define nodeid-counter 0)
    (define/private (next-nodeid)
      (begin0 nodeid-counter (set! nodeid-counter (add1 nodeid-counter))))

    ;; add! : Node -> NodeID
    ;; Add node to node trace, update location dependencies.
    ;; Nodes must be added in execution order.
    (define/public (add! node)
      (define nodeid (next-nodeid))
      (define-values (readlocs writelocs) (node-locations node))
      (hash-set! nodeid=>node nodeid node)
      (for ([readloc (in-list readlocs)])
        (hash-update! loc=>nodeids readloc (lambda (v) (cons nodeid v)) null))
      nodeid)

    ;; do! : Node -> Void
    ;; Perform node effect and register node in node trace (if needed).
    ;; (Eg, assignments to constants do not need to be repeated.)
    (define/public (do! node)
      (define (add-and-exec! [node node])
        (begin0 (add! node) (exec-node! #f node ctx)))
      (match node
        [(node:same-if branch result)
         (when (result:location? result) (add-and-exec!))]
        [(node:same kind fun result)
         (when (result:location? result) (add-and-exec!))]
        [(node:sample loc addr distr tagr)
         (define nodeid (add! node))
         (hash-set! key=>nodeid addr nodeid)
         (exec-node! #f node ctx)]
        [_ (add-and-exec!)]))

    ;; ----------------------------------------
    ;; Re-evaluation

    ;; get-slice-eval : (Listof DBKey)
    ;;               -> (values (StochasticCtx Boolean -> Any) Real Real)
    (define/public (get-slice-eval keys)
      (define s (get-slice keys))
      (define-values (slice-lprs slice-lobs) (slice->lprs+lobs s))
      (values (lambda (ctx mini?) (slice-eval s ctx mini?))
              slice-lprs slice-lobs))

    ;; show-slice : (Listof DBKey) -> Void
    (define/public (show-slice keys)
      (define s (get-slice keys))
      (match-define (slice _ all-nodes min-nodes _) s)
      (begin ;; initialize loc=>index
        (define loc=>index (make-hasheq))
        (for ([nodeid (in-range 0 nodeid-counter)])
          (define node (hash-ref nodeid=>node nodeid #f))
          (when node (void (node->expr node loc=>index)))))
      (eprintf "Slice (+ minimal, - full):\n")
      (parameterize ((print-reader-abbreviations #t))
        (for ([node all-nodes])
          (eprintf "  ~a ~s\n"
                   (if (for/or ([n (in-vector min-nodes)]) (eq? n node)) "+" "-")
                   (node->expr node loc=>index))))
      (eprintf "Posterior dist: ~e\n" (slice->posterior-dist s)))

    ;; get-slice : (Listof DBKey) -> Slice
    (define/public (get-slice keys)
      (define nodeids (get-slice-nodeids keys))
      (define all-nodes (nodeids->nodes nodeids #f))
      (define min-nodes (nodeids->nodes nodeids #t))
      (slice keys all-nodes min-nodes final-result))

    ;; get-slice-nodeids : (Listof DBKey) -> (Listof NodeID)
    (define/private (get-slice-nodeids keys)
      (define seen-nodeids (make-hasheqv))
      (define updated-locs (make-hasheq))
      (define (add-nodeids! nodeids)
        (for ([nodeid (in-list nodeids)])
          (unless (hash-ref seen-nodeids nodeid #f)
            (hash-set! seen-nodeids nodeid #t)
            (define node (hash-ref nodeid=>node nodeid))
            (define-values (readlocs writelocs) (node-locations node))
            (add-locs! writelocs))))
      (define (add-locs! locs)
        (for ([loc (in-list locs)])
          (unless (hash-ref updated-locs loc #f)
            (hash-set! updated-locs loc #t)
            (add-nodeids! (hash-ref loc=>nodeids loc null)))))
      (for ([key (in-list keys)])
        (let ([nodeid (hash-ref key=>nodeid key #f)])
          (when nodeid (add-nodeids! (list nodeid)))))
      (sort (hash-keys seen-nodeids) <))

    ;; Reach = Boolean, #t if reaches stochastic or same-check node

    ;; calculate-reach! : -> Void
    (define/private (calculate-reach!)
      (define loc=>defnodeid (make-hasheq)) ;; Location => NodeID
      (for ([(nodeid node) (in-hash nodeid=>node)])
        (define-values (readlocs writelocs) (node-locations node))
        (for ([writeloc (in-list writelocs)])
          (hash-set! loc=>defnodeid writeloc nodeid)))
      ;; ----
      (define (mark-nodeid nodeid reach)
        (unless (hash-ref nodeid=>reach nodeid #f)
          (hash-set! nodeid=>reach nodeid #t)
          (define node (hash-ref nodeid=>node nodeid))
          (define-values (readlocs writelocs) (node-locations node))
          (for ([readloc (in-list readlocs)])
            (define defnodeid (hash-ref loc=>defnodeid readloc))
            (mark-nodeid defnodeid #t))))
      (for ([(nodeid node) (in-hash nodeid=>node)])
        (cond [(or (node:same? node)
                   (node:same-if? node))
               (mark-nodeid nodeid #t)]
              [(or (node:sample? node)
                   (node:dscore? node)
                   (node:lscore? node)
                   (node:observe? node))
               (mark-nodeid nodeid #t)]
              [else (void)])))

    ;; nodeids->nodes : (Listof NodeID) Reach -> (Vectorof Node)
    ;; If only-reach? is true, only return same/stochastic-reaching nodes.
    (define/private (nodeids->nodes nodeids only-reach?)
      (list->vector
       (for/list ([nodeid (in-list nodeids)]
                  #:when (if only-reach? (hash-ref nodeid=>reach nodeid #f) #t))
         (hash-ref nodeid=>node nodeid))))
    ))

;; ============================================================

;; Slice = (slice (Listof DBKey) (Vectorof Node) (Vectorof Node) Result)
(struct slice (keys all-nodes min-nodes result))
(define no-result (string->uninterned-symbol "<<no-result>>"))

;; slice->lprs+lobs : Slice -> (values Real Real)
(define (slice->lprs+lobs s)
  (exec-stochastic-nodes! (slice-min-nodes s)))

;; slice-eval : Symbol/#f Slice StochasticCtx Boolean -> Any
(define (slice-eval who s ctx minimal?)
  (match-define (slice _ all-nodes min-nodes final-result) s)
  (exec-nodes! who (if minimal? min-nodes all-nodes) ctx)
  (if minimal? no-result (result->value final-result)))

;; slice->posterior-dist : Slice -> Dist/#f
;; Calculates the posterior dist of the RV sampled in first node,
;; using conjugacy relationships. Returns posterior or #f for failure.
;; (If dist returned, can be used for Gibbs step.)
(define (slice->posterior-dist s)
  (and (= 1 (length (slice-keys s))) (slice->posterior-dist* s)))

;; slice->posterior-dist* : Slice -> Dist/#f
(define (slice->posterior-dist* s)
  ;; PRE: slice over exactly one key
  (define nodes (slice-min-nodes s))
  ;; Pattern = #f | '_ | Real | (dist-symbol Pattern ...)
  (define loc=>pattern (make-hasheq)) ;; Location => Pattern
  (define (get-pattern r)
    (match r
      [(result:value v) (and (real? v) v)]
      [(result:location loc)
       (or (hash-ref loc=>pattern loc #f)
           (let ([v (fetch loc)]) (and (real? v) v)))]))
  (define (fun-pattern fun argps)
    (and (andmap values argps)
         (cond [(hash-ref function=>symbol fun #f)
                => (lambda (name) (cons name argps))]
               [else #f])))
  ;; ----
  (define dist
    (match (vector-ref nodes 0)
      [(node:sample loc _ distr _)
       (hash-set! loc=>pattern loc '_)
       (result->value distr)]))
  (and (conjugate-dist? dist)
       (for/and ([node (in-vector nodes 1)])
         (or (node:sample? node)
             (node:observe? node)
             (node:app? node)))
       (for/fold ([dist dist])
                 ([node (in-vector nodes 1)] #:break (not dist))
         (match node
           [(node:sample loc _ distr _)
            (dist-posterior dist (get-pattern distr) (fetch loc))]
           [(node:observe distr valr)
            (dist-posterior dist (get-pattern distr) (result->value valr))]
           [(node:app loc fun argrs)
            (hash-set! loc=>pattern loc (fun-pattern fun (map get-pattern argrs)))
            dist]))))
