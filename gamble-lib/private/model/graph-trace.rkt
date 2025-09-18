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
         (only-in "../util/density.rkt" density->real)
         "instrument.rkt")
(provide (all-defined-out))

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
           (raise-syntax-error #f "unsupported in model with tracing" stx)]
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

(struct model-closure (proc))
(struct model-memoized (proc args=>result addr))

(define (model-function? v)
  (or (model-closure? v) (model-memoized? v)))

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
        (raise-structural-change "if branch"))]
    [(node:same kind val result)
     `(unless (equal? (quote ,val) ,(result->expr result))
        (raise-structural-change (quote ,kind)))]
    [(node:app loc proc argrs)
     (loc-set! loc `(#%plain-app (quote ,proc) ,@(map result->expr argrs)))]
    [(node:app-mv locs proc argrs)
     `(store! ,(map loc-index locs) (#%plain-app (quote ,proc) ,@(map result->expr argrs)))]
    [(node:sample loc addr distr tagr)
     (loc-set! loc `(ctx-sample ,(result->expr distr)
                                ,(and tagr (result->expr tagr)) (quote ,addr)))]
    [(node:dscore argr)
     `(ctx-dscore ,(result->expr argr))]
    [(node:lscore argr)
     `(ctx-lscore ,(result->expr argr))]
    [(node:observe distr valr)
     `(ctx-observe ,(result->expr distr) ,(result->expr valr))]
    [(node:fail argr)
     `(ctx-fail ,(result->expr argr))]
    ))


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
    [(== begin-structural)
     (define args (results->values argrs))
     (for ([arg (in-list args)] [argr (in-list argrs)])
       (send graph do! (node:same "declared structural" arg argr)))
     (apply values (map result:value args))]
    [(? procedure? proc)
     (define args (results->values argrs))
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
         (apply values (map result:location locs))]))]
    [(model-memoized mfun args=>result addr)
     (define args (results->values argrs))
     (for ([arg (in-list args)] [argr (in-list argrs)])
       (send graph do! (node:same "memoized function argument" arg argr)))
     ;; Memo key *must* be actual argument values, not result wrappers:
     ;; because program could compute same value in two locations.
     (hash-ref! args=>result args
                (lambda ()
                  (define addr* (addr-add-mem addr args))
                  (graph-app* graph addr* mfun argrs)))]))

;; ============================================================
;; Graph

;; IDEA: use custom context whose `get-functions` returns model closures?
;; (or new type, "model primitives"?)

(define graph%
  (class object%
    (init-field ctx)
    (super-new)

    (define loc=>nodeids (make-hasheqv))    ;; Location => (Listof NodeID)
    (define nodeid=>node (make-hasheqv))    ;; NodeID => Node
    (define key=>nodeid (make-hash))        ;; DBKey => NodeID
    (define final-result #f)                ;; Result, mutated

    (define/public (show [expr? #t])
      (define loc=>index (make-hasheq))
      (printf "Node trace:\n")
      (for ([nodeid (in-range 0 nodeid-counter)])
        (when (hash-has-key? nodeid=>node nodeid)
          (define node (hash-ref nodeid=>node nodeid))
          (define expr (node->expr node loc=>index))
          (printf "  ~s : ~s\n" nodeid (if expr? expr node))))
      (printf "Store:\n")
      (define index=>loc (make-vector (hash-count loc=>index)))
      (for ([(loc index) (in-hash loc=>index)])
        (vector-set! index=>loc index loc))
      (for ([loc (in-vector index=>loc)] [index (in-naturals)])
        (printf "  ~s => ~e\n" index (unbox loc)))
      (printf "Key mapping:\n")
      (for ([(key nodeid) (in-hash key=>nodeid)])
        (printf "  ~s => ~s\n" key nodeid)))

    ;; ----------------------------------------
    ;; Run

    (define/public (run mdl)
      (match mdl
        [(model/tracing _ gproc _)
         (define result (gproc this (current-init-addr)))
         (set! final-result result)
         (result->value result)]))

    ;; ----------------------------------------
    ;; Context functions

    (define/public (get-functions)
      (define sample-prim
        (model-closure
         (lambda (addr distr [tagr #f])
           (define loc (box #f))
           (when tagr
             (do! (node:same "sample tag" (result->value tagr) tagr)))
           (do! (node:sample loc addr distr tagr))
           (result:location loc))))
      (define dscore-prim
        (model-closure
         (lambda (addr dr)
           (do! (node:dscore dr))
           (result:value (void)))))
      (define lscore-prim
        (model-closure
         (lambda (addr llr)
           (do! (node:lscore llr))
           (result:value (void)))))
      (define observe-prim
        (model-closure
         (lambda (addr distr valr)
           (do! (node:observe distr valr))
           (result:value (void)))))
      (define fail-prim
        (model-closure
         (lambda (addr [reasonr (result:value #f)])
           (do! (node:fail reasonr))
           (result:value (void)))))
      (define mem-prim
        (model-closure
         (lambda (addr funr)
           (define fun (result->value funr))
           (unless (or (procedure? fun) (model-function? fun))
             (raise-argument-error 'mem "(or/c procedure? model-function?)" fun))
           (do! (node:same "function for memoize" fun funr))
           (model-memoized fun (make-hash) addr))))
      (define run-model-prim
        (model-closure
         (lambda (addr mdlr)
           (define mdl (result->value mdlr))
           (unless (model? mdl)
             (raise-argument-error 'run-model "model?" mdl))
           (do! (node:same "model" mdl mdlr))
           (match mdl
             [(model/tracing _ gproc _)
              (gproc this addr)]
             [_ (error 'run-model
                       (string-append "non-tracing model called from tracing model"
                                      "\n  model: ~e")
                       mdl)]))))
      (values sample-prim
              dscore-prim
              lscore-prim
              observe-prim
              fail-prim
              mem-prim
              run-model-prim
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
        (begin0 (add! node) (exec-node! node)))
      (match node
        [(node:same-if branch result)
         (when (result:location? result) (add-and-exec!))]
        [(node:same kind fun result)
         (when (result:location? result) (add-and-exec!))]
        [(node:sample loc addr distr tagr)
         (define nodeid (add! node))
         (hash-set! key=>nodeid addr nodeid)
         (exec-node! node)]
        [_ (add-and-exec!)]))

    ;; exec-node! : Node StochasticCtx -> Void
    ;; Perform node effect.
    (define/public (exec-node! node [ctx ctx])
      (match node
        [(node:same-if branch result)
         (define new-branch (and (result->value result) #t))
         (unless (eq? new-branch branch)
           (error 'evaluate-model "structural change (if branch)"))]
        [(node:same kind val result)
         (define new-val (result->value result))
         (unless (equal? new-val val)
           (error 'evaluate-model "structural change (~a)" kind))]
        [(node:app loc proc argrs)
         (store! loc (apply proc (results->values argrs)))]
        [(node:app-mv locs proc argrs)
         (call-with-values
          (lambda () (apply proc (results->values argrs)))
          (lambda vs
            (unless (= (length vs) (length locs))
              (error 'evaluate-model "structural change (result arity)"))
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

    ;; exec-stochastic-nodes! : (Listof Node) -> (Values Real Real)
    ;; Replay only sample/observe nodes to calculate priors and likelihoods of given slice.
    (define/public (exec-stochastic-nodes! nodes)
      (define sumlprs 0.0)
      (define sumlobs 0.0)
      (for ([node (in-list nodes)])
        (match node
          [(node:sample loc addr distr tagr)
           (set! sumlprs
                 (+ sumlprs (dist-pdf (result->value distr) (fetch loc) #t)))]
          [(node:dscore argr)
           (set! sumlobs
                 (+ sumlobs (density->real (result->value argr) #t)))]
          [(node:lscore argr)
           (set! sumlobs
                 (+ sumlobs (result->value argr)))]
          [(node:observe distr valr)
           (set! sumlobs
                 (+ sumlobs (dist-pdf (result->value distr) (result->value valr) #t)))]
          [_ (void)]))
      (values sumlprs sumlobs))

    ;; ----------------------------------------
    ;; Re-evaluation

    ;; get-slice-eval : (Listof DBKey) (Listof NodeID)
    ;;               -> (values (StochasticCtx Boolean -> Any) Real Real)
    (define/public (get-slice-eval #:keys [keys null]
                                   #:nodeids [nodeids null])
      (define-values (nodes updated-locs) (get-slice keys nodeids))
      (define-values (slice-lprs slice-lobs) (exec-stochastic-nodes! nodes))
      (values (get-slice-proc/interp nodes) slice-lprs slice-lobs))

    (define/private (get-slice-proc/interp nodes)
      (lambda (ctx)
        (for ([node (in-list nodes)])
          (exec-node! node ctx))
        (result->value final-result)))

    ;; get-slice-expr : (Listof DBKey) (Listof NodeID) -> Expr
    (define/public (get-slice-expr #:keys [keys null]
                                   #:nodeids [nodeids null])
      (define-values (nodes updated-locs) (get-slice keys nodeids))
      `(begin
         ,@(for/list ([node (in-list nodes)])
             (node->expr node updated-locs))
         ,(match final-result
            [(result:value val) `(quote ,val)]
            [(result:location loc)
             `(fetch ,(hash-ref updated-locs loc '??))])))

    ;; get-slice : (Listof DBKey) (Listof NodeID) Boolean
    ;;           -> (values (Listof NodeID) (Hash Location (U Nat #t)))
    (define/private (get-slice keys nodeids [make-names? #t])
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
            (hash-set! updated-locs loc (if make-names? (hash-count updated-locs) #t))
            (add-nodeids! (hash-ref loc=>nodeids loc null)))))
      (add-nodeids! nodeids)
      (for ([key (in-list keys)])
        (let ([nodeid (hash-ref key=>nodeid key #f)])
          (when nodeid (add-nodeids! (list nodeid)))))
      (define sorted-nodeids (sort (hash-keys seen-nodeids) <))
      (values (map (lambda (nodeid) (hash-ref nodeid=>node nodeid)) sorted-nodeids)
              updated-locs))
    ))

;; ============================================================
