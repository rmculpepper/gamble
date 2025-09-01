#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/undefined
         "../base.rkt"
         (only-in "../dist.rkt" dist-pdf)
         (only-in "../util/density.rkt" density->real)
         "addr.rkt"
         "ast.rkt")
(provide (all-defined-out))

;; TODO:
;; - coalesce copies
;; - support `set!` ?
;; - support `mem`

;; IDEA: track `box` contents by location
;;   (define vs (for/list ([i 10]) (box (sample (uniform-dist 0 1)))))
;;   (unbox (list-ref vs 4))
;; If one element changes, no need to update entire list.
;; But `set-box!` not allowed! (Even if tracked, not safe to time travel
;; while sharing one mutable location! Same problem with `set!`.)

;; Another problem: can't use a single actual Racket box in multiple threads
;; simultaneously. Maybe better have new `cell`, `cell-ref`?
;; Or maybe don't mutate box, just store #<opaque> in it, track identity.

;;   (cell v) = (let ([x v]) (lambda () x))
;;   (cell-ref c) = (c)

;; ============================================================

(struct closure (lambdas mctx lenv))

;; ============================================================
;; Environments

;; This code assumes no local variables are mutated (enforced by `parse-ast`)
;; and no variables in context are mutated (not enforced).

;; MCtx = (model/ast ... AST Vector (Vectorof Identifier) Nat)
;; LEnv = (ImmHash LVar (U Result (Boxof Result/#f)))

;; lenv-lookup : LEnv LVar -> Result
(define (lenv-lookup lenv var)
  (define (fail) (error 'interpret "reference to uninitialized letrec-bound variable"))
  (define r (hash-ref lenv var))
  (if (box? r) (or (unbox r) (fail)) r))

;; lenv-lookups : LEnv (Listof LVar) -> (Listof Result)
(define (lenv-lookups lenv vars)
  (map (lambda (var) (hash-ref lenv var)) vars))

;; lenv-location : LEnv LVar -> Location
(define (lenv-location lenv vars)
  (result:location-location (lenv-lookup lenv vars)))

;; lenv-locations : LEnv (Listof LVar) -> (Listof Location)
(define (lenv-locations lenv vars)
  (map result:location-location (lenv-lookups lenv vars)))

;; lenv-bind : LEnv (Listof LVar) (Listof Result) -> LEnv
(define (lenv-bind lenv vars results)
  (for/fold ([lenv lenv]) ([var (in-list vars)] [result (in-list results)])
    (hash-set lenv var result)))

;; lenv-bind-box : LEnv LVar -> LEnv
(define (lenv-bind-box lenv var)
  (hash-set lenv var (box #f)))

;; lenv-update-box : LEnv LVar Result -> Void
(define (lenv-update-box lenv var result)
  (set-box! (hash-ref lenv var) result))

;; ============================================================
;; Result

;; A Result is one of
;; - (result:location Location)
;; - (result:value Any)         -- constant given branch choices
(struct result:location (location) #:prefab)
(struct result:value (value) #:prefab)
(struct result:value+id result:value (id) #:prefab)

;; ============================================================

;; MultiValueMode is one of
;; - Nat  -- expect given number of values
;; - #f   -- any number of values, discarded

;; ============================================================
;; Node traces

;; NodeTrace = (MutHash NodeID Node)
;; NodeID = Nat

;; A Node is one of
;; - (node:same-if Boolean result)                  -- enforce same branch
;; - (node:same String Any Result)                  -- enforce same proc/closure/etc
;; - (node:store Location Result)                   -- single var binding
;; - (node:stores (Listof Location) Result)         -- multiple var binding
;; - (node:apply (Listof Location) (Listof Result)) -- create lambda env
;; - (node:apply-tail Location (Listof Result))     -- create lambda rest arg binding
;; - (node:apply-prim Addr/#f Location Procedure Identifier/#f (Listof Result) MultiValueMode)
(struct node:same-if (branch result) #:prefab)
(struct node:same (kind val result) #:prefab)
(struct node:store (varloc result) #:prefab)
(struct node:stores (varlocs result) #:prefab)
(struct node:apply (varlocs argrs) #:prefab)
(struct node:apply-tail (varloc argrs) #:prefab)
(struct node:apply-prim (addr loc proc funid argrs mv) #:prefab)
(struct node:sample (loc addr distr labelr) #:prefab)
(struct node:dscore (argr) #:prefab)
(struct node:lscore (argr) #:prefab)
(struct node:observe (distr valr) #:prefab)
(struct node:fail (argr) #:prefab)
(struct node:mem (loc argr) #:prefab)

;; node->expr : Node (Hash Location Symbol) -> Expr
(define (node->expr node [loc=>name (hasheqv)])
  (define (loc-ref loc)
    (cond [(hash-ref loc=>name loc #f) => values]
          [else `(fetch (quote ,loc))]))
  (define (loc-set! loc rhs)
    (cond [(hash-ref loc=>name loc #f) => (lambda (name) `(define ,name ,rhs))]
          [else `(store! (quote ,loc) ,rhs)]))
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
    [(node:store varloc result)
     (loc-set! varloc (result->expr result))]
    [(node:stores varlocs result)
     `(define-values ,(map loc-ref varlocs) (apply values ,(result->expr result)))]
    [(node:apply varlocs argrs)
     `(begin ,@(for/list ([varloc (in-list varlocs)]
                          [argr (in-list argrs)])
                 (loc-set! varloc (result->expr argr))))]
    [(node:apply-tail varloc argrs)
     (loc-set! varloc `(list ,@(map result->expr argrs)))]
    [(node:apply-prim addr loc proc funid argrs mv)
     (let ([proc-expr (or funid `(quote ,proc))]
           [arg-exprs (map result->expr argrs)])
       (define (wrap expr)
         (if addr `(with-put-ADDR (quote ,addr) ,expr) expr))
       (case mv
         [(1) (loc-set! loc (wrap `(#%plain-app ,proc-expr ,@arg-exprs)))]
         [(#f) (wrap `(#%plain-app ,proc-expr ,@arg-exprs))]
         [else (loc-set! loc `(call-with-values
                               (lambda () ,(wrap `(#%plain-app ,proc-expr ,@arg-exprs)))
                               list))]))]
    [(node:sample loc addr distr labelr)
     (define label-expr
       (match labelr
         [(result:value #f) `(auto-label (quote ,addr))]
         [(result:value (? values v)) `(quote ,v)]
         [_ `(or ,(result->expr labelr) (auto-label (quote ,addr)))]))
     (loc-set! loc `(ctx-sample ,(result->expr distr) ,label-expr))]
    [(node:dscore argr)
     `(ctx-dscore ,(result->expr argr))]
    [(node:lscore argr)
     `(ctx-lscore ,(result->expr argr))]
    [(node:observe distr valr)
     `(ctx-observe ,(result->expr distr) ,(result->expr valr))]
    [(node:fail argr)
     `(ctx-fail ,(result->expr argr))]
    ;[(node:mem loc argr) _]
    ))

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
    [(node:store varloc result)
     (values (get-locs (list result)) (list varloc))]
    [(node:stores varlocs result)
     (values (get-locs (list result)) varlocs)]
    [(node:apply varlocs argrs)
     (define v+a-list
       (for/list ([varloc (in-list varlocs)]
                  [argr (in-list argrs)]
                  #:when (result:location? argr))
         (cons varloc (result:location-location argr))))
     (values (map cdr v+a-list) (map car v+a-list))]
    [(node:apply varloc argrs)
     (values (get-locs argrs) (list varloc))]
    [(node:apply-prim addr loc proc funid argrs mv)
     (values (get-locs argrs) (list loc))]
    [(node:sample loc addr distr labelr)
     (values (get-locs (list distr labelr)) (list loc))]
    [(node:dscore argr) (values (get-locs (list argr)) null)]
    [(node:lscore argr) (values (get-locs (list argr)) null)]
    [(node:observe distr valr) (values (get-locs (list distr valr)) null)]
    [(node:fail argr) (values (get-locs argr) null)]
    [(node:mem loc argr) (values (get-locs argr) null)]
    ))

;; ============================================================

(define interpreter%
  (class object%
    (init-field ctx)
    (super-new)

    (define the-store (make-hasheqv))       ;; Location => Any
    (define loc=>nodeids (make-hasheqv))    ;; Location => (Listof NodeID)
    (define nodeid=>node (make-hasheqv))    ;; NodeID => Node
    (define label=>nodeid (make-hash))      ;; Label => NodeID
    (define final-result #f)                ;; Result, mutated

    (define/public (show [expr? #t])
      (printf "Store:\n")
      (for ([loc (in-range 0 location-counter)])
        (when (hash-has-key? the-store loc)
          (printf "  ~s => ~e\n" loc (hash-ref the-store loc))))
      (printf "Node trace:\n")
      (for ([nodeid (in-range 0 nodeid-counter)])
        (when (hash-has-key? nodeid=>node nodeid)
          (define node (hash-ref nodeid=>node nodeid))
          (if expr?
              (printf "  ~s : ~v\n" nodeid (node->expr node))
              (printf "  ~s : ~e\n" nodeid node))))
      (printf "Label mapping:\n")
      (for ([(label nodeid) (in-hash label=>nodeid)])
        (printf "  ~s => ~s\n" label nodeid)))

    ;; ----------------------------------------
    ;; Store

    (define location-counter 0)
    (define/private (next-location)
      (begin0 location-counter (set! location-counter (add1 location-counter))))

    (define/public (store! loc val) (hash-set! the-store loc val))
    (define/public (fetch loc) (hash-ref the-store loc))

    ;; result->value : Result -> Any
    (define/private (result->value r)
      (match r
        [(result:location loc) (fetch loc)]
        [(result:value val) val]))
    (define/private (results->values rs)
      (for/list ([r (in-list rs)]) (result->value r)))

    ;; lenv-add : LEnv (Listof Var) -> LEnv
    (define (lenv-add lenv vars)
      (for/fold ([lenv lenv]) ([var (in-list vars)])
        (define loc (next-location))
        (hash-set lenv var (result:location loc))))

    ;; ----------------------------------------
    ;; Node trace, nodes

    (define nodeid-counter 0)
    (define/private (next-nodeid)
      (begin0 nodeid-counter (set! nodeid-counter (add1 nodeid-counter))))

    ;; add-node! : Node -> NodeID
    ;; Add node to node trace, update location dependencies.
    ;; Nodes must be added in execution order.
    (define/private (add-node! node)
      (define nodeid (next-nodeid))
      (define-values (readlocs writelocs) (node-locations node))
      (hash-set! nodeid=>node nodeid node)
      (for ([readloc (in-list readlocs)])
        (hash-update! loc=>nodeids readloc (lambda (v) (cons nodeid v)) null))
      nodeid)

    ;; do! : Node -> Void
    ;; Perform node effect and register node in node trace (if needed).
    ;; (Eg, assignments to constants do not need to be repeated.)
    (define/private (do! node)
      (define (add-and-exec! [node node])
        (begin0 (add-node! node) (exec-node! node)))
      (match node
        [(node:same-if branch result)
         (when (result:location? result) (add-and-exec!))]
        [(node:same kind fun result)
         (when (result:location? result) (add-and-exec!))]
        [(node:store varloc result)
         (match result
           [(result:location rloc) (add-and-exec!)]
           [(result:value val) (store! varloc val)])]
        [(node:stores (list varloc) result)
         (do! (node:store varloc result))]
        [(node:stores varlocs (result:location rloc))
         (add-and-exec!)]
        [(node:apply varlocs argrs)
         (for ([varloc (in-list varlocs)]
               [argr (in-list argrs)]
               #:when (result:value? argr))
           (store! varloc (result:value-value argr)))
         (define v+a-list
           (for/list ([varloc (in-list varlocs)]
                      [argr (in-list argrs)]
                      #:when (result:location? argr))
             (cons varloc argr)))
         (when (pair? v+a-list)
           (add-and-exec! (node:apply (map car v+a-list) (map cdr v+a-list))))]
        [(node:apply-tail varloc argrs)
         (cond [(andmap result:value? argrs)
                (store! varloc (map result:value-value argrs))]
               [else (add-and-exec!)])]
        [(node:apply-prim addr loc proc funid argrs mv) (add-and-exec!)]
        [(node:sample loc addr distr labelr)
         (define label (result->value labelr))
         (when (result:location? labelr)
           (add-node! (node:same "sample label" label labelr)))
         (define nodeid (add-node! node))
         (let ([label (or label (auto-label addr))])
           (hash-set! label=>nodeid label nodeid))
         (exec-node! node)]
        [(node:dscore argr) (add-and-exec!)]
        [(node:lscore argr) (add-and-exec!)]
        [(node:observe distr valr) (add-and-exec!)]
        [(node:fail argr) (add-and-exec!)]
        [(node:mem loc argr) (add-and-exec!)]
        ))

    ;; exec-node! : Node StochasticCtx -> Void
    ;; Perform node effect.
    (define/private (exec-node! node [ctx ctx])
      (match node
        [(node:same-if branch result)
         (define new-branch (and (result->value result) #t))
         (unless (eq? new-branch branch)
           (error 'interpret "structural change (if branch)"))]
        [(node:same kind val result)
         (define new-val (result->value result))
         (unless (equal? new-val val)
           (error 'interpret "structural change (~a)" kind))]
        [(node:store varloc result)
         (store! varloc (result->value result))]
        [(node:stores varlocs result)
         (for ([varloc (in-list varlocs)]
               [val (in-list (result->value result))])
           (store! varloc val))]
        [(node:apply varlocs argrs)
         (for ([varloc (in-list varlocs)]
               [argr (in-list argrs)])
           (store! varloc (result->value argr)))]
        [(node:apply-tail varloc argrs)
         (store! varloc (results->values argrs))]
        [(node:apply-prim addr loc proc funid argrs mv)
         (define (call) (apply proc (results->values argrs)))
         (define (call*) (if addr (with-put-ADDR addr (call)) (call)))
         (case mv
           [(1) (store! loc (with-put-ADDR addr (call*)))]
           [(#f) (begin0 (void) (with-put-ADDR addr (call*)))]
           [else (call-with-values
                  (lambda () (with-put-ADDR addr (call*)))
                  (lambda vals (store! loc vals)))])]
        [(node:sample loc addr distr labelr)
         (store! loc (send ctx sample (result->value distr)
                           (or (result->value labelr) (auto-label addr))))]
        [(node:dscore argr)
         (send ctx dscore (result->value argr))]
        [(node:lscore argr)
         (send ctx lscore (result->value argr))]
        [(node:observe distr valr)
         (send ctx observe (result->value distr) (result->value valr))]
        [(node:fail argr)
         (send ctx fail (result->value argr))]
        ;[(node:mem argr) _]
        ))

    ;; exec-stochastic-nodes! : (Listof Node) -> (Values Real Real)
    ;; Replay only sample/observe nodes to calculate priors and likelihoods of given slice.
    (define/private (exec-stochastic-nodes! nodes)
      (define sumlprs 0.0)
      (define sumlobs 0.0)
      (for ([node (in-list nodes)])
        (match node
          [(node:sample loc addr distr labelr)
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
    ;; Initial evaluation

    ;; eval-top : (Model X) -> X
    ;; Call once to complete initialization.
    (define/public (eval-top m)
      (define ast (model/ast-ast m))
      (set! final-result (init-eval ast m (hasheqv) 1 (current-init-addr)))
      (result->value final-result))

    ;; init-eval : AST MCtx LEnv MultiValue -> Result
    (define/public (init-eval ast mctx lenv mv addr)
      (define (one result)
        (unless (or (eqv? mv 1) (eqv? mv #f))
          (error 'interpreter "wrong result arity\n  expected: ~s\n  received: 1" mv))
        result)
      (define (recur ast [lenv lenv] [mv mv]) (init-eval ast mctx lenv mv addr))
      (define (recur1 ast [lenv lenv]) (init-eval ast mctx lenv 1 addr))
      (match ast
        [(ast:lvar index)
         (one (lenv-lookup lenv index))]
        [(ast:ctxvar index)
         (one (result:value+id (vector-ref (model/ast-env mctx) index)
                               (vector-ref (model/ast-envids mctx) index)))]
        [(ast:lambda args restarg body)
         (one (result:value (closure (list ast) mctx lenv)))]
        [(ast:case-lambda lambdas)
         (one (result:value (closure lambdas mctx lenv)))]
        [(ast:if e1 e2 e3)
         (define result (recur1 e1))
         (define branch (and (result->value result) #t))
         (do! (node:same-if branch result))
         (if branch
             (recur e2)
             (recur e3))]
        [(ast:begin es)
         (let loop ([es es])
           (match es
             [(list e) (recur e)]
             [(cons e es) (begin (recur e lenv #f) (loop es))]))]
        [(ast:begin0 e es)
         (begin0 (recur e)
           (for ([e (in-list es)]) (recur e lenv #f)))]
        [(ast:let-values clauses body)
         (define lenv*
           (for/fold ([lenv* lenv]) ([clause (in-list clauses)])
             (match-define (ast:lv-clause vars rhs) clause)
             (define result (recur rhs lenv (length vars)))
             (match vars
               [(list var)
                (lenv-bind lenv* (list var) (list result))]
               [vars
                (define lenv2 (lenv-add lenv* vars))
                (define varlocs (lenv-lookups lenv2 vars))
                (do! (node:stores varlocs result))
                lenv2])))
         (recur body lenv*)]
        [(ast:letrec-values clauses body)
         (define lenv*
           (for/fold ([lenv* lenv]) ([clause (in-list clauses)])
             (match-define (ast:lv-clause vars rhs) clause)
             (match vars
               [(list var)
                (lenv-bind-box lenv* var)]
               [else
                (lenv-add lenv* vars)])))
         (for ([clause (in-list clauses)])
           (match-define (ast:lv-clause vars rhs) clause)
           (match vars
             [(list var)
              (lenv-update-box lenv* var (recur1 rhs lenv*))]
             [else
              (define varlocs (lenv-locations lenv* vars))
              (define result (recur rhs lenv* (length vars)))
              (do! (node:stores varlocs result))]))
         (recur body lenv*)]
        [(ast:quote datum)
         (one (result:value datum))]
        ;[(ast:wcm e1 e2 e3) _]
        [(ast:app cs fun args)
         (define addr* (and cs (addr-add-call addr (+ (model/ast-csbase mctx) cs))))
         (init-apply (recur1 fun) (map recur1 args) mv addr*)]
        ;; ----------------------------------------
        [(ast:sample cs dist label)
         (define loc (next-location))
         (define addr* (and cs (addr-add-call addr (+ (model/ast-csbase mctx) cs))))
         (do! (node:sample loc addr* (recur1 dist) (recur1 label)))
         (one (result:location loc))]
        [(ast:dscore arg)
         (do! (node:dscore (recur1 arg)))
         (one (result:value (void)))]
        [(ast:lscore arg)
         (do! (node:lscore (recur1 arg)))
         (one (result:value (void)))]
        [(ast:observe dist value)
         (do! (node:observe (recur1 dist) (recur1 value)))
         (one (result:value (void)))]
        [(ast:fail arg)
         (do! (node:fail (recur1 arg)))
         (one (result:value (void)))]
        ;[(ast:mem cs arg) _]
        [(ast:run-model cs arg)
         (define result (recur1 arg))
         (do! (node:same "model" (result->value result) result))
         (define addr* (and cs (addr-add-call addr (+ (model/ast-csbase mctx) cs))))
         (match (result->value result)
           [(? model/ast? m)
            (define ast (model/ast-ast m))
            (init-eval ast m (hasheqv) mv addr*)])]
        ))

    (define/private (init-apply funr argrs mv addr)
      ;; PRE: addr is already extended with call site
      (define funval (result->value funr))
      (define funid (match funr [(result:value+id _ funid) funid] [_ #f]))
      (do! (node:same "application" funval funr))
      (match funval
        [(closure lams mctx lenv)
         (define argc (length argrs))
         (define lenv+body
           (for/or ([lam (in-list lams)])
             (match lam
               [(ast:lambda vars #f body)
                #:when (= argc (length vars))
                (define lenv* (lenv-bind lenv vars argrs))
                (cons lenv* body)]
               [(ast:lambda vars restvar body)
                #:when (>= argc (length vars))
                (define lenv1 (lenv-bind lenv vars (take argrs (length vars))))
                (define lenv2 (lenv-add lenv (list restvar)))
                (do! (node:apply-tail (lenv-location lenv2 restvar) (drop argrs (length vars))))
                (cons lenv2 body)]
               [_ #f])))
         (match lenv+body
           [(cons lenv* body)
            (init-eval body mctx lenv* mv addr)]
           [#f (error 'interpreter-apply "arity mismatch\n  procedure: ~e\n  arguments: ~e"
                      funval argrs)])]
        [(? procedure? proc)
         (define loc (next-location))
         ;; can't `eval` local var ref, so only use funid if bound at module-level
         (let ([funid (and funid (list? (identifier-binding funid)) funid)])
           (do! (node:apply-prim addr loc proc funid argrs mv)))
         (result:location loc)]
        ))

    ;; ----------------------------------------
    ;; Re-evaluation

    ;; get-slice-eval : (Listof Label) (Listof NodeID)
    ;;               -> (values (StochasticCtx -> (values Any StoreUpdate)) Real Real)
    (define/public (get-slice-eval #:labels [labels null]
                                   #:nodeids [nodeids null])
      (define-values (nodes updated-locs) (get-slice labels nodeids))
      (define-values (slice-lprs slice-lobs) (exec-stochastic-nodes! nodes))
      (values (get-slice-proc/interp nodes) slice-lprs slice-lobs))

    (define/private (get-slice-proc/interp nodes)
      (lambda (ctx)
        (for ([node (in-list nodes)])
          (exec-node! node ctx))
        (values (result->value final-result) '(#() . #()))))

    (define/private (get-slice-proc/eval nodes updated-locs)
      (expr->proc the-store (slice->expr nodes updated-locs)))

    ;; get-slice-expr : (Listof Label) (Listof NodeID) -> Expr
    (define/public (get-slice-expr labels [nodeids null])
      (define-values (nodes updated-locs) (get-slice labels nodeids))
      (slice->expr nodes updated-locs))

    ;; commit-store-update : StoreUpdate -> Void
    (define/public (commit-store-update commit)
      (match-define (cons locv valuev) commit)
      (for ([loc (in-vector locv)] [value (in-vector valuev)])
        (store! loc value)))

    ;; slice->expr : (Listof Node) (Hash Location Symbol) -> Expr
    (define/private (slice->expr nodes updated-locs)
      `(begin
         ,@(for/list ([node (in-list nodes)])
             (node->expr node updated-locs))
         (values ,(match final-result
                    [(result:value val) `(quote ,val)]
                    [(result:location loc)
                     (or (hash-ref updated-locs loc #f)
                         `(fetch (quote ,loc)))])
                 ,(let ([loc+name-list (hash-map updated-locs cons #t)])
                    `(cons (quote ,(list->vector (map car loc+name-list)))
                           (vector ,@(map cdr loc+name-list)))))))

    ;; get-slice : (Listof Label) (Listof NodeID) Boolean
    ;;           -> (values (Listof NodeID) (Hash Location (U Symbol #t)))
    (define/private (get-slice labels nodeids [make-names? #t])
      (define seen-nodeids (make-hasheqv))
      (define updated-locs (make-hasheqv))
      (define (make-name loc) (string->uninterned-symbol (format "a_~s" loc)))
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
            (hash-set! updated-locs loc (if make-names? (make-name loc) #t))
            (add-nodeids! (hash-ref loc=>nodeids loc null)))))
      (add-nodeids! nodeids)
      (for ([label (in-list labels)])
        (let ([nodeid (hash-ref label=>nodeid label #f)])
          (when nodeid (add-nodeids! (list nodeid)))))
      (define sorted-nodeids (sort (hash-keys seen-nodeids) <))
      (values (map (lambda (nodeid) (hash-ref nodeid=>node nodeid)) sorted-nodeids)
              updated-locs))
    ))

;; ============================================================

;; expr->proc : Store Expr -> (StochasticCtx -> StoreUpdate)
;; StoreUpdate = (cons (Vectorof Location) (Vectorof Any))
(define (expr->proc store update-expr)
  (define outer-expr
    `(lambda (get-ctx-functions the-store)
       (define (fetch loc) (hash-ref the-store loc))
       (define-values (ctx-sample
                       ctx-dscore
                       ctx-lscore
                       ctx-observe
                       ctx-fail
                       ctx-mem
                       ctx-run-model)
         (get-ctx-functions))
       (let-values () ,update-expr)))
  (define outer-proc
    (parameterize ((current-namespace (namespace-anchor->namespace eval-anchor)))
      (eval outer-expr)))
  (define (slice-eval ctx)
    (define (get-ctx-functions) (send ctx get-functions))
    (outer-proc get-ctx-functions store))
  slice-eval)

(module eval-support racket/base
  (require "addr.rkt")
  (provide (all-defined-out))
  (define-namespace-anchor eval-anchor)
  (struct structural-change (kind))
  (define (raise-structural-change kind)
    (raise (structural-change kind))))
(require (submod "." eval-support))
