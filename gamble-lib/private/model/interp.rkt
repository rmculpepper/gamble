#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/undefined
         "../base.rkt"
         "addr.rkt"
         "ast.rkt")
(provide (all-defined-out))

;; TODO:
;; - better primop handling
;; - coalesce copies
;; - support `set!` ?
;; - support `mem`
;; - support `run-model`

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

;; MCtx = (model/ast ... AST Vector (Vectorof Identifier) Nat)
;; LEnv = (ImmHash LVar Location)

;; lenv-lookup : LEnv Var -> Location
(define (lenv-lookup lenv var)
  (hash-ref lenv var))

;; lenv-lookups : LEnv (Listof Var) -> (Listof Location)
(define (lenv-lookups lenv vars)
  (map (lambda (var) (hash-ref lenv var)) vars))

;; ============================================================

;; MultiValueMode is one of
;; - Nat  -- expect given number of values
;; - #f   -- any number of values, discarded

;; ============================================================
;; Results

;; Result is one of
;; - (result:location Location)
;; - (result:value Any)         -- constant given branch choices
(struct result:location (location) #:prefab)
(struct result:value (value) #:prefab)
(struct result:value+id result:value (id) #:prefab)

;; ============================================================
;; Node traces

;; NodeTrace = (MutHash NodeID Node)
;; NodeID = Nat

;; A Node is one of
;; - (node:same-if Boolean Location)                -- enforce same branch
;; - (node:same String Any Location)                -- enforce same proc/closure/etc
;; - (node:store Location Result)                   -- single var binding
;; - (node:stores (Listof Location) Result)         -- multiple var binding
;; - (node:apply (Listof Location) (Listof Result)) -- create lambda env
;; - (node:apply-tail Location (Listof Result))     -- create lambda rest arg binding
;; - (node:apply-prim Addr Location Procedure Identifier/#f (Listof Result) MultiValueMode)
(struct node:same-if (branch testloc) #:prefab)
(struct node:same (kind val loc) #:prefab)
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
    [(node:same-if branch testloc)
     `(unless (eq? (quote ,branch) (and ,(loc-ref testloc) #t))
        (raise-structural-change "if branch"))]
    [(node:same kind val loc)
     `(unless (equal? (quote ,val) ,(loc-ref loc))
        (raise-structural-change (quote ,kind)))]
    [(node:store varloc (result:location rloc))
     (loc-set! varloc (loc-ref rloc))]
    [(node:stores varlocs (result:location rloc))
     `(define-values ,(map loc-ref varlocs) (apply values ,(loc-ref rloc)))]
    [(node:apply varlocs argrs)
     `(begin ,@(for/list ([varloc (in-list varlocs)]
                          [argr (in-list argrs)])
                 (loc-set! varloc (loc-ref (result:location-location argr)))))]
    [(node:apply-tail varloc argrs)
     (loc-set! varloc `(list ,@(map result->expr argrs)))]
    [(node:apply-prim addr loc proc funid argrs mv)
     (let ([proc-expr (or funid `(quote ,proc))]
           [arg-exprs (map result->expr argrs)])
       (define (wrap expr) `(with-put-ADDR (quote ,addr) ,expr)) ;; FIXME: conditional
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
    [(node:same-if branch testloc)
     (values (list testloc) null)]
    [(node:same kind val loc)
     (values (list loc) null)]
    [(node:store varloc (result:location rloc))
     (values (list rloc) (list varloc))]
    [(node:stores varlocs (result:location rloc))
     (values (list rloc) varlocs)]
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
;; Worklist of integers

;; FIXME: use heap?
(struct worklist ([ns #:mutable] h))

(define (new-worklist)
  (worklist null (make-hasheqv)))

(define (worklist-add! wl n)
  (define h (worklist-h wl))
  (unless (hash-ref h n #f)
    (hash-set! h n #t)
    (set-worklist-ns! wl (insert n (worklist-ns wl)))))

(define (insert x ns)
  (match ns
    [(cons n ns) #:when (> x n) (cons n (insert x ns))]
    [_ (cons x ns)]))

(define (worklist-remove-min! wl)
  (define ns (worklist-ns wl))
  (cond [(pair? ns)
         (define n (car ns))
         (hash-remove! (worklist-h wl) n)
         (set-worklist-ns! wl (cdr ns))
         n]
        [else #f]))

;; ============================================================

(define interpreter%
  (class object%
    (init-field ctx)
    (super-new)

    (define the-store (make-hasheqv))       ;; Location => Any
    (define the-store-deps (make-hasheqv))  ;; Location => (Listof NodeID)
    (define node-trace (make-hasheqv))      ;; NodeID => Node

    (define/public (show [expr? #t])
      (printf "Store:\n")
      (for ([loc (in-range 0 location-counter)])
        (when (hash-has-key? the-store loc)
          (printf "  ~s => ~e\n" loc (hash-ref the-store loc))))
      (printf "Node trace:\n")
      (for ([nodeid (in-range 0 nodeid-counter)])
        (when (hash-has-key? node-trace nodeid)
          (define node (hash-ref node-trace nodeid))
          (if expr?
              (printf "  ~s : ~v\n" nodeid (node->expr node))
              (printf "  ~s : ~e\n" nodeid node)))))

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

    ;; lenv-add : LEnv (Listof Var) -> (values LEnv (Listof Location))
    (define (lenv-add lenv vars)
      (for/fold ([lenv lenv]) ([var (in-list vars)])
        (define loc (next-location))
        (hash-set lenv var loc)))

    ;; ----------------------------------------
    ;; Node trace, nodes

    (define nodeid-counter 0)
    (define/private (next-nodeid)
      (begin0 nodeid-counter (set! nodeid-counter (add1 nodeid-counter))))

    ;; add-node! : Node -> Void
    ;; Add node to node trace, update location dependencies.
    ;; Nodes must be added in execution order.
    (define/private (add-node! node)
      (define nodeid (next-nodeid))
      (define-values (readlocs writelocs) (node-locations node))
      (hash-set! node-trace nodeid node)
      (for ([readloc (in-list readlocs)])
        (hash-update! the-store-deps readloc (lambda (v) (cons nodeid v)) null)))

    ;; do! : Node -> Void
    ;; Perform node effect and register node in node trace (if needed).
    ;; (Eg, assignments to constants do not need to be repeated.)
    (define/private (do! node)
      ;; (eprintf "do! ~e\n" node)
      (define (add-and-exec! [node node])
        (begin (add-node! node) (exec-node! node)))
      (match node
        [(node:same-if branch testloc) (add-and-exec!)]
        [(node:same kind fun loc) (add-and-exec!)]
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
         (match labelr
           [(result:location lloc)
            (add-node! (node:same "sample label" (fetch lloc) lloc))]
           [(result:value _) (void)])
         (add-and-exec!)]
        [(node:dscore argr) (add-and-exec!)]
        [(node:lscore argr) (add-and-exec!)]
        [(node:observe distr valr) (add-and-exec!)]
        [(node:fail argr) (add-and-exec!)]
        [(node:mem loc argr) (add-and-exec!)]
        ))

    ;; exec-node! : Node -> Void
    ;; Perform node effect.
    (define/private (exec-node! node)
      ;; (eprintf "exec! ~e\n" node)
      (match node
        [(node:same-if branch testloc)
         (define new-branch (and (fetch testloc) #t))
         (unless (eq? new-branch branch)
           (error 'interpret "structural change (if branch)"))]
        [(node:same kind val loc)
         (define new-val (fetch loc))
         (unless (equal? new-val val)
           (error 'interpret "structural change (~a)" kind))]
        [(node:store varloc (result:location rloc))
         (store! varloc (fetch rloc))]
        [(node:stores varlocs (result:location rloc))
         (for ([varloc (in-list varlocs)]
               [val (in-list (fetch rloc))])
           (store! varloc val))]
        [(node:apply varlocs argrs)
         (for ([varloc (in-list varlocs)]
               [argr (in-list argrs)])
           (store! varloc (result->value argr)))]
        [(node:apply-tail varloc argrs)
         (store! varloc (results->values argrs))]
        [(node:apply-prim addr loc proc funid argrs mv)
         (case mv
           [(1) (store! loc (with-put-ADDR addr (apply proc (results->values argrs))))]
           [(#f) (begin0 (void) (with-put-ADDR addr (apply proc (results->values argrs))))]
           [else (call-with-values
                  (lambda () (with-put-ADDR addr (apply proc (results->values argrs))))
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

    ;; ----------------------------------------
    ;; Initial evaluation

    (define/public (eval-top m)
      (define ast (model/ast-ast m))
      (define result (init-eval ast m (hasheqv) 1 (current-init-addr)))
      (values result (result->value result)))

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
         (one (result:location (hash-ref lenv index)))]
        [(ast:ctxvar index)
         (one (result:value+id (vector-ref (model/ast-env mctx) index)
                               (vector-ref (model/ast-envids mctx) index)))]
        [(ast:lambda args restarg body)
         (one (result:value (closure (list ast) mctx lenv)))]
        [(ast:case-lambda lambdas)
         (one (result:value (closure lambdas mctx lenv)))]
        [(ast:if e1 e2 e3)
         (match (recur1 e1)
           [(result:location loc)
            (match (fetch loc)
              [(? values)
               (do! (node:same-if #t loc))
               (recur e2)]
              [#f
               (do! (node:same-if #f loc))
               (recur e3)])]
           [(result:value (? values))
            (recur e2)]
           [(result:value #f)
            (recur e3)])]
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
             (define lenv2 (lenv-add lenv* vars))
             (define varlocs (lenv-lookups lenv2 vars))
             (do! (node:stores varlocs result))
             lenv2))
         (recur body lenv*)]
        [(ast:letrec-values clauses body)
         (define lenv*
           (for/fold ([lenv* lenv]) ([clause (in-list clauses)])
             (match-define (ast:lv-clause vars rhs) clause)
             (define lenv2 (lenv-add lenv* vars))
             lenv2))
         (for ([clause (in-list clauses)])
           (match-define (ast:lv-clause vars rhs) clause)
           (define varlocs (lenv-lookups lenv* vars))
           (define result (recur rhs lenv* (length vars)))
           (do! (node:stores varlocs result)))
         (recur body lenv*)]
        [(ast:quote datum)
         (one (result:value datum))]
        ;[(ast:wcm e1 e2 e3) _]
        [(ast:app cs fun args)
         (define addr* (addr-add-call addr (+ (model/ast-csbase mctx) cs)))
         (init-apply (recur1 fun) (map recur1 args) mv addr*)]
        ;; ----------------------------------------
        [(ast:sample cs dist label)
         (define loc (next-location))
         (do! (node:sample loc addr (recur1 dist) (recur1 label)))
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
        ;[(ast:run-model arg) _]
        ))

    (define/private (init-apply funr argrs mv addr)
      ;; PRE: addr is already extended with call site
      (define-values (funval funid)
        (match funr
          [(result:location funloc)
           (define funval (fetch funloc))
           (do! (node:same 'application funval funloc))
           (values funval #f)]
          [(result:value+id funval funid) (values funval funid)]
          [(result:value funval) (values funval #f)]))
      (match funval
        [(closure lams mctx lenv)
         (define argc (length argrs))
         (define lenv+body
           (for/or ([lam (in-list lams)])
             (match lam
               [(ast:lambda vars #f body)
                #:when (= argc (length vars))
                (define lenv* (lenv-add lenv vars))
                (do! (node:apply (lenv-lookups lenv* vars) argrs))
                (cons lenv* body)]
               [(ast:lambda vars restvar body)
                #:when (>= argc (length vars))
                (define lenv* (lenv-add lenv (append vars (list restvar))))
                (do! (node:apply (lenv-lookups lenv* vars) (take argrs (length vars))))
                (do! (node:apply-tail (lenv-lookup lenv* restvar) (drop argrs (length vars))))
                (cons lenv* body)]
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

    ;; re-trace : (Listof Location) (Listof NodeID) Boolean
    ;;         -> (values (Listof Node) (Hash Location Symbol/#t))
    (define/private (re-trace init-locs init-nodeids make-names?)
      (define wl (new-worklist))
      (define update-locs (make-hasheqv))
      (define (make-name loc) (string->uninterned-symbol (format "a_~s" loc)))
      (define (add-nodeids! nodeids)
        (for ([nodeid (in-list nodeids)])
          (worklist-add! wl nodeid)))
      (define (add-locs! locs add?)
        (for ([loc (in-list locs)])
          (when add?
            (when (memv loc init-locs)
              (error 're-trace "initially-modified location is updated"))
            (unless (hash-has-key? update-locs loc)
              (hash-set! update-locs loc (if make-names? (make-name loc) #t))))
          (define loc-nodeids (hash-ref the-store-deps loc null))
          (for ([nodeid (in-list loc-nodeids)]) (worklist-add! wl nodeid))))
      (add-nodeids! init-nodeids)
      (add-locs! init-locs #f)
      ;; --------------------
      (let loop ([nodes null])
        (define nodeid (worklist-remove-min! wl))
        (cond [nodeid
               (define node (hash-ref node-trace nodeid))
               (define-values (readlocs writelocs) (node-locations node))
               (add-locs! writelocs #t)
               (loop (cons node nodes))]
              [else
               (values (reverse nodes) update-locs)])))

    (define/public (re-eval init-locs init-nodeids)
      (define-values (nodes update-locs) (re-trace init-locs init-nodeids #f))
      (for ([node (in-list nodes)])
        (exec-node! node)))

    (define/public (re-slice init-locs init-nodeids)
      (define-values (nodes update-locs) (re-trace init-locs init-nodeids #t))
      `(let-values ()
         ,@(for/list ([node (in-list nodes)])
             (node->expr node update-locs))
         ,(let ([loc+name-list (hash-map update-locs cons #t)])
            `(cons (quote ,(list->vector (map car loc+name-list)))
                   (vector ,@(map cdr loc+name-list))))))
    ))
