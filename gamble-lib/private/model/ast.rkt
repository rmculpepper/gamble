#lang racket/base
(provide (all-defined-out))

(struct ast:lvar (index) #:prefab)
(struct ast:ctxvar (index) #:prefab)
(struct ast:lambda (vars restvar body) #:prefab)
(struct ast:case-lambda (lambdas) #:prefab)
(struct ast:if (e1 e2 e3) #:prefab)
(struct ast:begin (es) #:prefab)
(struct ast:begin0 (e0 es) #:prefab)
(struct ast:let-values (clauses body) #:prefab)
(struct ast:letrec-values (clauses body) #:prefab)
(struct ast:lv-clause (vars rhs) #:prefab)
(struct ast:quote (datum) #:prefab)
(struct ast:wcm (e1 e2 e3) #:prefab)
(struct ast:app (cs fun args) #:prefab)

(struct ast:sample (cs dist label) #:prefab)
(struct ast:dscore (arg) #:prefab)
(struct ast:lscore (arg) #:prefab)
(struct ast:observe (dist value) #:prefab)
(struct ast:fail (arg) #:prefab)
(struct ast:mem (cs arg) #:prefab)
(struct ast:run-model (arg) #:prefab)

(define (wrap-begin es)
  (if (and (pair? es) (null? (cdr es))) (car es) (ast:begin es)))

(define (wrap-let-values clauses body)
  (if (null? clauses) body (ast:let-values clauses body)))


;; ============================================================
(require racket/match
         racket/list
         racket/undefined)

(struct closure (lambdas ctxenv lenv))

;; ============================================================

#|
;; CtxEnv = (Vectorof Any)
;; LEnv = (ImmHash Nat (Box Any))

(define (lenv-add lenv vars vals)
  (for/fold ([lenv lenv]) ([var (in-list vars)] [val (in-list vals)])
    (hash-set lenv var (box val))))

(define (lenv-set! lenv vars vals)
  (for ([var (in-list vars)] [val (in-list vals)])
    (set-box! (hash-ref lenv var) val)))

(define (aeval ast ctxenv lenv)
  (define (recur ast [lenv lenv]) (aeval ast ctxenv lenv))
  (match ast
    [(ast:lvar index) (unbox (hash-ref lenv index))]
    [(ast:ctxvar index) (vector-ref ctxenv index)]
    [(ast:lambda args restarg body)
     (closure (list ast) ctxenv lenv)]
    [(ast:case-lambda lambdas)
     (closure lambdas ctxenv lenv)]
    [(ast:if e1 e2 e3)
     (if (recur e1) (recur e2) (recur e3))]
    [(ast:begin es)
     (let loop ([es es])
       (match es
         [(list e) (recur e)]
         [(cons e es) (begin (recur e) (loop es))]))]
    [(ast:begin0 e es)
     (begin0 (recur e)
       (for ([e (in-list es)]) (recur e)))]
    [(ast:let-values clauses body)
     (define lenv*
       (for/fold ([lenv* lenv]) ([clause (in-list clauses)])
         (match-define (ast:lv-clause vars rhs) clause)
         (call-with-values
          (lambda () (recur rhs))
          (lambda vals
            ;; FIXME: check same length
            (lenv-add lenv* vars vals)))))
     (recur body lenv*)]
    [(ast:letrec-values clauses body)
     (define lenv*
       (for/fold ([lenv* lenv]) ([clause (in-list clauses)])
         (match-define (ast:lv-clause vars rhs) clause)
         (lenv-add lenv* vars (map (lambda (v) undefined) vars))))
     (for ([clause (in-list clauses)])
       (match-define (ast:lv-clause vars rhs) clause)
       (call-with-values (lambda () (recur rhs lenv*))
                         (lambda vals (lenv-set! lenv vars vals))))
     (recur body lenv*)]
    [(ast:quote datum)
     datum]
    [(ast:wcm e1 e2 e3)
     (with-continuation-mark (recur e1) (recur e2) (recur e3))]
    [(ast:app cs fun args)
     (aapply cs (recur fun) (map recur args))]
    ;[(ast:sample cs dist label) _]
    ;[(ast:dscore arg) _]
    ;[(ast:lscore arg) _]
    ;[(ast:observe dist value) _]
    ;[(ast:fail arg) _]
    ;[(ast:mem cs arg) _]
    ;[(ast:run-model arg) _]
    ))

(define (aapply cs fun args)
  (match fun
    [(closure lams ctxenv lenv)
     (define argc (length args))
     (define lenv+body
       (for/or ([lam (in-list lams)])
         (match lam
           [(ast:lambda vars #f body)
            #:when (= argc (length vars))
            (define lenv* (lenv-add lenv vars args))
            (cons lenv* body)]
           [(ast:lambda vars restvar body)
            #:when (>= argc (length vars))
            (define lenv* (lenv-add lenv vars (take args (length vars))))
            (define lenv** (lenv-add lenv* (list restvar) (drop args (length vars))))
            (cons lenv** body)]
           [_ #f])))
     (match lenv+body
       [(cons lenv* body)
        (aeval body ctxenv lenv*)]
       [#f (error 'interpreter-apply "arity mismatch\n  procedure: ~e\n  arguments: ~e"
                  fun args)])]
    [(? procedure? proc) (apply proc args)]
    ))
|#

;; ============================================================

;; CtxEnv = (Vectorof Any)
;; LEnv = (ImmHash Nat Nat)

;; Store = (MutHash Nat Any)
;; NodeTrace = (MutHash Nat Node)

(struct node:if (branch testloc) #:prefab)
(struct node:operator (fun loc) #:prefab)
(struct node:store (varloc result) #:prefab)
(struct node:store-values (varlocs result) #:prefab)
(struct node:apply (varlocs argrs) #:prefab)
(struct node:apply-tail (varloc argrs) #:prefab)
(struct node:apply-prim (cs loc proc argrs mv) #:prefab)

(define the-store (make-hasheqv))       ;; Location => Any
(define the-record (make-hasheqv))      ;; NodeID => Node
(define the-store-deps (make-hasheqv))  ;; Location => (Listof NodeID)

(define location-counter 0)
(define (next-location)
  (begin0 location-counter (set! location-counter (add1 location-counter))))

(define (store! loc val) (hash-set! the-store loc val))
(define (fetch loc) (hash-ref the-store loc))

(define (get-result r)
  (match r
    [(result:location loc) (fetch loc)]
    [(result:value val) val]))

(define nodeid-counter 0)
(define (next-nodeid)
  (begin0 nodeid-counter (set! nodeid-counter (add1 nodeid-counter))))

(define (do! node)
  (eprintf "do! ~e\n" node)
  ;; enter into trace, and execute effect
  (define nodeid (next-nodeid))
  (define (add!) (add-node! nodeid node))
  (match node
    [(node:if branch testloc)
     (begin (add!) (void))]
    [(node:operator fun loc)
     (begin (add!) (void))]
    [(node:store varloc result)
     (match result
       [(result:location rloc)
        (begin (add!) (store! varloc (fetch rloc)))]
       [(result:value val) (store! varloc val)])]
    [(node:store-values (list varloc) result)
     (do! (node:store varloc result))]
    [(node:store-values varlocs (result:location rloc))
     (add!)
     (define vals (fetch rloc))
     (for ([varloc (in-list varlocs)] [val (in-list vals)])
       (store! varloc val))]
    [(node:apply varlocs argrs)
     (for ([varloc (in-list varlocs)]
           [argr (in-list argrs)]
           #:when (result:value? argr))
       (store! varloc (result:value-value argr)))
     (define v+a-list
       (for/list ([varloc (in-list varlocs)]
                  [argr (in-list argrs)]
                  #:when (result:location? argr))
         (store! varloc (fetch (result:location-location argr)))
         (cons varloc argr)))
     (when (pair? v+a-list)
       (add-node! nodeid (node:apply (map car v+a-list) #f (map cdr v+a-list))))]
    [(node:apply-tail varloc argrs)
     (cond [(andmap result:value? argrs)
            (store! varloc (map result:value-value argrs))]
           [else
            (add!)
            (store! varloc (map get-result argrs))])]
    [(node:apply-prim cs loc proc argrs mv)
     (add!)
     (case mv
       [(1) (store! loc (apply proc (map get-result argrs)))]
       [(#f) (begin0 (void) (apply proc (map get-result argrs)))]
       [else (call-with-values
              (lambda () (apply proc (map get-result argrs)))
              (lambda vals (store! loc vals)))])]
    ))

(define (exec-node! node)
  (eprintf "exec! ~e\n" node)
  (match node
    [(node:if branch testloc)
     (define new-branch (and (fetch testloc) #t))
     (unless (eq? new-branch branch)
       (error 'interpret "branch changed"))]
    [(node:operator fun loc)
     (define new-fun (fetch loc))
     (unless (equal? new-fun fun)
       (error 'interpret "application operator changed"))]
    [(node:store varloc (result:location rloc))
     (store! varloc (fetch rloc))]
    [(node:store-values varlocs (result:location rloc))
     (for ([varloc (in-list varlocs)]
           [val (in-list (fetch rloc))])
       (store! varloc val))]
    [(node:apply varlocs argrs)
     (for ([varloc (in-list varlocs)]
           [argr (in-list argrs)])
       (store! varloc (fetch (result:location-location argr))))]
    [(node:apply-tail varloc argrs)
     (store! varloc (map get-result argrs))]
    [(node:apply-prim cs loc proc argrs mv)
     (case mv
       [(1) (store! loc (apply proc (map get-result argrs)))]
       [(#f) (begin0 (void) (apply proc (map get-result argrs)))]
       [else (call-with-values
              (lambda () (apply proc (map get-result argrs)))
              (lambda vals (store! loc vals)))])]
    ))

(define (node->expr node loc=>name)
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
    [(node:if branch testloc)
     `(unless (eq? (quote ,branch) (and ,(loc-ref testloc) #t))
        (error 'interpret "branch changed"))]
    [(node:operator fun loc)
     `(unless (equal? (quote ,fun) ,(loc-ref loc))
        (error 'interpret "application operator changed"))]
    [(node:store varloc (result:location rloc))
     (loc-set! varloc (loc-ref rloc))]
    [(node:store-values varlocs (result:location rloc))
     `(define-values ,(map loc-ref varlocs) (apply values ,(loc-ref rloc)))]
    [(node:apply varlocs argrs)
     `(begin ,@(for/list ([varloc (in-list varlocs)]
                          [argr (in-list argrs)])
                 (loc-set! varloc (loc-ref (result:location-location argr)))))]
    [(node:apply-tail varloc argrs)
     (loc-set! varloc `(list ,@(map result->expr argrs)))]
    [(node:apply-prim cs loc proc argrs mv)
     (case mv
       [(1) (loc-set! loc `(,proc ,@(map result->expr argrs)))]
       [(#f) `(,proc ,@(map result->expr argrs))]
       [else (loc-set! loc `(call-with-values
                             (lambda () (,proc ,@(map result->expr argrs)))
                             list))])]
    ))

;; node-locations : Node -> (values (Listof Location) (Listof Location))
;; Returns reads-locations and writes-locations.
(define (node-locations node)
  (match node
    [(node:if branch testloc)
     (values (list testloc) null)]
    [(node:operator fun loc)
     (values (list loc) null)]
    [(node:store varloc (result:location rloc))
     (values (list rloc) (list varloc))]
    [(node:store-values varlocs (result:location rloc))
     (values (list rloc) varlocs)]
    [(node:apply varlocs argrs)
     (define v+a-list
       (for/list ([varloc (in-list varlocs)]
                  [argr (in-list argrs)]
                  #:when (result:location? argr))
         (cons varloc (result:location-location argr))))
     (values (map cdr v+a-list) (map car v+a-list))]
    [(node:apply varloc argrs)
     (values (for/list ([argr (in-list argrs)] #:when (result:location? argr))
               (result:location-location argr))
             (list varloc))]
    [(node:apply-prim cs loc proc argrs mv)
     (values (for/list ([argr (in-list argrs)] #:when (result:location? argr))
               (result:location-location argr))
             (list loc))]
    ))

(define (add-node! nodeid node)
  (define-values (readlocs writelocs) (node-locations node))
  (hash-set! the-record nodeid node)
  (for ([readloc (in-list readlocs)])
    (hash-update! the-store-deps readloc (lambda (v) (cons nodeid v)) null)))

;; lenv-add : LEnv (Listof Var) -> (values LEnv (Listof Location))
(define (lenv-add lenv vars)
  (for/fold ([lenv lenv]) ([var (in-list vars)])
    (define loc (next-location))
    (hash-set lenv var loc)))

;; lenv-lookup : LEnv Var -> Location
(define (lenv-lookup lenv var)
  (hash-ref lenv var))

;; lenv-lookups : LEnv (Listof Var) -> (Listof Location)
(define (lenv-lookups lenv vars)
  (map (lambda (var) (hash-ref lenv var)) vars))

;; Result is one of
;; - (result:location Location)
;; - (result:value Any)         -- constant given branch choices
(struct result:location (location) #:prefab)
(struct result:value (value) #:prefab)

;; MultiValue = Nat | #f (discarded)

;; init-eval : AST CtxEnv LEnv MultiValue -> Result
(define (init-eval ast ctxenv lenv mv)
  (eprintf "eval(~s) ~e ~e\n" mv ast lenv)
  (define (one result)
    (unless (or (eqv? mv 1) (eqv? mv #f))
      (error 'interpreter "wrong result arity\n  expected: ~s\n  received: 1" mv))
    result)
  (define (recur ast [lenv lenv] [mv mv]) (init-eval ast ctxenv lenv mv))
  (define (recur1 ast [lenv lenv]) (init-eval ast ctxenv lenv 1))
  (match ast
    [(ast:lvar index)
     (one (result:location (hash-ref lenv index)))]
    [(ast:ctxvar index)
     (one (result:value (vector-ref ctxenv index)))]
    [(ast:lambda args restarg body)
     (one (result:value (closure (list ast) ctxenv lenv)))]
    [(ast:case-lambda lambdas)
     (one (result:value (closure lambdas ctxenv lenv)))]
    [(ast:if e1 e2 e3)
     (match (recur1 e1)
       [(result:location loc)
        (match (fetch loc)
          [(? values)
           (do! (node:if #t loc))
           (recur e2)]
          [#f
           (do! (node:if #f loc))
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
         (do! (node:store-values varlocs result))
         lenv2))
     (recur body lenv*)]
    [(ast:letrec-values clauses body)
     (define lenv*
       (for/fold ([lenv* lenv]) ([clause (in-list clauses)])
         (match-define (ast:lv-clause vars rhs) clause)
         (define lenv2 (lenv-add lenv* vars))
         #;(define varlocs (lenv-lookups lenv2 vars))
         #;(do! (node:clear-values varlocs))
         lenv2))
     (for ([clause (in-list clauses)])
       (match-define (ast:lv-clause vars rhs) clause)
       (define varlocs (lenv-lookups lenv* vars))
       (define result (recur rhs lenv* (length vars)))
       (do! (node:store-values varlocs result)))
     (recur body lenv*)]
    [(ast:quote datum)
     (one (result:value datum))]
    ;[(ast:wcm e1 e2 e3) _]
    [(ast:app cs fun args)
     (rapply cs (recur1 fun) (map recur1 args) mv)]
    ;[(ast:sample cs dist label) _]
    ;[(ast:dscore arg) _]
    ;[(ast:lscore arg) _]
    ;[(ast:observe dist value) _]
    ;[(ast:fail arg) _]
    ;[(ast:mem cs arg) _]
    ;[(ast:run-model arg) _]
    ))

(define (rapply cs funr argrs mv)
  (define funval
    (match funr
      [(result:location funloc)
       (define funval (fetch funloc))
       (do! (node:operator funval funloc))
       funval]
      [(result:value funval) funval]))
  (match funval
    [(closure lams ctxenv lenv)
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
        (init-eval body ctxenv lenv* mv)]
       [#f (error 'interpreter-apply "arity mismatch\n  procedure: ~e\n  arguments: ~e"
                  funval argrs)])]
    [(? procedure? proc)
     (define loc (next-location))
     (do! (node:apply-prim cs loc proc argrs mv))
     (result:location loc)]
    ))

;; ============================================================

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

;; ----------------------------------------

;; re-trace : (Listof Location) (Listof NodeID) Boolean
;;         -> (values (Listof Node) (Hash Location Symbol/#t))
(define (re-trace init-locs init-nodeids make-names?)
  (define wl (new-worklist))
  (define update-locs (make-hasheqv))
  (define (make-name loc) (string->uninterned-symbol (format "loc~s" loc)))
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
           (define node (hash-ref the-record nodeid))
           (define-values (readlocs writelocs) (node-locations node))
           (add-locs! writelocs #t)
           (loop (cons node nodes))]
          [else
           (values (reverse nodes) update-locs)])))

(define (re-eval init-locs init-nodeids)
  (define-values (nodes update-locs) (re-trace init-locs init-nodeids #f))
  (for ([node (in-list nodes)])
    (exec-node! node)))

(define (re-slice init-locs init-nodeids)
  (define-values (nodes update-locs) (re-trace init-locs init-nodeids #t))
  `(let-values ()
     ,@(for/list ([node (in-list nodes)])
         (node->expr node update-locs))
     (lambda ()
       ,@(for/list ([(loc name) (in-hash update-locs)])
           `(store! (quote ,loc) ,name)))))

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
