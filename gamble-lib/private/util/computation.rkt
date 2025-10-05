#lang racket/base
(require racket/math
         (for-syntax racket/base
                     racket/syntax))
(provide (all-defined-out))

;; Computation graphs with reverse-mode automatic differentiation.

(module ast racket/base
  (require (for-syntax racket/base)
           racket/match
           racket/list)
  (provide (all-defined-out))

  ;; A Computation is one of
  ;; - Symbol
  ;; - Flonum
  ;; - (comp:_ Comp ...)
  (struct comp:+ (a b) #:prefab)
  (struct comp:* (a b) #:prefab)
  (struct comp:- (a b) #:prefab)
  (struct comp:/ (a b) #:prefab)
  (struct comp:log (a) #:prefab)
  (struct comp:exp (a) #:prefab)
  (struct comp:expt (a n) #:prefab)
  (struct comp:gamma (a) #:prefab)
  (struct comp:log-gamma (a) #:prefab)
  (struct comp:digamma (a) #:prefab)
  (struct comp:if< (a b then else) #:prefab)
  (struct comp:unknown (cs) #:prefab) ;; unknown function of given

  ;; A comp:unknown always produces NAN result, but only pushes NAN to arguments,
  ;; so other parts of gradient are still defined.

  (define (c2+ a b) (if (and (real? a) (real? b)) (+ a b) (comp:+ a b)))
  (define (c2* a b) (if (and (real? a) (real? b)) (* a b) (comp:* a b)))
  (define (c2- a b) (if (and (real? a) (real? b)) (- a b) (comp:- a b)))
  (define (c2/ a b) (if (and (real? a) (real? b)) (/ a b) (comp:/ a b)))

  (define (c+ c . cs) (for/fold ([c c]) ([e (in-list cs)]) (c2+ c e)))
  (define (c- c . cs) (for/fold ([c c]) ([e (in-list cs)]) (c2- c e)))
  (define (c* c . cs) (for/fold ([c c]) ([e (in-list cs)]) (c2* c e)))
  (define (c/ c . cs) (for/fold ([c c]) ([e (in-list cs)]) (c2/ c e)))
  (define (c-exp a) (if (real? a) (exp a) (comp:exp a)))
  (define (c-expt a b) (comp:expt a b))
  (define (c-neg a) (c* -1.0 a))
  (define (c-recip a) (c/ 1.0 a))
  (define (c-sqr a) (c* a a))
  (define (c-sqrt a) (c-expt a 0.5))

  (define (c-if< a b then else) (comp:if< a b then else))
  (define (c-if> a b then else) (comp:if< b a then else))
  (define (c-if<= a b then else) (c-if> a b else then))
  (define (c-if>= a b then else) (c-if< a b else then))

  (define-syntax (c-cond stx)
    (syntax-case stx (< <= > >= else)
      [(_ [else result])
       #'result]
      [(_ [(< a b) then] . clauses)
       #'(c-if< a b then (c-cond . clauses))]
      [(_ [(> a b) then] . clauses)
       #'(c-if> a b then (c-cond . clauses))]
      [(_ [(<= a b) then] . clauses)
       #'(c-if<= a b then (c-cond . clauses))]
      [(_ [(>= a b) then] . clauses)
       #'(c-if>= a b then (c-cond . clauses))]))

  (define (c-log c)
    (match c
      [(? real? x) (log x)]
      [(? symbol? x) (comp:log x)]
      [(comp:* a b) (comp:+ (c-log a) (c-log b))]
      [(comp:/ a b) (comp:- (c-log a) (c-log b))]
      [(comp:exp a) a]
      [(comp:expt a n) (c* n (c-log a))]
      [(comp:gamma a) (comp:log-gamma a)]
      [(comp:if< a b then else) (comp:if< a b (c-log then) (c-log else))]
      [(comp:unknown cs) (comp:unknown cs)]
      [_ (comp:log c)]))

  ;; comp->po : Comp -> (Listof Comp)
  ;; Return all subcomputations in post order.
  (define (comp->po c)
    (define seen (make-hash))
    (define rpolist null)
    (define (loop c)
      (match c
        [(? symbol?) (void)]
        [(? real?) (void)]
        [_ (loop* c)]))
    (define (loop* c)
      (unless (hash-ref seen c #f)
        (match c
          [(comp:+ a b) (loop a) (loop b)]
          [(comp:* a b) (loop a) (loop b)]
          [(comp:- a b) (loop a) (loop b)]
          [(comp:/ a b) (loop a) (loop b)]
          [(comp:log a) (loop a)]
          [(comp:exp a) (loop a)]
          [(comp:expt a n) (loop a) (loop n)]
          [(comp:gamma a) (loop a)]
          [(comp:log-gamma a) (loop a)]
          [(comp:digamma a) (loop a)]
          [(comp:if< a b then else) (for-each loop (list a b then else))]
          [(comp:unknown cs) (for-each loop cs)])
        (hash-set! seen c #t)
        (set! rpolist (cons c rpolist))))
    (loop c)
    (reverse rpolist))

  (define (comp-rad c vars)
    (define comp=>name (make-hash))
    (define comp=>dname (make-hash))
    (for ([var (in-list vars)])
      (hash-set! comp=>name var (string->symbol (format "var_~a" var)))
      (hash-set! comp=>dname var (string->symbol (format "dvar_~a" var))))
    (define po (comp->po c))
    (for ([c (in-list po)] [index (in-naturals 1)])
      (define name (string->symbol (format "tmp_~a" index)))
      (define dname (string->symbol (format "dtmp_~a" index)))
      (hash-set! comp=>name c name)
      (hash-set! comp=>dname c dname))
    (define (ref c) (if (real? c) c (hash-ref comp=>name c)))
    (define fwd-exprs
      (for/list ([c (in-list po)])
        (define expr
          (match c
            [(comp:+ a b) `(+ ,(ref a) ,(ref b))]
            [(comp:* a b) `(* ,(ref a) ,(ref b))]
            [(comp:- a b) `(- ,(ref a) ,(ref b))]
            [(comp:/ a b) `(/ ,(ref a) ,(ref b))]
            [(comp:log a) `(log ,(ref a))]
            [(comp:exp a) `(exp ,(ref a))]
            [(comp:expt a b) `(expt ,(ref a) ,(ref b))]
            [(comp:unknown cs) +nan.0]
            [(comp:gamma a) `(gamma ,(ref a))]
            [(comp:log-gamma a) `(log-gamma ,(ref a))]
            [(comp:digamma a) `(digamma ,(ref a))]
            [(comp:if< a b then else) `(if (< ,(ref a) ,(ref b)) ,(ref then) ,(ref else))]))
        `(define ,(ref c) ,expr)))
    (define dname=>exprs (make-hash))
    (define (rprop c expr)
      (define dc (if (real? c) #f (hash-ref comp=>dname c)))
      (when dc (hash-set! dname=>exprs dc (cons expr (hash-ref dname=>exprs dc null)))))
    (rprop (last po) '1.0)
    (define rev-comp-exprs
      (for/list ([c (in-list (reverse po))])
        (define dc (hash-ref comp=>dname c))
        (match c
          [(comp:+ a b)
           (rprop a dc)
           (rprop b dc)]
          [(comp:* a b)
           (rprop a `(* ,dc ,(ref b)))
           (rprop b `(* ,dc ,(ref a)))]
          [(comp:- a b)
           (rprop a dc)
           (rprop b `(- ,dc))]
          [(comp:/ a b)
           (rprop a `(/ ,dc ,(ref b)))
           (rprop b `(* ,dc -1.0 ,(ref a) (/ (sqr ,(ref b)))))]
          [(comp:log a)
           (rprop a `(/ ,dc ,(ref a)))]
          [(comp:exp a)
           (rprop a `(* ,dc ,(ref c)))]
          [(comp:expt a b)
           (rprop a `(* ,dc ,(ref b) (expt ,(ref a) (sub1 ,(ref b)))))
           (rprop b `(* ,dc ,(ref c) (log ,(ref a))))]
          [(comp:unknown cs)
           (for ([c (in-list cs)]) (rprop c +nan.0))]
          [(comp:gamma a)
           (rprop a `(* ,dc ,(ref c) (digamma ,(ref a))))]
          [(comp:log-gamma a)
           (rprop a `(* ,dc (digamma ,(ref a))))]
          [(comp:if< a b then else)
           (rprop then `(if (< ,(ref a) ,(ref b)) ,dc 0.0))
           (rprop else `(if (< ,(ref a) ,(ref b)) 0.0 ,dc))])
        `(define ,dc (+ ,@(hash-ref dname=>exprs dc null)))))
    (define rev-vars-exprs
      (for/list ([var (in-list vars)])
        (define dvar (hash-ref comp=>dname var))
        `(define ,dvar (+ ,@(hash-ref dname=>exprs dvar null)))))
    `(lambda ,(map (lambda (var) (hash-ref comp=>name var)) vars)
       ,@(filter values fwd-exprs)
       ,@(filter values rev-comp-exprs)
       ,@(filter values rev-vars-exprs)
       (values ,(hash-ref comp=>name c)
               (vector ,@(map (lambda (var) (hash-ref comp=>dname var)) vars))))))

;; ----------------------------------------

(module pdfs racket/base
  (require (submod ".." ast)
           racket/match
           racket/math)
  (provide (all-defined-out))

  (define (comp:choose n k) (comp:unknown (list n k)))
  (define (comp:factorial n) (comp:unknown (list n)))

  (define (beta-pdf x a b)
    (c/ (c* (c-expt x (c- a 1.0))
            (c-expt (c- 1.0 x) (c- b 1.0))
            (comp:gamma (c+ a b)))
        (c* (comp:gamma a)
            (comp:gamma b))))

  (define (cauchy-pdf x x0 gamma)
    (c-recip (c* pi gamma (c+ 1.0 (c-sqr (c/ (c- x x0) gamma))))))

  (define (exponential-pdf x mean)
    (let ([rate (c-recip mean)])
      (c* rate (c-exp (c* -1.0 rate x)))))

  (define (gamma-pdf x alpha theta)
    (c/ (c* (c-expt x (c- alpha 1.0))
            (c-exp (c-neg (c/ x theta))))
        (c* (comp:gamma alpha)
            (c-expt theta alpha))))

  (define (logistic-pdf x mu s)
    (define c (c-exp (c/ (c- mu x) s)))
    (c/ c s (c-sqr c)))

  (define (normal-pdf x mu s)
    (c/ (c-exp (c/ (c-neg (c-sqr (c- x mu)))
                   (c* 2.0 s s)))
        (c-sqrt (c* 2.0 pi s s))))

  (define (pareto-pdf x xm alpha)
    (c/ (c* alpha (c-expt xm alpha))
        (c-expt x (c+ 1.0 alpha))))

  (define (triangle-pdf x lo hi mode)
    (c-cond [(< x lo) 0.0]
            [(> x hi) 0.0]
            [(< x mode)
             (c/ (c* 2.0 (c- x lo))
                 (c* (c- hi lo) (c- mode lo)))]
            [(> x mode)
             (c/ (c* 2.0 (c- hi x))
                 (c* (c- hi lo) (c- hi mode)))]
            [else (c/ 2.0 (c- hi lo))]))

  (define (uniform-pdf x lo hi)
    (c-if< x lo 0.0 (c-if> x hi 0.0 (c-recip (c- hi lo)))))

  ;; ----------------------------------------
  ;; Assume degrees (nu) argument is constant.

  (define (student-t-pdf x nu xm scale)
    (c/ (c* (comp:unknown (list nu))
            (c-expt (c+ 1.0
                        (c/ (c-sqr (c- x xm))
                            nu
                            (c-sqr scale)))
                    (c/ (c+ nu 1.0) -2.0)))
        scale))

  ;; ----------------------------------------
  ;; Assume integer arguments are constant.
  ;; Assume values are actually valid for distribution.

  (define (bernoulli-pdf k p)
    (c-cond [(> k 0.5) p]
            [else (c- 1.0 p)]))

  (define (binomial-pdf k n p)
    (c* (comp:choose n k)
        (c-expt p k)
        (c-expt (c- 1.0 p) (c- n k))))

  (define (categorial-pdf k ps)
    (let loop ([i 1] [ps (vector->list ps)])
      (match ps
        [(list* pi ps)
         (c-if< k (+ i 0.5) pi (loop (add1 i) ps))]
        [(list)
         0.0])))

  (define (geometric-pdf p k)
    (c* (c-expt (c- 1.0 p) k) p))

  (define (negative-binomial-pdf k r p)
    (c* (comp:choose (c+ k r -1) k)
        (c-expt (c- 1.0 p) k)
        (c-expt p r)))

  (define (poisson-pdf k rate)
    (c/ (c* (c-expt rate k) (c-expt (c-neg rate)))
        (comp:factorial k))))

;; ----------------------------------------

(require (for-syntax (submod "." ast)))

(define-syntax (value+grad stx)
  (syntax-case stx ()
    [(_ (x ...) comp-expr)
     (let ()
       (define comp (syntax-local-eval #'(let ([x 'x] ...) comp-expr)))
       (define expr (comp-rad comp (syntax->datum #'(x ...))))
       (datum->syntax #'here expr))]))

(define-syntax (grad stx)
  (syntax-case stx ()
    [(_ (x ...) comp-expr)
     (with-syntax ([(tmp ...) (generate-temporaries #'(x ...))])
       #'(let ([f (value+grad (x ...) comp-expr)])
           (lambda (tmp ...) (let-values ([(value grad) (f tmp ...)]) grad))))]))
