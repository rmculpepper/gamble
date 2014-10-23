;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     syntax/parse/experimental/eh
                     racket/syntax)
         racket/contract
         racket/match
         racket/math
         racket/pretty
         racket/dict
         racket/generic
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "private/dist.rkt"
         "private/dist-impl.rkt"
         "matrix.rkt"
         "private/sort.rkt")
(provide dist?
         integer-dist?
         real-dist?
         finite-dist?
         (contract-out
          [dist-param-count (-> dist? exact-nonnegative-integer?)]
          [dist-pdf
           (->* [dist? any/c] [any/c] real?)]
          [dist-cdf
           (->* [dist? any/c] [any/c any/c] real?)]
          [dist-inv-cdf
           (->* [dist? (real-in 0 1)] [any/c any/c] any)]
          [dist-sample
           (-> dist? any)]
          [dist-enum
           (-> dist? any)]
          [dist-support
           (-> dist? any)]
          [dist-mean
           (-> dist? any)]
          [dist-median
           (-> dist? any)]
          [dist-modes
           (-> dist? any)]
          [dist-variance
           (-> dist? any)]
          [dist-energy
           (-> dist? any/c any)]
          [dist-Denergy
           (->* [dist? any/c] [] #:rest list? any)]
          [dist-update-prior
           (-> dist? any/c vector? (or/c dist? #f))])

         ;; Discrete dists
         discrete-dist
         discrete-dist?
         (contract-out
          [make-discrete-dist
           (->* [dict?] [#:normalize? any/c] discrete-dist?)]
          [make-discrete-dist*
           (->* [vector?]
                [(vectorof (>=/c 0))
                 #:normalize? any/c #:sort? any/c]
                discrete-dist?)]
          [normalize-discrete-dist
           (-> discrete-dist? discrete-dist?)]
          [discrete-dist-values
           (-> discrete-dist? vector?)]
          [discrete-dist-weights
           (-> discrete-dist? vector?)]))

;; Distributions from math/distributions have performance penalty in untyped code
;; (Also, no discrete-dist? predicate.)

(begin-for-syntax

  (define-syntax-class param-spec
    (pattern [param:id ctc:expr]))

  (define-splicing-syntax-class prob-functions
    (pattern (~seq (~or (~once (~seq #:pdf pdf-fun:expr))
                        (~once (~seq #:sample sample-fun:expr))
                        (~optional (~seq #:cdf cdf-fun:expr))
                        (~optional (~seq #:inv-cdf inv-cdf-fun:expr)))
                   ...)))

  (define-eh-alternative-set stat-options
    (pattern
     (~or (~optional (~seq #:support support:expr))
          (~optional (~seq #:enum enum:expr))
          (~optional (~seq #:mean mean:expr))
          (~optional (~seq #:median median:expr))
          (~optional (~seq #:modes modes:expr))
          (~optional (~seq #:variance variance:expr))
          (~optional (~seq #:conjugate conj-fun:expr))
          (~optional (~seq #:Denergy Denergy-fun:expr)))))

  )

(define-syntax (define-dist-type stx)
  (syntax-parse stx
    [(define-dist-type name-dist (p:param-spec ...)
       f:prob-functions
       (~or (~eh-var o stat-options)
            (~optional (~seq #:guard guard-fun:expr))
            (~optional (~and no-provide #:no-provide))
            (~optional (~seq #:extra [extra-clause ...])
                       #:defaults ([(extra-clause 1) '()])))
       ...)
     (with-syntax ([make-name-dist (format-id #'name-dist "make-~a" #'name-dist)]
                   [(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name-dist "~a-~a" #'name-dist param))])
       (quasitemplate
        (begin
          (struct name-dist (p.param ...)
                  #:extra-constructor-name make-name-dist
                  (?? (?@ #:guard guard-fun))
                  #:methods gen:dist
                  [(define (*pdf d x log?)
                     (f.pdf-fun (get-param d) ... x log?))
                   (define (*sample d)
                     (f.sample-fun (get-param d) ...))
                   (define (*cdf d x log? 1-p?)
                     (?? (f.cdf-fun (get-param d) ... x log? 1-p?)
                         (error 'dist-cdf "not defined for distribution\n  given: ~e" d)))
                   (define (*inv-cdf d x log? 1-p?)
                     (?? (f.inv-cdf-fun (get-param d) ... x log? 1-p?)
                         (error 'dist-inv-cdf "not defined for distribution\n  given: ~e" d)))
                   (define (*type d) 'name-dist)
                   (define (*params d) (vector (get-param d) ...))
                   (?? (define (*enum d) (let ([p.param (get-param d)] ...) o.enum)))
                   (?? (define (*support d) (let ([p.param (get-param d)] ...) o.support)))
                   (?? (define (*mean d) (let ([p.param (get-param d)] ...) o.mean)))
                   (?? (define (*median d) (let ([p.param (get-param d)] ...) o.median)))
                   (?? (define (*modes d) (let ([p.param (get-param d)] ...) o.modes)))
                   (?? (define (*variance d) (let ([p.param (get-param d)] ...) o.variance)))
                   (?? (define (*Denergy d x . d/dts)
                         (apply (let ([p.param (get-param d)] ...) o.Denergy-fun) x d/dts)))
                   (?? (define (*conj d data-d data)
                         (let ([p.param (get-param d)] ...)
                           (o.conj-fun data-d data))))]
                  extra-clause ...
                  #:transparent)
          #,(if (attribute no-provide)
                #'(begin)
                #'(provide (contract-out [struct name-dist ([p.param p.ctc] ...)]))))))]))

;; ----

(begin-for-syntax
  (define-syntax-class name-dist-id
    #:attributes (name)
    (pattern nd:id
             #:do [(define m (regexp-match #rx"^(.*)-dist$" (symbol->string (syntax-e #'nd))))]
             #:fail-unless m "expected identifier matching `<name>-dist'"
             #:with name (format-id #'nd "~a" (cadr m))))
  (define-syntax-class kind-kw
    (pattern #:real)
    (pattern #:nat))
  )

(define-syntax (define-fl-dist-type stx)
  (syntax-parse stx
    [(define-fl-dist-type name-dist:name-dist-id (p:param-spec ...)
       kind:kind-kw
       (~optional (~seq #:guard guard-fun:expr))
       (~and (~seq (~or (~eh-var o stat-options)) ...)
             (~seq more-options ...)))
     (define prefix "m:fl")
     (define name #'name-dist.name)
     (with-syntax ([fl-pdf (format-id name "~a~a-pdf" prefix name)]
                   [fl-cdf (format-id name "~a~a-cdf" prefix name)]
                   [fl-inv-cdf (format-id name "~a~a-inv-cdf" prefix name)]
                   [fl-sample (format-id name "~a~a-sample" prefix name)]
                   [(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name-dist "~a-~a" #'name-dist param))]
                   [convert-in #'exact->inexact]
                   [convert-out
                    (syntax-case #'kind ()
                      [#:real #'values]
                      [#:nat  #'inexact->exact])]
                   [make-name-dist (format-id #'name-dist "make-~a" #'name-dist)])
       (quasitemplate
        (begin
          (define (pdf p.param ... x log?)
            (fl-pdf p.param ... (convert-in x) log?))
          (define (cdf p.param ... x log? 1-p?)
            (fl-cdf p.param ... (convert-in x) log? 1-p?))
          (define (inv-cdf p.param ... x log? 1-p?)
            (convert-out (fl-inv-cdf p.param ... x log? 1-p?)))
          (define (sample p.param ...)
            (convert-out (flvector-ref (fl-sample p.param ... 1) 0)))
          (define-dist-type name-dist (p ...)
            #:pdf pdf #:cdf cdf #:inv-cdf inv-cdf #:sample sample
            #:guard (?? guard-fun (lambda (p.param ... _name) (values (convert-in p.param) ...)))
            more-options ...))))]))

;; If every q is 0, returns 0 without evaluating e.
(define-syntax-rule (ifnz [q ...] e)
  (if (and (zero? q) ...) 0 e))

;; Multiply, but short-circuit if first arg evals to 0.
(define-syntax-rule (lazy* a b ...)
  (let ([av a]) (ifnz [av] (* av b ...))))

(define (digamma x) (m:psi0 x))

;; ----

(define-dist-type bernoulli-dist
  ([p (real-in 0 1)])
  #:pdf bernoulli-pdf
  #:cdf bernoulli-cdf
  #:inv-cdf bernoulli-inv-cdf
  #:sample bernoulli-sample
  #:enum 2
  #:support '#s(integer-range 0 1)
  #:mean p
  #:median (cond [(> p 1/2) 1] [(= p 1/2) 1/2] [else 0])
  #:modes (cond [(> p 1/2) '(1)] [(= p 1/2) '(0 1)] [else '(0)])
  #:variance (* p (- 1 p)))

(define-fl-dist-type binomial-dist
  ([n exact-positive-integer?]
   [p (real-in 0 1)])
  #:nat #:enum (add1 n)
  #:support (integer-range 0 n)
  #:mean (* n p)
  #:modes (filter-modes (lambda (x) (m:flbinomial-pdf n p x #f))
                        (let ([m (inexact->exact (floor (* (+ n 1) p)))])
                          (list m (sub1 m))))
  #:variance (* n p (- 1 p)))

(define-fl-dist-type geometric-dist
  ([p (real-in 0 1)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean (/ (- 1 p) p)
  #:modes '(0)
  #:variance (/ (- 1 p) (* p p)))

(define-fl-dist-type poisson-dist
  ([mean (>/c 0)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean mean
  #:modes (if (integer? mean)
              (list (inexact->exact mean) (sub1 (inexact->exact mean)))
              (list (inexact->exact (floor mean))))
  #:variance mean)

(define-fl-dist-type beta-dist
  ([a (>=/c 0)]
   [b (>=/c 0)])
  #:real
  #:support '#s(real-range 0 1) ;; [0,1]
  #:mean (/ a (+ a b))
  #:modes (if (and (> a 1) (> b 1))
              (list (/ (+ a -1) (+ a b -2)))
              '())
  #:variance (/ (* a b) (* (+ a b) (+ a b) (+ a b 1)))
  #:Denergy (lambda (x [dx 1] [da 0] [db 0])
              (+ (lazy* dx (+ (/ (- 1 a) x)
                              (/ (- b 1) (- 1 x))))
                 (lazy* da (- (log x)))
                 (lazy* db (- (log (- 1 x))))
                 (lazy* da (digamma a))
                 (lazy* db (digamma b))
                 (lazy* (+ da db) (- (digamma (+ a b))))))
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(bernoulli-dist _)
                   (beta-dist (+ a (for/sum ([x data] #:when (= x 1)) 1))
                              (+ b (for/sum ([x data] #:when (= x 0)) 0)))]
                  [`(binomial-dist ,n _)
                   (beta-dist (+ a (vector-sum data))
                              (+ b (for/sum ([x (in-vector data)]) (- n x))))]
                  [`(geometric-dist _)
                   (beta-dist (+ a (vector-length data))
                              (+ b (vector-sum data)))]
                  [_ #f])))

(define-fl-dist-type cauchy-dist
  ([mode real?]
   [scale (>/c 0)])
  #:real
  #:support '#s(real-range -inf.0 +inf.0)
  #:mean +nan.0
  #:modes (list mode)
  #:variance +nan.0
  #:Denergy (lambda (x [dx 1] [dm 0] [ds 0])
              (define x-m (- x mode))
              (+ (lazy* ds (/ scale))
                 (* (/ (* 2 scale x-m) (+ (* scale scale) (* x-m x-m)))
                    (- (/ (- dx dm) scale)
                       (lazy* ds (/ x-m scale scale)))))))

(define-fl-dist-type exponential-dist
  ([mean (>/c 0)])
  ;; λ = 1/mean
  #:real
  #:support '#s(real-range 0 +inf.0) ;; [0, inf)
  #:mean mean
  #:modes '(0)
  #:variance (expt mean 2)
  #:Denergy (lambda (x [dx 1] [dm 0])
              (define /mean (/ mean))
              (+ (lazy* dm (- /mean (* x /mean /mean)))
                 (* dx /mean))))

(define-fl-dist-type gamma-dist
  ([shape (>/c 0)]
   [scale (>/c 0)])
  ;; k = shape, θ = scale
  #:real
  #:support '#s(real-range 0 +inf.0) ;; (0,inf)
  #:mean (* shape scale)
  #:modes (if (> shape 1) (list (* (- shape 1) scale)) null)
  #:variance (* shape scale scale)
  #:Denergy (lambda (x [dx 1] [dk 0] [dθ 0])
              (define k shape)
              (define θ scale)
              (+ (lazy* dx (+ (/ (- 1 k) x) (/ θ)))
                 (lazy* dk (+ (digamma k) (log θ) (- (log x))))
                 (lazy* dθ (- (/ k θ) (/ x (* θ θ))))))
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(poisson-dist _)
                   (gamma-dist (+ shape (vector-sum data))
                               (/ scale (add1 (* (vector-length data) scale))))]
                  [`(exponential-dist _)
                   (gamma-dist (+ shape (vector-length data))
                               (/ (+ (/ scale) (vector-sum data))))]
                  [`(gamma-dist ,data-shape _)
                   (gamma-dist (+ shape (* data-shape (vector-length data)))
                               (/ (+ (/ scale) (vector-sum data))))]
                  [`(inverse-gamma-dist ,data-shape _)
                   (gamma-dist (+ shape (* (vector-length data) data-shape))
                               (/ (+ (/ scale) (for/sum ([x (in-vector data)]) (/ x)))))]
                  [`(normal-dist ,data-mean _)
                   (gamma-dist (+ shape (/ (vector-length data) 2))
                               (/ (+ (/ scale)
                                     (* 1/2 (for/sum ([x (in-vector data)])
                                              (sqr (- x data-mean)))))))]
                  [_ #f])))

(define-fl-dist-type inverse-gamma-dist
  ([shape (>/c 0)]
   [scale (>/c 0)])
  #:real
  #:support '#s(real-range 0 +inf.0) ;; (0,inf)
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(normal-dist ,data-mean _)
                   (inverse-gamma-dist
                    (+ shape (/ (vector-ref data) 2))
                    (/ (+ (/ scale)
                          (* 1/2 (for/sum ([x (in-vector data)])
                                   (sqr (- x data-mean)))))))]
                  [_ #f])))

(define-fl-dist-type logistic-dist
  ([mean real?]
   [scale (>/c 0)])
  #:real
  #:support '#s(real-range -inf.0 +inf.0)
  #:mean mean #:median mean #:modes (list mean)
  #:variance (* scale scale pi pi 1/3)
  #:Denergy (lambda (x [dx 1] [dm 0] [ds 0])
              (define s scale)
              (define x-m (- x mean))
              (define A (- (/ (- dx dm) s) (lazy* ds (/ x-m (* s s)))))
              (define B (exp (- (/ x-m s))))
              (+ A
                 (lazy* ds (/ s))
                 (* 2 (/ (+ 1 B)) B (- A)))))

(define-fl-dist-type pareto-dist
  ([scale (>/c 0)]  ;; x_m
   [shape (>/c 0)]) ;; alpha
  #:real
  #:support (real-range scale +inf.0) ;; [scale,inf)
  #:mean (if (<= shape 1)
             +inf.0
             (/ (* scale shape) (sub1 shape)))
  #:modes (list scale)
  #:variance (if (<= shape 2)
                 +inf.0
                 (/ (* scale scale shape)
                    (* (- shape 1) (- shape 1) (- shape 2))))
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(uniform-dist 0 _)
                   (pareto-dist
                    (for/fold ([acc -inf.0]) ([x (in-vector data)]) (max x acc))
                    (+ shape (vector-length data)))]
                  [_ #f])))

(define-fl-dist-type normal-dist
  ([mean real?]
   [stddev (>/c 0)])
  #:real
  #:support '#s(real-range -inf.0 +inf.0)
  #:mean mean
  #:median mean
  #:modes (list mean)
  #:variance (* stddev stddev)
  #:Denergy (lambda (x [dx 1] [dμ 0] [dσ 0])
              (define μ mean)
              (define σ stddev)
              (define x-μ (- x μ))
              (+ (lazy* dσ (- (/ σ) (/ (* x-μ x-μ) (* σ σ σ))))
                 (lazy* (- dx dμ)
                        (/ x-μ (* σ σ)))))
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(normal-dist _ ,data-stddev)
                   (normal-dist (/ (+ (/ mean (sqr stddev))
                                      (/ (vector-sum data)
                                         (sqr data-stddev)))
                                   (+ (/ (sqr stddev))
                                      (/ (vector-length data)
                                         (sqr data-stddev))))
                                (sqrt
                                 (/ (+ (/ (sqr stddev))
                                       (/ (vector-length data)
                                          (sqr data-stddev))))))]
                  [_ #f])))

(define-fl-dist-type uniform-dist
  ([min real?]
   [max real?])
  #:real
  #:guard (lambda (a b _name)
            (unless (< a b)
              (error 'uniform-dist
                     "lower bound is not less than upper bound\n  lower: ~e\n  upper: ~e"
                     a b))
            (values (exact->inexact a) (exact->inexact b)))
  #:support (real-range min max)
  #:mean (/ (+ min max) 2)
  #:median (/ (+ min max) 2)
  #:variance (* (- max min) (- max min) 1/12)
  #:Denergy (lambda (x [dx 1] [dmin 0] [dmax 0])
              (cond [(<= min x max)
                     (lazy* (- dmax dmin) (/ (- max min)))]
                    [else 0])))

;; ----------------------------------------

(define-dist-type categorical-dist
  ([weights (vectorof (>=/c 0))])
  #:pdf categorical-pdf
  #:cdf categorical-cdf
  #:inv-cdf categorical-inv-cdf
  #:sample categorical-sample
  #:guard (lambda (weights _name) (validate/normalize-weights 'categorical-dist weights))
  #:enum (vector-length weights)
  #:support (integer-range 0 (sub1 (vector-length weights)))
  #:mean (for/sum ([i (in-naturals)] [w (in-vector weights)]) (* i w))
  #:modes (let-values ([(best best-w)
                        (for/fold ([best null] [best-w -inf.0])
                                  ([i (in-naturals)] [w (in-vector weights)])
                          (cond [(> w best-w)
                                 (values (list i) w)]
                                [(= w best-w)
                                 (values (cons i best) best-w)]
                                [else (values best best-w)]))])
            (reverse best)))

;; FIXME: flag for symmetric alphas, can sample w/ fewer gamma samplings
(define-dist-type dirichlet-dist
  ([alpha (vectorof (>/c 0))])
  #:pdf dirichlet-pdf
  #:sample dirichlet-sample
  #:guard (lambda (alpha _name)
            (vector->immutable-vector (vector-map exact->inexact alpha)))
  ;; #:support ;; [0,1]^n
  ;; (product (make-vector (vector-length concentrations) '#s(real-range 0 1)))
  #:mean (let ([alphasum (vector-sum alpha)])
           (for/vector ([ai (in-vector alpha)]) (/ ai alphasum)))
  #:modes (if (for/and ([ai (in-vector alpha)]) (> ai 1))
              (let ([denom (for/sum ([ai (in-vector alpha)]) (sub1 ai))])
                (list (for/vector ([ai (in-vector alpha)]) (/ (sub1 ai) denom))))
              null)
  #:variance (let* ([a0 (vector-sum alpha)]
                    [denom (* a0 a0 (add1 a0))])
               (for/vector ([ai (in-vector alpha)])
                 (/ (* ai (- a0 ai)) denom)))
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(categorical-dist _)
                   (define n (vector-length alpha))
                   (define countv (make-vector n 0))
                   (for ([x (in-vector data)] [i (in-range n)])
                     (vector-set! countv i (add1 (vector-ref countv i))))
                   (dirichlet-dist (vector-map + alpha countv))]
                  [_ #f])))

(define-dist-type multi-normal-dist
  ([mean col-matrix?] [cov square-matrix?])
  #:pdf multi-normal-pdf
  #:sample multi-normal-sample
  #:guard (lambda (mean cov _name)
            (define n (matrix-num-rows mean))
            (unless (= n (square-matrix-size cov))
              (error 'multi-normal-dist
                     "covariance matrix has wrong number of rows\n  expected: ~s\n value: ~e"
                     n cov))
            ;; check cov is symmetric, nonnegative-definite (?)
            (values mean cov))
  #:mean mean
  #:modes (list mean) ;; FIXME: degenerate cases?
  #:variance cov)

(define-dist-type wishart-dist
  ([n real?] [V square-matrix?])
  #:pdf wishart-pdf #:sample wishart-sample
  #:guard (lambda (n V _name)
            (unless (matrix-symmetric? V)
              (error 'wishart-dist "expected symmetric matrix\n  given: ~e" V))
            (define p (square-matrix-size V))
            (unless (> n (- p 1))
              (error 'wishart-dist "expected n > p - 1\n  n: ~e\n  p: ~e" n p))
            ;; FIXME: check V positive definite
            (values n V)))

(define-dist-type inverse-wishart-dist
  ([n real?] [Vinv square-matrix?])
  #:pdf inverse-wishart-pdf #:sample inverse-wishart-sample
  #:guard (lambda (n Vinv _name)
            (unless (matrix-symmetric? Vinv)
              (error 'wishart-dist "expected symmetric matrix\n  given: ~e" Vinv))
            (define p (square-matrix-size Vinv))
            (unless (> n (- p 1))
              (error 'wishart-dist "expected n > p - 1\n  n: ~e\n  p: ~e" n p))
            ;; FIXME: check V positive definite
            (values n Vinv)))

;; Not a real dist. Useful for throwing arbitrary factors into a trace.
(define-dist-type improper-dist
  ([ldensity real?])
  #:pdf improper-pdf #:sample improper-sample)

;; ============================================================
;; Discrete distribution

;; Categorical dist has support 1..N; discrete has arbitrary values as support.
;; Prints nicely (sort), but non-standard constructor, can't use as match pattern.

;; Not automatically normalized.
;; Uses equal? to distinguish elements of support.

(define-dist-type *discrete-dist
  ([vs vector?]
   [ws vector?]
   [wsum real?])
  #:pdf discrete-pdf
  #:sample discrete-sample
  #:enum vs
  #:support 'finite
  #:no-provide
  #:extra
  [#:property prop:custom-write
   (lambda (obj port mode)
     (print-discrete-dist (*discrete-dist-vs obj) (*discrete-dist-ws obj) port mode))])

(define (discrete-dist? x) (*discrete-dist? x))
(define (discrete-dist-values d) (*discrete-dist-vs d))
(define (discrete-dist-weights d) (*discrete-dist-ws d))

(define-syntax (discrete-dist stx)
  (define-splicing-syntax-class maybe-normalize
    (pattern (~seq #:normalize? normalize?:expr))
    (pattern (~seq) #:with normalize? #'#t))
  (define-syntax-class vwpair
    #:description "pair of value and weight expressions"
    (pattern [value:expr weight]
             #:declare weight (expr/c #'(>=/c 0))))
  (syntax-parse stx
    [(discrete-dist :maybe-normalize p:vwpair ...)
     #'(make-discrete-dist* #:normalize? normalize?
                            #:sort? #t
                            (vector p.value ...) (vector p.weight ...))]))

(define (make-discrete-dist dict #:normalize? [normalize? #t])
  (define len (dict-count dict))
  (define vs (make-vector len #f))
  (define ws (make-vector len #f))
  (for ([(v w) (in-dict dict)]
        [i (in-naturals)])
    (vector-set! vs i v)
    (vector-set! ws i w))
  (for ([w (in-vector ws)])
    (unless (and (rational? w) (>= w 0))
      (raise-argument-error 'dict->discrete-dist "(dict/c any/c (>=/c 0))" dict)))
  (make-discrete-dist* vs ws #:normalize? normalize? #:sort? #t))

(define (make-discrete-dist* vs
                             [ws (let ([len (vector-length vs)])
                                   (make-vector len (/ len)))]
                             #:normalize? [normalize? #t]
                             #:sort? [sort? #t])
  (unless (= (vector-length vs) (vector-length ws))
    (error 'make-discrete-dist
           "values and weights vectors have different lengths\n  values: ~e\n  weights: ~e"
           vs ws))
  (define-values (vs1 ws1)
    (if sort?
        (combine-duplicates vs ws)
        (values vs ws)))
  (define vs* (vector->immutable-vector vs1))
  (define-values (ws* wsum*)
    (if normalize?
        (let ([wsum (vector-sum ws1)])
          (values (vector->immutable-vector
                   (vector-map (lambda (w) (/ w wsum)) ws1))
                  1))
        (values (vector->immutable-vector ws1)
                (vector-sum ws1))))
  (*discrete-dist vs* ws* wsum*))

(define (normalize-discrete-dist d)
  (make-discrete-dist* (*discrete-dist-vs d) (*discrete-dist-ws d) #:normalize? #t))

(define (print-discrete-dist vs ws port mode)
  (define (recur x p)
    (case mode
      ((#t) (write x p))
      ((#f) (display x p))
      ((0 1) (print x p mode))))

  ;; Only two cases: 0 vs everything else
  (define (print-prefix p)
    (case mode
      [(0) (write-string "(discrete-dist" p)]
      [else (write-string "#<discrete-dist:" p)]))
  (define (print-suffix p)
    (case mode
      [(0) (write-string ")" p)]
      [else (write-string ">" p)]))

  (define (print-contents p leading-space)
    (let ([lead (if leading-space (make-string (add1 leading-space) #\space) " ")])
      (for ([v (in-vector vs)]
            [w (in-vector ws)])
        (when leading-space
          (pretty-print-newline p (pretty-print-columns)))
        (write-string lead p)
        (write-string "[" p)
        (recur v p)
        (write-string " " p)
        (recur w p)
        (write-string "]" p))))

  (define (print/one-line p)
    (print-prefix p)
    (print-contents p #f)
    (print-suffix p))

  (define (print/multi-line p)
    (let-values ([(line col pos) (port-next-location p)])
      (print-prefix p)
      (print-contents p col)
      (print-suffix p)))

  (cond [(and (pretty-printing)
              (integer? (pretty-print-columns)))
         ((let/ec esc
            (letrec ([tport
                      (make-tentative-pretty-print-output-port
                       port
                       (- (pretty-print-columns) 1)
                       (lambda () 
                         (esc
                          (lambda ()
                            (tentative-pretty-print-port-cancel tport)
                            (print/multi-line port)))))])
              (print/one-line tport)
              (tentative-pretty-print-port-transfer tport port))
            void))]
        [else
         (print/one-line port)])
  (void))
