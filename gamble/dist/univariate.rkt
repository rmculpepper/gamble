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
         (rename-in racket/match [match-define defmatch])
         racket/math
         racket/pretty
         racket/dict
         racket/generic
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "../private/dist.rkt"
         "../private/interfaces.rkt"
         "../private/instrument.rkt"
         "../private/dist-define.rkt"
         "../private/dist-impl.rkt")
(provide #| implicit in define-dist-type |#)

;; Multiply, but short-circuit if first arg evals to 0.
(define-syntax-rule (lazy* a b ...)
  (let ([av a]) (if (zero? av) 0 (* av b ...))))

(define (digamma x) (m:psi0 x))

;; ============================================================

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
  #:variance (* p (- 1 p))
  #:drift (lambda (value scale-factor) (cons (- 1 value) 0.0)))

(define-fl-dist-type binomial-dist
  ([n exact-nonnegative-integer?]
   [p (real-in 0 1)])
  #:nat #:enum (add1 (inexact->exact n))
  #:support (integer-range 0 (inexact->exact n))
  #:mean (* n p)
  #:modes (filter-modes (lambda (x) (m:flbinomial-pdf n p x #f))
                        (let ([m (inexact->exact (floor (* (+ n 1) p)))])
                          (list m (sub1 m))))
  #:variance (* n p (- 1 p))
  #:drift (lambda (value scale-factor)
            (drift:add-discrete-normal value (* scale-factor (sqrt (* n p (- 1 p)))) 0 n)))
;; DRIFT: normal w/ computed scale, discretize (round away from zero), add, reject if not in range
;; (Q: can any reasonable scale lead to high rejection rate?)
;; NOTE: it's harder than it looks: asymmetric, need to normalize for rejecting, etc
;; **OR** (Sean recommends): use discrete version of beta resampling for uniform
;; The proposal is asymmetric in either case.

(define-fl-dist-type geometric-dist
  ([p (real-in 0 1)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean (/ (- 1 p) p)
  #:modes '(0)
  #:variance (/ (- 1 p) (* p p))
  #:drift (lambda (value scale-factor)
            (drift:add-discrete-normal value (* scale-factor (sqrt (- 1 p)) (/ p)) 0 +inf.0)))

(define-fl-dist-type poisson-dist
  ([mean (>/c 0)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean mean
  #:modes (if (integer? mean)
              (list (inexact->exact mean) (sub1 (inexact->exact mean)))
              (list (inexact->exact (floor mean))))
  #:variance mean
  #:drift (lambda (value scale-factor)
            (drift:add-discrete-normal value (* scale-factor (sqrt mean)) 0 +inf.0)))

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
                  [_ #f]))
  #:drift (lambda (value scale-factor)
            ;; mode = α / (α + β), peakedness = α + β = S (our choice)
            ;; So if we want dist peaked at x:
            ;;   α = S * x
            ;;   β = S - α = S * (1 - x)
            (define S 10) ;; "peakedness" parameter
            (drift:asymmetric (lambda (x) (beta-dist (* S x) (* S (- 1 x)))) value)))

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
                       (lazy* ds (/ x-m scale scale))))))
  #:drift (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor))))

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
                 (* dx /mean)))
  #:drift (lambda (value scale-factor) (drift:mult-exp-normal value (* mean scale-factor))))

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
                  [_ #f]))
  #:drift (lambda (value scale-factor) (drift:mult-exp-normal value (* scale (sqrt shape) scale-factor))))

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
                  [_ #f]))
  #:drift (lambda (value scale-factor)
            (defmatch (cons value* R-F)
              (drift:mult-exp-normal (/ value) (* scale (sqrt shape) scale-factor)))
            (cons (/ value*) R-F)))

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
                 (* 2 (/ (+ 1 B)) B (- A))))
  #:drift (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor))))

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
;; DRIFT: FIXME

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
                  [_ #f]))
  #:drift (lambda (value scale-factor) (drift:add-normal value (* stddev scale-factor))))

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
                    [else 0]))
  #:drift (lambda (value scale-factor)
            ;; Use beta to get proposal for Uniform(0,1), adjust.
            (define S 10) ;; "peakedness"
            (define (to-01 x) (/ (- x min) (- max min)))
            (define (from-01 x) (+ (* x (- max min)) min))
            (defmatch (cons value* R-F)
              (drift:asymmetric (lambda (x) (beta-dist (* S x) (* S (- 1 x))))
                                (to-01 value)))
            (cons (from-01 value*) R-F)))


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

(define-dist-type multinomial-dist
  ([n exact-nonnegative-integer?]
   [weights (vectorof (>=/c 0))])
  #:pdf multinomial-dist
  #:sample multinomial-sample
  #:has-mass
  #:guard (lambda (n weights _name)
            (values n (validate/normalize-weights 'multinomial-dist weights)))
  #:drift (lambda (value scale-factor)
            (multinomial-drift n weights value scale-factor)))

  ;; FIXME: doesn't belong in univariate, exactly, but doesn't use matrices
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
;; DRIFT: (1 - eps) * value + eps * Dir(alpha)
;; ie, weighted avg of current value and new Dirichlet draw
;; Q: for alpha, should use either same parameters, OR could use uniform (1 ...)???
;; ** OR **: take current value, multiply by f(scale-factor), use that as Dirichlet param, draw
;; NOTE: not symmetric!

;; Not a real dist. Useful for throwing arbitrary factors into a trace.
(define-dist-type improper-dist
  ([ldensity real?])
  #:pdf improper-pdf #:sample improper-sample)


;; ============================================================
;; Univariate dist functions

;; ------------------------------------------------------------
;; Bernoulli dist functions
;; -- math/dist version converts exact->inexact

(define (bernoulli-pdf prob v log?)
  (define p
    (cond [(eqv? v 0) (- 1 prob)]
          [(eqv? v 1) prob]
          [else 0]))
  (convert-p p log? #f))
(define (bernoulli-cdf prob v log? 1-p?)
  (define p
    (cond [(< v 0) 0]
          [(< v 1) (- 1 prob)]
          [else 1]))
  (convert-p p log? 1-p?))
(define (bernoulli-inv-cdf prob p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (cond [(<= p (- 1 prob)) 0]
        [else 1]))
(define (bernoulli-sample prob)
  (if (<= (random) prob) 1 0))


;; ------------------------------------------------------------
;; Categorical weighted dist functions
;; -- Assume weights are nonnegative, normalized.

(define (categorical-pdf probs k log?)
  (cond [(not (< k (vector-length probs)))
         (impossible log? 'categorical "index out of bounds")]
        [else
         (define l (vector-ref probs k))
         (if log? (log l) l)]))
(define (categorical-cdf probs k log? 1-p?)
  (define p (for/sum ([i (in-range (add1 k))] [prob (in-vector probs)]) prob))
  (convert-p p log? 1-p?))
(define (categorical-inv-cdf probs p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (let loop ([i 0] [p p])
    (cond [(>= i (vector-length probs))
           (error 'categorical-dist:inv-cdf "out of values")]
          [(< p (vector-ref probs i))
           i]
          [else
           (loop (add1 i) (- p (vector-ref probs i)))])))
(define (categorical-sample probs)
  (categorical-inv-cdf probs (random) #f #f))


;; ------------------------------------------------------------
;; Multinomial weighted dist functions
;; -- Assume weights are nonnegative, normalized.

;; sampling and pdf as repeated binomial

(define (multinomial-pdf n0 probs v log?)
  (cond [(and (vector? v) (= (vector-length v) (vector-length probs)))
         (define n (exact->inexact n0))
         (define ll
           (for/sum ([vi (in-vector v)]
                     [prob (in-vector (multinomial->binomial-weights probs))])
             (m:flbinomial-pdf n prob (exact->inexact vi) #t)))
         (if log? ll (exp ll))]
        [else
         (impossible log? 'multinomial "not a vector of correct size")]))

(define (multinomial-sample n probs)
  (define v (make-vector (vector-length probs)))
  (for ([i (in-range (vector-length probs))]
        [prob (in-vector (multinomial->binomial-weights probs))])
    (define k
      (inexact->exact
       (flvector-ref (m:flbinomial-sample (exact->inexact n) prob 1) 0)))
    (vector-set! v i k))
  v)

;; Do some number of moves based on scale-factor. Symmetric.
(define (multinomial-drift n _probs old scale-factor)
  (cond [(or (zero? n) (<= (vector-length old) 1))
         (cons old 0)]
        [else
         (define v (vector-copy old))
         (define (pick-nonempty-index)
           (define i (random (vector-length v)))
           (if (zero? (vector-ref v i))
               (pick-nonempty-index)
               i))
         (define (pick-other-index i)
           (define j (random (sub1 (vector-length v))))
           (if (>= j i) (add1 j) j))
         (for ([_a (in-range (inexact->exact (ceiling (* n scale-factor))))])
           (define i (pick-nonempty-index))
           (define j (pick-other-index i))
           (vector-set! v i (sub1 (vector-ref v i)))
           (vector-set! v j (add1 (vector-ref v j))))
         (cons v 0)]))


;; Given (vector pi ...) where pi = prob(X = i), produce
;; (vector qi ...) where qi = prob(X = i | X >= i).
(define (multinomial->binomial-weights probs)
  (define v (make-vector (vector-length probs)))
  (for/fold ([prest 1.0])
            ([probi (in-vector probs)] [i (in-naturals)])
    (vector-set! v i (/ probi prest))
    (* prest (- 1.0 probi)))
  (vector-set! v (sub1 (vector-length probs)) 1.0)
  v)


;; ------------------------------------------------------------
;; Inverse gamma distribution

(define (m:flinverse-gamma-pdf shape scale x log?)
  (m:flgamma-pdf shape scale (/ x) log?))
(define (m:flinverse-gamma-cdf shape scale x log? 1-p?)
  (m:flgamma-cdf shape scale (/ x) log? 1-p?))
(define (m:flinverse-gamma-inv-cdf shape scale x log? 1-p?)
  (/ (m:flgamma-inv-cdf shape scale x log? 1-p?)))
(define (m:flinverse-gamma-sample shape scale n)
  (define flv (m:flgamma-sample shape scale n))
  (for ([i (in-range n)])
    (flvector-set! flv i (/ (flvector-ref flv i))))
  flv)


;; ------------------------------------------------------------
;; Pareto distribution

(define (m:flpareto-pdf scale shape x log?)
  (define lp
    (if (>= x scale)
        (- (+ (log shape) (* shape (log scale)))
           (* (add1 shape) (log x)))
        -inf.0))
  (if log? lp (exp lp)))
(define (m:flpareto-cdf scale shape x log? 1-p?)
  (define p
    (if (> x scale)
        (- 1.0 (expt (/ scale x) shape))
        0.0))
  (convert-p p log? 1-p?))
(define (m:flpareto-inv-cdf scale shape p log? 1-p?)
  (define p* (unconvert-p p log? 1-p?))
  (* scale (expt p* (- (/ shape)))))
(define (m:flpareto-sample scale shape n)
  (define flv (make-flvector n))
  (for ([i (in-range n)])
    (flvector-set! flv i (m:flpareto-inv-cdf scale shape (random) #f #f)))
  flv)


;; ------------------------------------------------------------
;; Dirichlet distribution

(define-memoize1 (log-multinomial-beta alpha)
  (- (for/sum ([ai (in-vector alpha)]) (m:log-gamma ai))
     (m:log-gamma (for/sum ([ai (in-vector alpha)]) ai))))

(define (dirichlet-pdf alpha x log?)
  (cond [(not (vector? x))
         (impossible log? 'dirichlet "not a vector")]
        [(not (= (vector-length x) (vector-length alpha)))
         (impossible log? 'dirichlet "vector has wrong length")]
        [else
         (define lp
           (- (for/sum ([xi (in-vector x)] [ai (in-vector alpha)]) (* (sub1 ai) (log xi)))
              (log-multinomial-beta alpha)))
         (if log? lp (exp lp))]))
(define (dirichlet-sample alpha)
  ;; TODO: batch gamma sampling when all alphas same?
  (define n (vector-length alpha))
  (define x (make-vector n))
  (for ([a (in-vector alpha)] [i (in-range n)])
    (vector-set! x i (flvector-ref (m:flgamma-sample a 1.0 1) 0)))
  (define gsum (for/sum ([g (in-vector x)]) g))
  (for ([i (in-range n)])
    (vector-set! x i (/ (vector-ref x i) gsum)))
  x)


;; ============================================================
;; Improper dist functions

(define (improper-pdf ldensity v log?)
  (if log? ldensity (exp ldensity)))
(define (improper-sample ldensity)
  (error 'improper-dist:sample "not implemented"))

;; ============================================================
;; Convenience functions

;; == Finite distributions ==

(begin-instrumented
 ;; flip : Prob -> (U #t #f)
 (define (flip [prob 1/2])
   (01->boolean (sample (bernoulli-dist prob)))))

(define (01->boolean n) (positive? n)) ;; Not exported, no need for error checking.
(declare-observation-propagator (01->boolean _)
  boolean? (lambda (y) (if y 1 0)) (lambda (x) 1))

;; bernoulli : Prob -> (U 1 0)
(define (bernoulli [prob 1/2])
  (sample (bernoulli-dist prob)))

;; categorical : (Vectorof Prob) -> Nat
(define (categorical weights)
  (sample (categorical-dist weights)))

;; discrete-uniform : Nat -> Nat
(define (discrete-uniform n)
  (sample (categorical-dist (make-vector n (/ n)))))

;; == Countable distributions ==

;; binomial : Nat Prob -> Integer
(define (binomial n p)
  (sample (binomial-dist n p)))

;; geometric : Prob -> Integer
(define (geometric [p 1/2])
  (sample (geometric-dist p)))

;; poisson : Real -> Integer
(define (poisson mean)
  (sample (poisson-dist mean)))

;; == Continuous distributions ==

;; beta : PositiveReal PositiveReal -> Real in [0,1]
(define (beta a b)
  (sample (beta-dist a b)))

(define (cauchy [mode 0] [scale 1])
  (sample (cauchy-dist mode scale)))

;; exponential : PositiveReal -> PositiveReal
;; NOTE: mean aka scale = 1/rate
(define (exponential [mean 1])
  (sample (exponential-dist mean)))

;; gamma : PositiveReal PositiveReal -> Real
;; NOTE: scale = 1/rate
(define (gamma [shape 1] [scale 1])
  (sample (gamma-dist shape scale)))

(define (inverse-gamma [shape 1] [scale 1])
  (sample (inverse-gamma-dist shape scale)))

;; logistic : Real Real -> Real
(define (logistic [mean 0] [scale 1])
  (sample (logistic-dist mean scale)))

;; normal : Real PositiveReal -> Real
;; NOTE: stddev = (sqrt variance)
(define (normal [mean 0] [stddev 1])
  (sample (normal-dist mean stddev)))

;; pareto : Real Real -> Real
(define (pareto shape scale)
  (sample (pareto-dist shape scale)))

;; uniform : Real Real -> Real
(define uniform
  (case-lambda
    [() (uniform 0 1)]
    [(max) (uniform 0 max)]
    [(min max)
     (sample (uniform-dist min max))]))

;; == Dirichlet ==

(define (dirichlet alpha)
  (sample (dirichlet-dist alpha)))

;; == Improper ==

;; factor : Real -> Real
;; Weight the current trace by the given log-factor.
(define (factor ldensity)
  (observe-sample (improper-dist ldensity) 0))

;; == Exports ==

(define (probability? x)
  (and (real? x) (<= 0 x 1)))

;; The following stochastic procedures have codomain contract of any
;; so that their internal call to sample is in tail position (no
;; result check frame). (Except for flip, FIXME.)
;; Also true for similar functions in multivariate.rkt.
(provide (contract-out
          [flip
           (->* [] [probability?] any)]
          [bernoulli
           (->* [] [probability?] any)]
          [categorical
           (-> (vectorof (>=/c 0)) 
               any)]
          [discrete-uniform
           (-> exact-positive-integer? any)]
          [geometric
           (->* [] [probability?] any)]
          [poisson
           (-> (>/c 0) any)]
          [binomial
           (-> exact-nonnegative-integer? probability? any)]
          [beta
           (-> (>/c 0) (>/c 0) any)]
          [cauchy
           (->* [] [real? (>/c 0)] any)]
          [exponential
           (->* [] [(>/c 0)] any)]
          [gamma
           (->* [] [(>/c 0) (>/c 0)] any)]
          [inverse-gamma
           (->* [] [(>/c 0) (>/c 0)] any)]
          [logistic
           (->* [] [real? (>/c 0)] any)]
          [normal
           (->* [] [real? (>/c 0)] any)]
          [pareto
           (-> (>/c 0) (>/c 0) any)]
          [uniform
           (->* [] [real? real?] any)]
          [dirichlet
           (-> (vectorof (>/c 0)) any)]
          [factor
           (-> real? any)]))
