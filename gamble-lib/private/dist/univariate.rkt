;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract/base
         racket/match
         racket/math
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "base.rkt"
         "define.rkt")
(provide (all-defined-out))

;; Multiply, but short-circuit if first arg evals to 0.
;; FIXME: preserve (in)exactness?
(define-syntax-rule (lazy* a b ...)
  (let ([av a]) (if (zero? av) 0 (* av b ...))))

;; ============================================================
;; Continuous real distributions from math library

(define-real-dist-struct binomial-dist
  ([n exact-nonnegative-integer? #:exact]
   [p (real-in 0 1)])
  #:nat #:prefix m:flbinomial
  #:real-methods
  [(define (-support self)
     (match-define (binomial-dist n _) self)
     (cons 0 n))
   (define (-mean self)
     (match-define (binomial-dist n p) self)
     (* n p))
   (define (-modes self)
     (match-define (binomial-dist n p) self)
     (filter-modes (lambda (x) (m:flbinomial-pdf n p x #f))
                   (let ([m (inexact->exact (floor (* (+ n 1) p)))])
                     (list m (sub1 m)))))
   (define (-variance self)
     (match-define (binomial-dist n p) self)
     (* n p (- 1 p)))]
  #:methods gen:enum-dist
  [(define (-enum self)
     (match-define (binomial-dist n _) self)
     (add1 n))]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt (* n p (- 1 p))))))
  #:drift1 (lambda (value scale-factor)
             (drift:add-discrete-normal value (* scale-factor (sqrt (* n p (- 1 p)))) 0 n))
  |#)

(define-real-dist-struct geometric-dist
  ([p (real-in 0 1)])
  #:nat #:prefix m:flgeometric
  #:real-methods
  [(define (-support self)
     '(0 . +inf.0))
   (define (-mean self)
     (match-define (geometric-dist p) self)
     (/ (- 1 p) p))
   (define (-modes self)
     '(0))
   (define (-variance self)
     (match-define (geometric-dist p) self)
     (/ (- 1 p) (* p p)))]
  #:methods gen:enum-dist
  [(define (-enum self)
     'lazy)]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt (- 1 p)) (/ p)) 0 +inf.0))
  #:drift1 (lambda (value scale-factor)
             (drift:add-discrete-normal value (* scale-factor (sqrt (- 1 p)) (/ p)) 0 +inf.0))
  |#)

(define-real-dist-struct poisson-dist
  ([mean (>/c 0)])
  #:nat #:prefix m:flpoisson
  #:real-methods
  [(define (-support self)
     '(0 . +inf.0))
   (define (-mean self)
     (match-define (poisson-dist mean) self)
     mean)
   (define (-modes self)
     (match-define (poisson-dist mean) self)
     (if (integer? mean)
         (list mean (sub1 mean))
         (list (floor mean))))
   (define (-variance self)
     (match self [(poisson-dist mean) mean]))]
  #:methods gen:enum-dist
  [(define (-enum self) 'lazy)]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt mean)) 0 +inf.0))
  #:drift1 (lambda (value scale-factor)
             (drift:add-discrete-normal value (* scale-factor (sqrt mean)) 0 +inf.0)))
  |#)

(define-real-dist-struct beta-dist
  ([a (>=/c 0)]
   [b (>=/c 0)])
  #:real #:prefix m:flbeta
  #:real-methods
  [(define (-support self) '(0 . 1))
   (define (-mean self)
     (match self [(beta-dist a b) (/ a (+ a b))]))
   (define (-modes self)
     (match-define (beta-dist a b) self)
     (if (and (> a 1) (> b 1))
         (list (/ (+ a -1) (+ a b -2)))
         '()))
   (define (-variance self)
     (match-define (beta-dist a b) self)
     (/ (* a b) (* (+ a b) (+ a b) (+ a b 1))))
   (define (-denergy self x [dx 1] [da 0] [db 0])
     (match-define (beta-dist a b) self)
     (+ (lazy* dx (+ (/ (- 1 a) x)
                     (/ (- b 1) (- 1 x))))
        (lazy* da (- (log x)))
        (lazy* db (- (log (- 1 x))))
        (lazy* da (digamma a))
        (lazy* db (digamma b))
        (lazy* (+ da db) (- (digamma (+ a b))))))]
  #|
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
  #:drift-dist (lambda (value scale-factor)
                 ;; mode = α / (α + β), peakedness = α + β = S (our choice)
                 ;; So if we want dist peaked at x:
                 ;;   α = S * x
                 ;;   β = S - α = S * (1 - x)
                 (define S 10) ;; "peakedness" parameter
                 (beta-dist (* S value) (* S (- 1 value))))
  |#)

(define-real-dist-struct cauchy-dist
  ([mode rational?]
   [scale (>/c 0)])
  #:real #:prefix m:flcauchy
  #:real-methods
  [(define (-support self) '(-inf.0 . +inf.0))
   (define (-mean self) +nan.0)
   (define (-modes self) (list (cauchy-dist-mode self)))
   (define (-variance self) +nan.0)
   (define (-denergy self x [dx 1] [dm 0] [ds 0])
     (match-define (cauchy-dist mode scale) self)
     (define x-m (- x mode))
     (+ (lazy* ds (/ scale))
        (* (/ (* 2 scale x-m) (+ (* scale scale) (* x-m x-m)))
           (- (/ (- dx dm) scale)
              (lazy* ds (/ x-m scale scale))))))]
  #|
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* scale scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor)))
  |#)

(define-real-dist-struct exponential-dist
  ([mean (>/c 0)])
  ;; λ = 1/mean
  #:real #:prefix m:flexponential
  #:real-methods
  [(define (-support self) '(0 . +inf.0))
   (define (-mean self)
     (match-define (exponential-dist mean) self)
     mean)
   (define (-modes self) '(0))
   (define (-variance self)
     (match-define (exponential-dist mean) self)
     (expt mean 2))
   (define (-denergy self x [dx 1] [dm 0])
     (match-define (exponential-dist mean) self)
     (define /mean (/ mean))
     (+ (lazy* dm (- /mean (* x /mean /mean)))
        (* dx /mean)))]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (mult-exp-normal-dist value (* mean scale-factor)))
  #:drift1 (lambda (value scale-factor)
             (drift:mult-exp-normal value (* mean scale-factor)))
  |#)

(define-real-dist-struct gamma-dist
  ([shape (>/c 0)]
   [scale (>/c 0)])
  ;; k = shape, θ = scale
  #:real #:prefix m:flgamma
  #:real-methods
  [(define (-support self) '(0 . +inf.0)) ;; (0, inf)
   (define (-mean self)
     (match-define (gamma-dist shape scale) self)
     (* shape scale))
   (define (-modes self)
     (match-define (gamma-dist shape scale) self)
     (if (> shape 1) (list (* (- shape 1) scale)) null))
   (define (-variance self)
     (match-define (gamma-dist shape scale) self)
     (* shape scale scale))
   (define (-denergy self x [dx 1] [dk 0] [dθ 0])
     (match-define (gamma-dist shape scale) self)
     (define k shape)
     (define θ scale)
     (+ (lazy* dx (+ (/ (- 1 k) x) (/ θ)))
        (lazy* dk (+ (digamma k) (log θ) (- (log x))))
        (lazy* dθ (- (/ k θ) (/ x (* θ θ))))))]
  #|
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
  #:drift-dist (lambda (value scale-factor)
                 (mult-exp-normal-dist value (* scale (sqrt shape) scale-factor)))
  #:drift1 (lambda (value scale-factor)
             (drift:mult-exp-normal value (* scale (sqrt shape) scale-factor)))
  |#)

(define-real-dist-struct logistic-dist
  ([mean real?]
   [scale (>/c 0)])
  #:real #:prefix m:fllogistic
  #:real-methods
  [(define (-support self) '(-inf.0 . +inf.0))
   (define (-mean self) (logistic-dist-mean self))
   (define (-median self) (logistic-dist-mean self))
   (define (-modes self) (list (logistic-dist-mean self)))
   (define (-variance self)
     (match-define (logistic-dist mean scale) self)
     (* scale scale pi pi 1/3))
   (define (-denergy self x [dx 1] [dm 0] [ds 0])
     (match-define (logistic-dist mean scale) self)
     (define s scale)
     (define x-m (- x mean))
     (define A (- (/ (- dx dm) s) (lazy* ds (/ x-m (* s s)))))
     (define B (exp (- (/ x-m s))))
     (+ A
        (lazy* ds (/ s))
        (* 2 (/ (+ 1 B)) B (- A))))]
  #|
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* scale scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor)))
  |#)

(define-real-dist-struct normal-dist
  ([mean real?]
   [stddev (>/c 0)])
  #:real #:prefix m:flnormal
  #:real-methods
  [(define (-support self) '(-inf.0 . +inf.0))
   (define (-mean self) (normal-dist-mean self))
   (define (-median self) (normal-dist-mean self))
   (define (-modes self) (list (normal-dist-mean self)))
   (define (-variance self)
     (match-define (normal-dist mean stddev) self)
     (* stddev stddev))
   (define (-denergy self x [dx 1] [dμ 0] [dσ 0])
     (match-define (normal-dist mean stddev) self)
     (define μ mean)
     (define σ stddev)
     (define x-μ (- x μ))
     (+ (lazy* dσ (- (/ σ) (/ (* x-μ x-μ) (* σ σ σ))))
        (lazy* (- dx dμ)
               (/ x-μ (* σ σ)))))]
  #|
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
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* stddev scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* stddev scale-factor)))
  |#)

(define-real-dist-struct uniform-dist
  ([min real?]
   [max real?])
  #:real #:prefix m:fluniform
  #:guard (lambda (a b _name)
            (unless (< a b)
              (error 'uniform-dist
                     "lower bound is not less than upper bound\n  lower: ~e\n  upper: ~e"
                     a b))
            (values a b))
  #:real-methods
  [(define (-support self)
     (match-define (uniform-dist min max) self)
     (cons min max))
   (define (-mean self)
     (match-define (uniform-dist min max) self)
     (/ (+ min max) 2))
   (define (-median self)
     (match-define (uniform-dist min max) self)
     (/ (+ min max) 2))
   (define (-variance self)
     (match-define (uniform-dist min max) self)
     (* (- max min) (- max min) 1/12))
   (define (-denergy self x [dx 1] [dmin 0] [dmax 0])
     (match-define (uniform-dist min max) self)
     (cond [(<= min x max)
            (lazy* (- dmax dmin) (/ (- max min)))]
           [else 0]))]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (define equiv-dist (affine-distx (beta-dist 1 1) min (- max min)))
                 (dist-drift-dist equiv-dist scale-factor))
  |#)


;; ============================================================
;; Additional continuous real distributions

(define-real-dist-struct pareto-dist
  ([scale (>/c 0)]  ;; x_m
   [shape (>/c 0)]) ;; alpha
  #:real
  #:dist-methods
  [(define (-sample self)
     (-invcdf self (random) #f #f))]
  #:real-methods
  [(define (-pdf self x log?)
     (match-define (pareto-dist scale shape) self)
     (define lp
       (if (>= x scale)
           (- (+ (log shape) (* shape (log scale)))
              (* (add1 shape) (log x)))
           -inf.0))
     (if log? lp (exp lp)))
   (define (-cdf self x log? 1-p?)
     (match-define (pareto-dist scale shape) self)
     (define p
       (if (> x scale)
           (- 1.0 (expt (/ scale x) shape))
           0.0))
     (convert-p p log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (pareto-dist scale shape) self)
     (define p* (unconvert-p p log? 1-p?))
     (* scale (expt p* (- (/ shape)))))
   (define (-support self)
     (cons (pareto-dist-scale self) +inf.0))
   (define (-mean self)
     (match-define (pareto-dist scale shape) self)
     (if (<= shape 1)
         +inf.0
         (/ (* scale shape) (sub1 shape))))
   (define (-modes self) (list (pareto-dist-scale self)))
   (define (-variance self)
     (match-define (pareto-dist scale shape) self)
     (if (<= shape 2)
         +inf.0
         (/ (* scale scale shape)
            (* (- shape 1) (- shape 1) (- shape 2)))))]
   #|
   #:conjugate (lambda (data-d data)
                (match data-d
                  [`(uniform-dist 0 _)
                   (pareto-dist
                    (for/fold ([acc -inf.0]) ([x (in-vector data)]) (max x acc))
                    (+ shape (vector-length data)))]
                  [_ #f]))
   |#)
;; DRIFT: FIXME

;; ------------------------------------------------------------

(define-real-dist-struct t-dist
  ([degrees (>/c 0)]
   [mean rational?]
   [scale (>/c 0)])
  #:real
  #:dist-methods
  [(define (-sample self)
     (match-define (t-dist degrees location scale) self)
     (+ location (* scale (std-t-sample degrees))))]
  #:real-methods
  [(define (-pdf self x log?)
     (match-define (t-dist degrees mean scale) self)
     (define sx (/ (- x mean) scale))
     (define logpdf (- (std-t-logpdf degrees sx) (log scale)))
     (if log? logpdf (exp logpdf)))
   (define (-cdf self x log? 1-p?)
     (match-define (t-dist degrees mean scale) self)
     (define sx (/ (- x mean) scale))
     (define p (std-t-cdf degrees sx))
     (convert-p p log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (t-dist degrees mean scale) self)
     (define p* (unconvert-p p log? 1-p?))
     (error 't-inv-cdf "unimplemented"))
   (define (-support self) '(-inf.0 . +inf.0))
   (define (-mean self)
     (match-define (t-dist degrees mean scale) self)
     (if (> degrees 1) 0 #f))
   (define (-median self) 0)
   (define (-variance self) #f)]
  #|
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* scale scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor)))
  |#)

(define (std-t-logpdf degrees x)
  (define logprefix (std-t-logpdf-prefix degrees))
  (- logprefix
     (* (+ 1.0 degrees) 0.5
        (log (+ 1.0 (/ (* x x) degrees))))))

;; FIXME: memoize/special-case?
(define (std-t-logpdf-prefix degrees)
  (+ (m:log-gamma (* 0.5 (+ 1.0 degrees)))
     (* -0.5 (log degrees))
     (* -0.5 (log pi))
     (* -1.0 (m:log-gamma (* degrees 0.5)))))

(define (std-t-cdf degrees x)
  (cond [#f ;(= degrees 1)
         (+ 0.5 (* (/ pi) (atan x)))]
        [#f ;(= degrees 2)
         (+ 0.5 (/ x (* 2 (sqrt (+ 2 (* x x))))))]
        [else
         (define (gen-cdf x)
           (cond [(> x 0)
                  (define x* (/ degrees (+ (* x x) degrees)))
                  (define a (* degrees 0.5))
                  (define b 0.5)
                  (- 1.0 (* 0.5 (m:beta-inc a b x* #f #t)))]
                 [(< x 0)
                  (- 1.0 (gen-cdf (- x)))]
                 [(= x 0)
                  0.5]))
         (gen-cdf x)]))

(define (std-t-sample degrees)
  (define u (- (* 2.0 (random)) 1))
  (define v (- (* 2.0 (random)) 1))
  (define w (+ (* u u) (* v v)))
  (cond [(> w 1)
         (std-t-sample degrees)]
        [else
         (define c^2 (/ (* u u) w))
         (define r^2 (* degrees (+ -1.0 (expt w (/ -2.0 degrees)))))
         (define x-abs (sqrt (* r^2 c^2)))
         (if (zero? (random 2))
             x-abs
             (- x-abs))]))

;; ------------------------------------------------------------

(define-real-dist-struct samples-dist
  ([samples vector? #;(vectorof real?) #:exact])
  #:real
  #:guard (lambda (samples _name)
            (vector->immutable-vector (vector-sort samples <)))
  #:dist-methods
  [(define (-sample self)
     (define samples (samples-dist-samples self))
     (vector-ref samples (random (vector-length samples))))]
  #:real-methods
  [(define (-pdf self x log?) ;; linear, suboptimal
     (define samples (samples-dist-samples self))
     (/ (for/sum ([v (in-vector samples)] #:when (= v x)) 1)
        (vector-length samples)))
   (define (-cdf self x log? 1-p?) ;; linear, suboptimal
     (define samples (samples-dist-samples self))
     (/ (for/sum ([v (in-vector samples)] #:when (<= v x)) 1)
        (vector-length samples)))
   (define (-invcdf self p0 log? 1-p?)
     (define p (unconvert-p p0 log? 1-p?))
     (define samples (samples-dist-samples self))
     (vector-ref samples (* p (vector-length samples))))
   (define (-mean self)
     (define samples (samples-dist-samples self))
     (/ (for/sum ([v (in-vector samples)]) v)
        (vector-length samples)))
   (define (-median self)
     (define samples (samples-dist-samples self))
     (vector-ref samples (quotient (vector-length samples) 2)))
   #; (define (-variance self) _)
   ])


;; ============================================================
;; Discrete distributions

(define-real-dist-struct bernoulli-dist
  ([p (real-in 0 1)])
  #:nat
  #:dist-methods
  [(define (-sample self)
     (define prob (bernoulli-dist-p self))
     (if (<= (random) prob) 1 0))]
  #:real-methods
  [(define (-pdf self v log?)
     (define prob (bernoulli-dist-p self))
     (define p
       (cond [(= v 0) (- 1 prob)]
             [(= v 1) prob]
             [else 0]))
     (convert-p p log? #f))
   (define (-cdf self v log? 1-p?)
     (define prob (bernoulli-dist-p self))
     (define p
       (cond [(< v 0) 0]
             [(< v 1) (- 1 prob)]
             [else 1]))
     (convert-p p log? 1-p?))
   (define (-invcdf self p0 log? 1-p?)
     (define prob (bernoulli-dist-p self))
     (define p (unconvert-p p0 log? 1-p?))
     (cond [(< p prob) 1] [else 0]))
   (define (-support self) '(0 . 1))
   (define (-mean self) (bernoulli-dist-p self))
   (define (-median self)
     (match-define (bernoulli-dist p) self)
     (cond [(> p 0.5) 1] [(= p 0.5) 1/2] [else 0]))
   (define (-modes self)
     (match-define (bernoulli-dist p) self)
     (cond [(> p 1/2) '(1)] [(= p 1/2) '(0 1)] [else '(0)]))
   (define (-variance self)
     (match-define (bernoulli-dist p) self)
     (* p (- 1 p)))]
  #:methods gen:enum-dist
  [(define (-enum self) 2)]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (define (squash x) (/ x (+ 1 x))) ;; R+ -> [0,1]
                 ;; FIXME: is this a good thing to do???
                 (define driftiness (squash scale-factor))
                 (bernoulli-dist (cond [(= value 1) (- 1 driftiness)]
                                       [(= value 0) driftiness])))
  #:drift1 (lambda (value scale-factor) (cons (- 1 value) 0))
  |#)

;; ------------------------------------------------------------

(define-real-dist-struct categorical-dist
  ([weights (vectorof (>=/c 0))])
  #:nat
  #:guard (lambda (weights _name)
            (validate/normalize-weights 'categorical-dist weights))
  #:dist-methods
  [(define (-sample self)
     (define weights (categorical-dist-weights self))
     (categorical-inv-cdf weights (random)))]
  #:real-methods
  [(define (-pdf self x0 log?)
     (define weights (categorical-dist-weights self))
     (define x (and (integer? x0) (inexact->exact x0)))
     (cond [(and (exact-nonnegative-integer? x)
                 (< x (vector-ref weights)))
            (vector-ref weights x)]
           [else (if log? -inf.0 0)]))
   (define (-cdf self k log? 1-p?)
     (define probs (categorical-dist-weights self))
     (define p (for/sum ([i (in-range (add1 k))] [prob (in-vector probs)]) prob))
     (convert-p p log? 1-p?))
   (define (-invcdf self p0 log? 1-p?)
     (define probs (categorical-dist-weights self))
     (define p (unconvert-p p0 log? 1-p?))
     (categorical-inv-cdf probs p))
   (define (-support self)
     (define weights (categorical-dist-weights self))
     ;; integer-range
     (cons 0 (sub1 (vector-length weights))))
   (define (-mean self)
     (define weights (categorical-dist-weights self))
     (for/sum ([i (in-naturals)] [w (in-vector weights)]) (* i w)))
   (define (-modes self)
     (define weights (categorical-dist-weights self))
     (let-values ([(best best-w)
                   (for/fold ([best null] [best-w -inf.0])
                             ([i (in-naturals)] [w (in-vector weights)])
                     (cond [(> w best-w)
                            (values (list i) w)]
                           [(= w best-w)
                            (values (cons i best) best-w)]
                           [else (values best best-w)]))])
       (reverse best)))]
  #:methods gen:enum-dist
  [(define (-enum self)
     (define weights (categorical-dist-weights self))
     (vector-length weights))])

;; -- Assume weights are nonnegative, normalized.

(define (categorical-inv-cdf probs p)
  (let loop ([i 0] [p p])
    (cond [(>= i (vector-length probs))
           (error 'categorical-dist:inv-cdf "out of values")]
          [(< p (vector-ref probs i))
           i]
          [else
           (loop (add1 i) (- p (vector-ref probs i)))])))


;; ------------------------------------------------------------

#|

(define-dist-type multinomial-dist
  ([n exact-nonnegative-integer?]
   [weights (vectorof (>=/c 0))])
  #:counting
  #:pdf multinomial-dist
  #:sample multinomial-sample
  #:guard (lambda (n weights _name)
            (values n (validate/normalize-weights 'multinomial-dist weights)))
  ;; FIXME: drift by computing new multinomial-dist ??
  ;;  eg, scale-weighted average of prior weights and derived from current value?
  #:drift1 (lambda (value scale-factor) (multinomial-drift n weights value scale-factor)))

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


;; ============================================================
;; Other distributions

;; FIXME: doesn't belong in univariate, exactly, but doesn't use matrices
;; FIXME: flag for symmetric alphas, can sample w/ fewer gamma samplings
(define-dist-type dirichlet-dist
  ([alpha (vectorof (>/c 0))])
  #:lebesgue
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

(define-syntax-rule (define-memoize1 (fun arg) . body)
  (begin (define memo-table (make-weak-hash))
         (define (fun arg)
           (cond [(hash-ref memo-table arg #f)
                  => values]
                 [else
                  (define r (let () . body))
                  (hash-set! memo-table arg r)
                  r]))))

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
;; Attic

#;
(define (discrete-normal-dist mean stddev a b)
  (discretize-distx (clip-distx (normal mean stddev) (- a 0.5) (+ b 0.5))))

#;
(define (mult-exp-normal-dist x scale)
  ;; Want to multiply by factor log-normally distributed, with stddev proportional
  ;; to scale. For log-normal, variance = (exp[s^2] - 1)(exp[s^2]).
  ;; Let's approximate as exp[2s^2] - 1. So we want
  ;; exp[2s^2] - 1 ~= scale^2, so
  ;; s ~= sqrt(log(scale^2 + 1))    -- dropped a factor of 2, nuisance
  (define s (sqrt (log (+ 1 (* scale scale)))))
  (affine-distx (exp-distx (normal-dist 0 (sqrt (log (+ 1 (* scale scale)))))) x 0))

|#

;; ============================================================
;; Utils

(define (validate/normalize-weights who weights)
  (unless (and (vector? weights)
               (for/and ([w (in-vector weights)])
                 (and (rational? w) (>= w 0))))
    (raise-argument-error who "(vectorof (>=/c 0))" weights))
  (define weight-sum (for/sum ([w (in-vector weights)]) w))
  (unless (> weight-sum 0)
    (error who "weights sum to zero\n  weights: ~e" weights))
  (if (= weight-sum 1)
      (vector->immutable-vector weights)
      (vector-map (lambda (w) (/ w weight-sum)) weights)))

(define (filter-modes f ms)
  (define-values (best best-p)
    (for/fold ([best null] [best-p -inf.0])
        ([m (in-list ms)])
      (define m-p (f m))
      (cond [(> m-p best-p)
             (values (list m) m-p)]
            [(= m-p best-p)
             (values (cons m best) best-p)]
            [else
             (values best best-p)])))
  (reverse best))

(define (vector-sum v) (for/sum ([x (in-vector v)]) x))

;; use in pdf functions instead of raising type (or other) error
(define (impossible log? who reason)
  ;; FIXME: may be useful to log occurrences of these
  (if log? -inf.0 0))

(define (digamma x) (m:psi0 x))
