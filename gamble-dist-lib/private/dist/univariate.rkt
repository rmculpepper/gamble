;; Copyright (c) 2014-2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract/base
         racket/match
         racket/math
         racket/generic
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "dist.rkt"
         "dist-define.rkt")
(provide (all-defined-out))

;; Multiply, but short-circuit if first arg evals to 0.
(define-syntax-rule (lazy* a b ...)
  (let ([av a]) (if (zero? av) 0 (* av b ...))))

;; ============================================================
;; Continuous real distributions from math library

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
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt (* n p (- 1 p))))))
  #:drift1 (lambda (value scale-factor)
             (drift:add-discrete-normal value (* scale-factor (sqrt (* n p (- 1 p)))) 0 n)))

(define-fl-dist-type geometric-dist
  ([p (real-in 0 1)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean (/ (- 1 p) p)
  #:modes '(0)
  #:variance (/ (- 1 p) (* p p))
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt (- 1 p)) (/ p)) 0 +inf.0))
  #:drift1 (lambda (value scale-factor)
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
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt mean)) 0 +inf.0))
  #:drift1 (lambda (value scale-factor)
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
  #:drift-dist (lambda (value scale-factor)
                 ;; mode = α / (α + β), peakedness = α + β = S (our choice)
                 ;; So if we want dist peaked at x:
                 ;;   α = S * x
                 ;;   β = S - α = S * (1 - x)
                 (define S 10) ;; "peakedness" parameter
                 (beta-dist (* S value) (* S (- 1 value)))))

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
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* scale scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor))))

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
  #:drift-dist (lambda (value scale-factor)
                 (mult-exp-normal-dist value (* mean scale-factor)))
  #:drift1 (lambda (value scale-factor)
             (drift:mult-exp-normal value (* mean scale-factor))))

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
  #:drift-dist (lambda (value scale-factor)
                 (mult-exp-normal-dist value (* scale (sqrt shape) scale-factor)))
  #:drift1 (lambda (value scale-factor)
             (drift:mult-exp-normal value (* scale (sqrt shape) scale-factor))))

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
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* scale scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor))))

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
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* stddev scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* stddev scale-factor))))

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
  #:drift-dist (lambda (value scale-factor)
                 (define equiv-dist (affine-distx (beta-dist 1 1) min (- max min)))
                 (dist-drift-dist equiv-dist scale-factor)))


;; ============================================================
;; Additional continuous real distributions

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

(define-fl-dist-type t-dist
  ([degrees (>/c 0)]
   [mean rational?]
   [scale (>/c 0)])
  #:real
  #:support #s(real-range -inf.0 +inf.0)
  #:mean (if (> degrees 1) 0 #f)
  #:median 0
  #:variance #f
  #:drift-dist (lambda (value scale-factor) (normal-dist value (* scale scale-factor)))
  #:drift1 (lambda (value scale-factor) (drift:add-normal value (* scale scale-factor))))

(define (m:flt-pdf degrees mean scale x log?)
  (define sx (/ (- x mean) scale))
  (define logpdf (- (std-t-logpdf degrees sx) (log scale)))
  (if log? logpdf (exp logpdf)))

(define (m:flt-cdf degrees mean scale x log? 1-p?)
  (define sx (/ (- x mean) scale))
  (define p (std-t-cdf degrees sx))
  (convert-p p log? 1-p?))

(define (m:flt-inv-cdf degrees location scale p log? 1-p?)
  (define p* (unconvert-p p log? 1-p?))
  (error 't-inv-cdf "unimplemented"))
(define (m:flt-sample degrees location scale n)
  (define flv (make-flvector n))
  (for ([i (in-range n)])
    (flvector-set! flv i (+ location (* scale (std-t-sample degrees)))))
  flv)

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


;; ============================================================
;; Discrete distributions

(define-dist-type bernoulli-dist
  ([p (real-in 0 1)])
  #:counting
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
  #:drift-dist (lambda (value scale-factor)
                 (define (squash x) (/ x (+ 1 x))) ;; R+ -> [0,1]
                 ;; FIXME: is this a good thing to do???
                 (define driftiness (squash scale-factor))
                 (bernoulli-dist (cond [(= value 1) (- 1 driftiness)]
                                       [(= value 0) driftiness])))
  #:drift1 (lambda (value scale-factor) (cons (- 1 value) 0)))

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

(define-dist-type categorical-dist
  ([weights (vectorof (>=/c 0))])
  #:counting
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
