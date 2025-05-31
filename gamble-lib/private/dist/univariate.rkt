;; Copyright 2014-2025 Ryan Culpepper
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
         "define.rkt"
         (submod "util.rkt" math)
         (submod "util.rkt" density)
         (submod "util.rkt" weights))
(provide (all-defined-out))

;; ============================================================
;; Continuous real distributions from math library

(define-dist-struct beta-dist
  ([a nonnegative-rational? inexact]
   [b nonnegative-rational? inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (beta-dist a b) self)
     (flvector-ref (m:flbeta-sample a b 1) 0))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (beta-dist a b) self)
     (m:flbeta-pdf a b (inexact x) log?))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (beta-dist a b) self)
     (m:flbeta-cdf a b (inexact x) log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (beta-dist a b) self)
     (m:flbeta-cdf a b (inexact p) log? 1-p?))
   (define (-real-support self) '(0 . 1))
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
        (lazy* (+ da db) (- (digamma (+ a b))))))
   (define (-conjugate self data-d data)
     (match-define (beta-dist a b) self)
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
   (define (-drift-dist self value scale-factor)
     (match-define (beta-dist a b) self)
     ;; mode = α / (α + β), peakedness = α + β = S (our choice)
     ;; So if we want dist peaked at x:
     ;;   α = S * x
     ;;   β = S - α = S * (1 - x)
     (define S 10) ;; "peakedness" parameter
     (beta-dist (* S value) (* S (- 1 value))))])

(define-dist-struct cauchy-dist
  ([mode rational? inexact]
   [scale positive-rational? inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (cauchy-dist mode scale) self)
     (flvector-ref (m:flcauchy-sample mode scale 1) 0))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (cauchy-dist mode scale) self)
     (m:flcauchy-pdf mode scale (inexact x) log?))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (cauchy-dist mode scale) self)
     (m:flcauchy-cdf mode scale (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (cauchy-dist mode scale) self)
     (m:flcauchy-inv-cdf mode scale (inexact x) log? 1-p?))
   (define (-real-support self) '(-inf.0 . +inf.0))
   (define (-mean self) +nan.0)
   (define (-modes self) (list (cauchy-dist-mode self)))
   (define (-variance self) +nan.0)
   (define (-denergy self x [dx 1] [dm 0] [ds 0])
     (match-define (cauchy-dist mode scale) self)
     (define x-m (- x mode))
     (+ (lazy* ds (/ scale))
        (* (/ (* 2 scale x-m) (+ (* scale scale) (* x-m x-m)))
           (- (/ (- dx dm) scale)
              (lazy* ds (/ x-m scale scale))))))
   (define (-drift-dist self value scale-factor)
     (match-define (cauchy-dist mode scale) self)
     (normal-dist value (* scale scale-factor)))
   #;
   (define (-drift1 self value scale-factor)
     (match-define (cauchy-dist mode scale) self)
     (drift:add-normal value (* scale scale-factor)))])

(define-dist-struct exponential-dist
  ([mean positive-rational? inexact])
  ;; λ = 1/mean
  #:methods gen:dist
  [(define (-sample self)
     (match-define (exponential-dist mean) self)
     (flvector-ref (m:flexponential-sample mean 1) 0))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (exponential-dist mean) self)
     (m:flexponential-pdf mean (inexact x) log?))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (exponential-dist mean) self)
     (m:flexponential-cdf mean (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (exponential-dist mean) self)
     (m:flexponential-inv-cdf mean (inexact x) log? 1-p?))
   (define (-real-support self) '(0 . +inf.0))
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
        (* dx /mean)))
   #;
   (define (-drift-dist self value scale-factor)
     (mult-exp-normal-dist value (* mean scale-factor)))
   #;
   (define (-drift1 self value scale-factor)
     (drift:mult-exp-normal value (* mean scale-factor)))])

(define-dist-struct gamma-dist
  ([shape positive-rational? inexact]
   [scale positive-rational? inexact])
  ;; k = shape, θ = scale
  #:methods gen:dist
  [(define (-sample self)
     (match-define (gamma-dist shape scale) self)
     (flvector-ref (m:flgamma-sample shape scale 1) 0))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (gamma-dist shape scale) self)
     (m:flgamma-pdf shape scale (inexact x) log?))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (gamma-dist shape scale) self)
     (m:flgamma-cdf shape scale (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (gamma-dist shape scale) self)
     (m:flgamma-inv-cdf shape scale (inexact x) log? 1-p?))
   (define (-real-support self) '(0 . +inf.0)) ;; (0, inf)
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
        (lazy* dθ (- (/ k θ) (/ x (* θ θ))))))
   (define (-conjugate self data-d data)
     (match-define (gamma-dist shape scale) self)
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
   #;
   (define (-drift-dist self value scale-factor)
     (match-define (gamma-dist shape scale) self)
     (mult-exp-normal-dist value (* scale (sqrt shape) scale-factor)))
   #;
   (define (-drift1 self value scale-factor)
     (match-define (gamma-dist shape scale) self)
     (drift:mult-exp-normal value (* scale (sqrt shape) scale-factor)))])

(define-dist-struct logistic-dist
  ([mean rational? inexact]
   [scale positive-rational? inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (logistic-dist mean scale) self)
     (flvector-ref (m:fllogistic-sample mean scale 1) 0))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (logistic-dist mean scale) self)
     (m:fllogistic-pdf mean scale (inexact x) log?))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (logistic-dist mean scale) self)
     (m:fllogistic-cdf mean scale (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (logistic-dist mean scale) self)
     (m:fllogistic-inv-cdf mean scale (inexact x) log? 1-p?))
   (define (-real-support self) '(-inf.0 . +inf.0))
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
        (* 2 (/ (+ 1 B)) B (- A))))
   (define (-drift-dist self value scale-factor)
     (match-define (logistic-dist mean scale) self)
     (normal-dist value (* scale scale-factor)))
   #;
   (define (-drift1 self value scale-factor)
     (match-define (logistic-dist mean scale) self)
     (drift:add-normal value (* scale scale-factor)))])

(define-dist-struct normal-dist
  ([mean rational? inexact]
   [scale positive-rational? inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (normal-dist mean scale) self)
     (flvector-ref (m:flnormal-sample mean scale 1) 0))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (normal-dist mean scale) self)
     (m:flnormal-pdf mean scale (inexact x) log?))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (normal-dist mean scale) self)
     (m:flnormal-cdf mean scale (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (normal-dist mean scale) self)
     (m:flnormal-inv-cdf mean scale (inexact x) log? 1-p?))
   (define (-support self) '(-inf.0 . +inf.0))
   (define (-mean self) (normal-dist-mean self))
   (define (-median self) (normal-dist-mean self))
   (define (-modes self) (list (normal-dist-mean self)))
   (define (-variance self)
     (match-define (normal-dist mean scale) self)
     (* scale scale))
   (define (-denergy self x [dx 1] [dμ 0] [dσ 0])
     (match-define (normal-dist μ σ) self)
     (define x-μ (- x μ))
     (+ (lazy* dσ (- (/ σ) (/ (* x-μ x-μ) (* σ σ σ))))
        (lazy* (- dx dμ)
               (/ x-μ (* σ σ)))))
   (define (-conjugate self data-d data)
     (match-define (normal-dist mean scale) self)
     (match data-d
       [`(normal-dist _ ,data-scale)
        (normal-dist (/ (+ (/ mean (sqr scale))
                           (/ (vector-sum data)
                              (sqr data-scale)))
                        (+ (/ (sqr scale))
                           (/ (vector-length data)
                              (sqr data-scale))))
                     (sqrt
                      (/ (+ (/ (sqr scale))
                            (/ (vector-length data)
                               (sqr data-scale))))))]
       [_ #f]))
   (define (-drift-dist self value scale-factor)
     (match-define (normal-dist mean scale) self)
     (normal-dist value (* scale scale-factor)))
   #;
   (define (-drift1 self value scale-factor)
     (match-define (normal-dist mean scale) self)
     (drift:add-normal value (* stddev scale-factor)))])

(define-dist-struct uniform-dist
  ([lo rational? inexact]
   [hi rational? inexact])
  #:guard (lambda (lo hi)
            (unless (< lo hi)
              (error 'uniform-dist
                     (string-append
                      "invalid range, lower bound is not less than upper bound"
                      "\n  lower: ~e\n  upper: ~e")
                     lo hi))
            (values lo hi))
  #:methods gen:dist
  [(define (-sample self)
     (match-define (uniform-dist lo hi) self)
     (+ lo (* (- hi lo) (random))))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (uniform-dist lo hi) self)
     (m:fluniform-pdf lo hi (inexact x) log?))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (uniform-dist lo hi) self)
     (m:fluniform-cdf lo hi (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (uniform-dist lo hi) self)
     (m:fluniform-inv-cdf lo hi (inexact x) log? 1-p?))
   (define (-real-support self)
     (match-define (uniform-dist lo hi) self)
     (cons lo hi))
   (define (-mean self)
     (match-define (uniform-dist lo hi) self)
     (/ (+ lo hi) 2))
   (define (-median self)
     (match-define (uniform-dist lo hi) self)
     (/ (+ lo hi) 2))
   (define (-variance self)
     (match-define (uniform-dist lo hi) self)
     (let ([w (- hi lo)]) (* w w (inexact 1/12))))
   (define (-denergy self x [dx 1] [dlo 0] [dhi 0])
     (match-define (uniform-dist lo hi) self)
     (cond [(<= lo x hi)
            (lazy* (- dhi dlo) (/ (- hi lo)))]
           [else 0]))
   #;
   (define (-drift-dist self value scale-factor)
     (match-define (uniform-dist lo hi) self)
     (define equiv-dist (affine-distx (beta-dist 1 1) lo (- hi lo)))
     (dist-drift-dist equiv-dist scale-factor))])

;; ============================================================
;; Additional continuous real distributions

(define-dist-struct pareto-dist
  ([scale positive-rational? inexact]  ;; x_m
   [shape positive-rational? inexact]) ;; alpha
  #:methods gen:dist
  [(define (-sample self)
     (-invcdf self (random) #f #f))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (pareto-dist scale shape) self)
     (define lp
       (if (>= x scale)
           (- (+ (log shape) (* shape (log scale)))
              (* (add1 shape) (log x)))
           -inf.0))
     (if log? lp (exp lp)))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
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
   (define (-real-support self)
     (cons (pareto-dist-scale self) +inf.0))
   (define (-mean self)
     (match-define (pareto-dist scale shape) self)
     (if (<= shape 1)
         +inf.0
         (/ (* scale shape) (sub1 shape))))
   (define (-modes self)
     (list (pareto-dist-scale self)))
   (define (-variance self)
     (match-define (pareto-dist scale shape) self)
     (if (<= shape 2)
         +inf.0
         (/ (* scale scale shape)
            (* (- shape 1) (- shape 1) (- shape 2)))))
   (define (-conjugate self data-d data)
     (match-define (pareto-dist scale shape) self)
     (match data-d
       [`(uniform-dist 0 _)
        (pareto-dist
         (for/fold ([acc -inf.0]) ([x (in-vector data)]) (max x acc))
         (+ shape (vector-length data)))]
       [_ #f]))])

(define-dist-struct t-dist
  ([degrees positive-rational? inexact]
   [mean rational? inexact]
   [scale positive-rational? inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (t-dist degrees mean scale) self)
     (+ mean (* scale (std-t-sample degrees))))]
  #:methods gen:continuous-dist
  [(define (-pdf self x log?)
     (match-define (t-dist degrees mean scale) self)
     (define sx (/ (- x mean) scale))
     (define logpdf (- (std-t-logpdf degrees sx) (log scale)))
     (if log? logpdf (exp logpdf)))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (t-dist degrees mean scale) self)
     (define sx (/ (- x mean) scale))
     (define p (std-t-cdf degrees sx))
     (convert-p p log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (t-dist degrees mean scale) self)
     (define p* (unconvert-p p log? 1-p?))
     (error 't-inv-cdf "unimplemented"))
   (define (-real-support self)
     '(-inf.0 . +inf.0))
   (define (-mean self)
     (match-define (t-dist degrees mean scale) self)
     (if (> degrees 1) 0 #f))
   (define (-median self) 0)
   (define (-variance self) #f)
   (define (-drift-dist self value scale-factor)
     (match-define (t-dist degrees mean scale) self)
     (normal-dist value (* scale scale-factor)))
   #;
   (define (-drift1 self value scale-factor)
     (match-define (t-dist degrees mean scale) self)
     (drift:add-normal value (* scale scale-factor)))])

(define (std-t-logpdf degrees x)
  (define logprefix (std-t-logpdf-prefix degrees))
  (- logprefix
     (* (+ 1.0 degrees) 0.5
        (log (+ 1.0 (/ (* x x) degrees))))))

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
        [else (std-t-cdf* degrees x)]))
(define (std-t-cdf* degrees x)
  (cond [(> x 0)
         (define x* (/ degrees (+ (* x x) degrees)))
         (define a (* degrees 0.5))
         (define b 0.5)
         (- 1.0 (* 0.5 (m:beta-inc a b x* #f #t)))]
        [(< x 0)
         (- 1.0 (std-t-cdf* degrees (- x)))]
        [(= x 0)
         0.5]))

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
;; Discrete integer distributions from math library (infinite)

(define-dist-struct binomial-dist
  ([n exact-nonnegative-integer?]
   [p (real-in 0 1) inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (binomial-dist n p) self)
     (exact (flvector-ref (m:flbinomial-sample (inexact n) p 1) 0)))]
  #:methods gen:integer-dist
  [(define (-pmf self x log?)
     (match-define (binomial-dist n p) self)
     (m:flbinomial-pdf (inexact n) p (inexact x) #f))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (binomial-dist n p) self)
     (m:flbinomial-cdf (inexact n) p (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (binomial-dist n p) self)
     (exact (m:flbinomial-inv-cdf (inexact n) p (inexact x) log? 1-p?)))
   (define (-support self)
     (match-define (binomial-dist n _) self)
     (cons 0 n))
   (define (-mean self)
     (match-define (binomial-dist n p) self)
     (* n p))
   (define (-modes self)
     (match-define (binomial-dist n p) self)
     (filter-modes (lambda (x) (m:flbinomial-pdf (inexact n) p x #f))
                   (let ([m (exact (floor (* (+ n 1) p)))])
                     (list m (sub1 m)))))
   (define (-variance self)
     (match-define (binomial-dist n p) self)
     (* n p (- 1 p)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (match-define (binomial-dist n _) self)
     (in-range 0 (add1 n)))]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt (* n p (- 1 p))))))
  #:drift1 (lambda (value scale-factor)
             (drift:add-discrete-normal value (* scale-factor (sqrt (* n p (- 1 p)))) 0 n))
  |#)

(define-dist-struct geometric-dist
  ([p (real-in 0 1) inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (geometric-dist p) self)
     (exact (flvector-ref (m:flgeometric-sample p 1) 0)))]
  #:methods gen:integer-dist
  [(define (-pmf self x log?)
     (match-define (geometric-dist p) self)
     (m:flgeometric-pdf p (inexact x) #f))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (geometric-dist p) self)
     (m:flgeometric-cdf p (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (geometric-dist p) self)
     (exact (m:flgeometric-inv-cdf p (inexact x) log? 1-p?)))
   (define (-support self)
     '(0 . +inf.0))
   (define (-mean self)
     (match-define (geometric-dist p) self)
     (/ (- 1 p) p))
   (define (-modes self)
     '(0))
   (define (-variance self)
     (match-define (geometric-dist p) self)
     (/ (- 1 p) (* p p)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-naturals))]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt (- 1 p)) (/ p)) 0 +inf.0))
  #:drift1 (lambda (value scale-factor)
             (drift:add-discrete-normal value (* scale-factor (sqrt (- 1 p)) (/ p)) 0 +inf.0))
  |#)

(define-dist-struct poisson-dist
  ([mean positive-rational? inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (poisson-dist mean) self)
     (exact (flvector-ref (m:flpoisson-sample mean 1) 0)))]
  #:methods gen:integer-dist
  [(define (-pmf self x log?)
     (match-define (poisson-dist mean) self)
     (m:flpoisson-pdf mean (inexact x) #f))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (poisson-dist mean) self)
     (m:flpoisson-cdf mean (inexact x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (poisson-dist mean) self)
     (exact (m:flpoisson-inv-cdf mean (inexact x) log? 1-p?)))
   (define (-support self)
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
  #:methods gen:enumerable-dist
  [(define (-enum self)
     (in-naturals))]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (discrete-normal-dist value (* scale-factor (sqrt mean)) 0 +inf.0))
  #:drift1 (lambda (value scale-factor)
             (drift:add-discrete-normal value (* scale-factor (sqrt mean)) 0 +inf.0)))
  |#)


;; ============================================================
;; Discrete integer distributions from math library (finite)

(define-dist-struct bernoulli-dist
  ([p (real-in 0 1) inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (bernoulli-dist p) self)
     (if (<= (random) p) 1 0))]
  #:methods gen:integer-dist
  [(define (-pmf self x log?)
     (match-define (bernoulli-dist p) self)
     (define r
       (cond [(= x 0) (- 1 p)]
             [(= x 1) p]
             [else 0]))
     (convert-p r log? #f))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (bernoulli-dist p) self)
     (define r
       (cond [(< x 0) 0]
             [(< x 1) (- 1 p)]
             [else 1]))
     (convert-p r log? 1-p?))
   (define (-invcdf self r0 log? 1-p?)
     (match-define (bernoulli-dist p) self)
     (define r (unconvert-p r0 log? 1-p?))
     (cond [(< r p) 1] [else 0]))
   (define (-support self) '(0 . 1))
   (define (-mean self) (bernoulli-dist-p self))
   (define (-modes self)
     (match-define (bernoulli-dist p) self)
     (cond [(> p 1/2) '(1)] [(= p 1/2) '(0 1)] [else '(0)]))
   (define (-variance self)
     (match-define (bernoulli-dist p) self)
     (* p (- 1 p)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-range 0 2))]
  #|
  #:drift-dist (lambda (value scale-factor)
                 (define (squash x) (/ x (+ 1 x))) ;; R+ -> [0,1]
                 ;; FIXME: is this a good thing to do???
                 (define driftiness (squash scale-factor))
                 (bernoulli-dist (cond [(= value 1) (- 1 driftiness)]
                                       [(= value 0) driftiness])))
  #:drift1 (lambda (value scale-factor) (cons (- 1 value) 0))
  |#)

(define-dist-struct categorical-dist
  ;; support is {1,...,k}
  ([weights vector? -categorical-guard-weights])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (categorical-dist ws) self)
     (-categorical-inv-cdf 'dist-sample:categorical-dist ws (random)))]
  #:methods gen:integer-dist
  [(define (-pmf self x0 log?)
     (match-define (categorical-dist ws) self)
     (define x (and (integer? x0) (inexact->exact x0)))
     (cond [(and x (<= 1 x (vector-length ws)))
            (convert-p (vector-ref ws (sub1 x)) log?)]
           [else (if log? -inf.0 0)]))]
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (categorical-dist ws) self)
     (cond [(rational? x) (convert-p (-categorical-cdf ws x) log? 1-p?)]
           [else (if log? -inf.0 0)]))
   (define (-invcdf self p0 log? 1-p?)
     (match-define (categorical-dist ws) self)
     (define p (unconvert-p p0 log? 1-p?))
     (-categorical-inv-cdf 'dist-inv-cdf:categorical-dist ws p))
   (define (-support self)
     (match-define (categorical-dist ws) self)
     (cons 1 (vector-length ws)))
   (define (-mean self)
     (match-define (categorical-dist ws) self)
     (for/sum ([i (in-naturals 1)] [w (in-vector ws)]) (* i w)))
   (define (-modes self)
     (match-define (categorical-dist ws) self)
     (define weights (categorical-dist-weights self))
     (let-values ([(best best-w)
                   (for/fold ([best null] [best-w -inf.0])
                             ([i (in-naturals 1)] [w (in-vector weights)])
                     (cond [(> w best-w)
                            (values (list i) w)]
                           [(= w best-w)
                            (values (cons i best) best-w)]
                           [else (values best best-w)]))])
       (reverse best)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (match-define (categorical-dist ws) self)
     (in-range 1 (add1 (vector-length ws))))])

;; categorical:intern-ws : WeakHash[ImmVector => #t]
(define categorical:intern-ws (make-weak-hash))

(define (-categorical-guard-weights in-ws)
  (cond [(hash-ref-key categorical:intern-ws in-ws #f)
         => values]
        [else
         (define ws (normalize-weights 'categorical-dist in-ws))
         (hash-set! categorical:intern-ws ws #t)
         ws]))

;; categorical:ws=>cws : WeakHasheq[ImmVector => ImmVector]
(define categorical:ws=>cws (make-weak-hasheq))

(define (-categorical-cws ws)
  (hash-ref! categorical:ws=>cws ws (lambda () (cumulative-vector ws))))

(define (-categorical-cdf ws x)
  (define k (exact (floor x)))
  (cond [(<= 1 k (vector-length ws))
         (define cws (-categorical-cws ws))
         (vector-ref cws (sub1 k))]
        [(< k 1) 0]
        [else 1]))

(define (-categorical-inv-cdf who ws p)
  (define cws (-categorical-cws ws))
  (or (for/or ([i (in-naturals 1)] [cw (in-vector cws)])
        (and (<= p cw) i))
      (error who "internal error: out of values")))

;; ------------------------------------------------------------

#;
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
