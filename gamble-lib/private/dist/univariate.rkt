;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract/base
         racket/match
         racket/math
         racket/flonum
         racket/vector
         racket/generic
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         (prefix-in m: (only-in math/flonum flbinomial fllog-binomial))
         "base.rkt"
         (submod "util.rkt" define)
         (submod "util.rkt" math)
         (submod "util.rkt" search)
         (submod "util.rkt" weights))
(provide (all-defined-out))

;; ============================================================
;; Continuous real distributions from math library

(define-dist-struct beta-dist
  ([a positive-rational? fl]
   [b positive-rational? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (beta-dist a b) self)
     (flvector-ref (m:flbeta-sample a b 1) 0))
   (define (-pdf self x log?)
     (match-define (beta-dist a b) self)
     (m:flbeta-pdf a b (fl x) log?))]
  #:methods gen:conjugate-dist
  [(define (-conjugate self xdistp xs)
     (match-define (beta-dist a b) self)
     (match xdistp
       [`(bernoulli-dist _)
        (define s (vector-sum xs))
        (beta-dist (+ a s) (+ b (- (vector-length xs) s)))]
       [`(binomial-dist ,n _)
        (define s (vector-sum xs))
        (beta-dist (+ a s) (+ b (- (* n (vector-length xs)) s)))]
       [`(boolean-dist _)
        (define s (for/sum ([e (in-vector xs)] #:when e) 1))
        (beta-dist (+ a s) (+ b (- (vector-length xs) s)))]
       [`(geometric-dist _)
        (define s (vector-length xs))
        (define f (vector-sum xs))
        (beta-dist (+ a s) (+ b f))]
       [_ #f]))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (beta-dist a b) self)
     (m:flbeta-cdf a b (fl x) log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (beta-dist a b) self)
     (m:flbeta-inv-cdf a b (fl p) log? 1-p?))
   (define (-support self)
     (real-range 0.0 1.0))
   (define (-mean self)
     (match self [(beta-dist a b) (/ a (+ a b))]))
   (define (-modes self)
     (match-define (beta-dist a b) self)
     (if (and (> a 1) (> b 1))
         (list (/ (+ a -1) (+ a b -2)))
         '()))
   (define (-variance self)
     (match-define (beta-dist a b) self)
     (/ (* a b) (* (+ a b) (+ a b) (+ a b 1))))])

(define-dist-struct cauchy-dist
  ([mode rational? fl]
   [scale positive-rational? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (cauchy-dist mode scale) self)
     (flvector-ref (m:flcauchy-sample mode scale 1) 0))
   (define (-pdf self x log?)
     (match-define (cauchy-dist mode scale) self)
     (m:flcauchy-pdf mode scale (fl x) log?))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (cauchy-dist mode scale) self)
     (m:flcauchy-cdf mode scale (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (cauchy-dist mode scale) self)
     (m:flcauchy-inv-cdf mode scale (fl x) log? 1-p?))
   (define (-support self)
     (real-range -inf.0 +inf.0))
   (define (-mean self) #|undefined|# #f)
   (define (-modes self) (list (cauchy-dist-mode self)))
   (define (-variance self) #|undefined|# #f)])

(define-dist-struct exponential-dist
  ([mean positive-rational? fl])
  ;; λ = 1/mean
  #:methods gen:dist
  [(define (-sample self)
     (match-define (exponential-dist mean) self)
     (flvector-ref (m:flexponential-sample mean 1) 0))
   (define (-pdf self x log?)
     (match-define (exponential-dist mean) self)
     (m:flexponential-pdf mean (fl x) log?))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (exponential-dist mean) self)
     (m:flexponential-cdf mean (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (exponential-dist mean) self)
     (m:flexponential-inv-cdf mean (fl x) log? 1-p?))
   (define (-support self)
     (real-range 0.0 +inf.0))
   (define (-mean self)
     (exponential-dist-mean self))
   (define (-median self)
     (* (exponential-dist-mean self) (log 2.0)))
   (define (-modes self) '(0.0))
   (define (-variance self)
     (expt (exponential-dist-mean self) 2))])

(define-dist-struct gamma-dist
  ([shape positive-rational? fl]
   [scale positive-rational? fl])
  ;; k = shape, θ = scale
  #:methods gen:dist
  [(define (-sample self)
     (match-define (gamma-dist shape scale) self)
     (flvector-ref (m:flgamma-sample shape scale 1) 0))
   (define (-pdf self x log?)
     (match-define (gamma-dist shape scale) self)
     (m:flgamma-pdf shape scale (fl x) log?))]
  #:methods gen:conjugate-dist
  [(define (-conjugate self xdistp xs)
     (match-define (gamma-dist shape scale) self)
     (match xdistp
       [`(poisson-dist _)
        (gamma-dist (+ shape (vector-sum xs))
                    (/ scale (add1 (* (vector-length xs) scale))))]
       [`(exponential-dist _)
        (gamma-dist (+ shape (vector-length xs))
                    (/ (+ (/ scale) (vector-sum xs))))]
       [`(gamma-dist ,xs-shape _)
        (gamma-dist (+ shape (* xs-shape (vector-length xs)))
                    (/ (+ (/ scale) (vector-sum xs))))]
       [`(inverse-gamma-dist ,xs-shape _)
        (gamma-dist (+ shape (* (vector-length xs) xs-shape))
                    (/ (+ (/ scale) (for/sum ([x (in-vector xs)]) (/ x)))))]
       [`(normal-dist ,xs-mean _)
        (gamma-dist (+ shape (/ (vector-length xs) 2))
                    (/ (+ (/ scale)
                          (* 1/2 (for/sum ([x (in-vector xs)])
                                   (sqr (- x xs-mean)))))))]
       [_ #f]))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (gamma-dist shape scale) self)
     (m:flgamma-cdf shape scale (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (gamma-dist shape scale) self)
     (m:flgamma-inv-cdf shape scale (fl x) log? 1-p?))
   (define (-support self)
     (real-range 0.0 +inf.0))
   (define (-mean self)
     (match-define (gamma-dist shape scale) self)
     (* shape scale))
   (define (-modes self)
     (match-define (gamma-dist shape scale) self)
     (if (> shape 1) (list (* (- shape 1.0) scale)) '(0.0)))
   (define (-variance self)
     (match-define (gamma-dist shape scale) self)
     (* shape scale scale))])

(define-dist-struct logistic-dist
  ([mean rational? fl]
   [scale positive-rational? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (logistic-dist mean scale) self)
     (flvector-ref (m:fllogistic-sample mean scale 1) 0))
   (define (-pdf self x log?)
     (match-define (logistic-dist mean scale) self)
     (m:fllogistic-pdf mean scale (fl x) log?))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (logistic-dist mean scale) self)
     (m:fllogistic-cdf mean scale (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (logistic-dist mean scale) self)
     (m:fllogistic-inv-cdf mean scale (fl x) log? 1-p?))
   (define (-support self)
     (real-range -inf.0 +inf.0))
   (define (-mean self) (logistic-dist-mean self))
   (define (-median self) (logistic-dist-mean self))
   (define (-modes self) (list (logistic-dist-mean self)))
   (define (-variance self)
     (match-define (logistic-dist mean scale) self)
     (/ (* scale scale pi pi) 3.0))])

(define-dist-struct normal-dist
  ([mean rational? fl]
   [stddev positive-rational? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (normal-dist mean scale) self)
     (flvector-ref (m:flnormal-sample mean scale 1) 0))
   (define (-pdf self x log?)
     (match-define (normal-dist mean scale) self)
     (m:flnormal-pdf mean scale (fl x) log?))]
  #:methods gen:conjugate-dist
  [(define (-conjugate self xdistp xs)
     (match-define (normal-dist mean scale) self)
     (define var (sqr scale))
     (match xdistp
       [`(normal-dist _ ,xs-scale)
        (define xs-var (sqr xs-scale))
        (normal-dist (/ (+ (/ mean var)
                           (/ (vector-sum xs) xs-var))
                        (+ (/ var)
                           (/ (vector-length xs) xs-var)))
                     (sqrt
                      (/ (+ (/ var)
                            (/ (vector-length xs) xs-var)))))]
       [_ #f]))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (normal-dist mean scale) self)
     (m:flnormal-cdf mean scale (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (normal-dist mean scale) self)
     (m:flnormal-inv-cdf mean scale (fl x) log? 1-p?))
   (define (-support self)
     (real-range -inf.0 +inf.0))
   (define (-mean self) (normal-dist-mean self))
   (define (-median self) (normal-dist-mean self))
   (define (-modes self) (list (normal-dist-mean self)))
   (define (-variance self) (sqr (normal-dist-stddev self)))])

(define-dist-struct uniform-dist
  ([lo rational? fl]
   [hi rational? fl])
  #:guard (lambda (lo hi)
            (unless (< lo hi)
              (error 'uniform-dist "invalid range\n  range: (~e, ~e)" lo hi))
            (values lo hi))
  #:methods gen:dist
  [(define (-sample self)
     (match-define (uniform-dist lo hi) self)
     (+ lo (* (- hi lo) (random))))
   (define (-pdf self x log?)
     (match-define (uniform-dist lo hi) self)
     (m:fluniform-pdf lo hi (fl x) log?))]
  #:methods gen:conjugate-dist
  [;; See also clipping rule in dist-posterior.
   (define/generic conjugate -conjugate)
   (define (-conjugate self xdistp xs)
     (match-define (uniform-dist lo hi) self)
     (and (= lo 0.0) (= hi 1.0) (conjugate (beta-dist 1.0 1.0) xdistp xs)))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (uniform-dist lo hi) self)
     (m:fluniform-cdf lo hi (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (uniform-dist lo hi) self)
     (m:fluniform-inv-cdf lo hi (fl x) log? 1-p?))
   (define (-support self)
     (match-define (uniform-dist lo hi) self)
     (real-range lo hi))
   (define (-mean self)
     (match-define (uniform-dist lo hi) self)
     (/ (+ lo hi) 2.0))
   (define (-median self)
     (match-define (uniform-dist lo hi) self)
     (/ (+ lo hi) 2.0))
   (define (-variance self)
     (match-define (uniform-dist lo hi) self)
     (let ([w (- hi lo)]) (* w w (fl 1/12))))])

(define-dist-struct triangle-dist
  ([lo rational? fl]
   [hi rational? fl]
   [mode rational? fl])
  #:guard (lambda (lo hi mode)
            (unless (< lo hi)
              (error 'triangle-dist "invalid range\n  range: (~e, ~e)" lo hi))
            (unless (and (<= lo mode) (<= mode hi))
              (error 'triangle-dist
                     (string-append
                      "mode is not between lower and upper bounds"
                      "\n  mode: ~e\n  range: [~e, ~e]")
                     mode lo hi))
            (values lo hi mode))
  #:methods gen:dist
  [(define (-sample self)
     (match-define (triangle-dist lo hi mode) self)
     (flvector-ref (m:fltriangle-sample lo hi mode 1) 0))
   (define (-pdf self x log?)
     (match-define (triangle-dist lo hi mode) self)
     (m:fltriangle-pdf lo hi mode (fl x) log?))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (triangle-dist lo hi mode) self)
     (m:fltriangle-cdf lo hi mode (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (triangle-dist lo hi mode) self)
     (m:fltriangle-inv-cdf lo hi mode (fl x) log? 1-p?))
   (define (-support self)
     (match-define (triangle-dist lo hi mode) self)
     (real-range lo hi))
   (define (-mean self)
     (match-define (triangle-dist lo hi mode) self)
     (/ (+ lo hi mode) 3.0))
   (define (-mode self)
     (match-define (triangle-dist lo hi mode) self)
     (list mode))
   (define (-variance self)
     (match-define (triangle-dist lo hi mode) self)
     (/ (- (+ (* lo lo) (* hi hi) (* mode mode))
           (+ (* lo hi) (* lo mode) (* hi mode)))
        18.0))])


;; ============================================================
;; Additional continuous real distributions

(define-dist-struct pareto-dist
  ([scale positive-rational? fl]  ;; x_m
   [shape positive-rational? fl]) ;; alpha
  #:methods gen:dist
  [(define (-sample self)
     (-invcdf self (random) #f #f))
   (define (-pdf self x log?)
     (match-define (pareto-dist scale shape) self)
     (define lp
       (if (>= x scale)
           (- (+ (log shape) (* shape (log scale)))
              (* (add1 shape) (log x)))
           -inf.0))
     (if log? lp (exp lp)))]
  #:methods gen:conjugate-dist
  [(define (-conjugate self xdistp xs)
     (match-define (pareto-dist scale shape) self)
     (match xdistp
       [`(uniform-dist 0 _)
        (pareto-dist (for/fold ([acc scale]) ([x (in-vector xs)]) (max x acc))
                     (+ shape (vector-length xs)))]
       [_ #f]))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (pareto-dist xm alpha) self)
     (cond [(< x xm) (impossible log?)]
           [log?
            (define ltail (* alpha (- (log xm) (log x))))
            (if 1-p? ltail (logspace- 0 ltail))]
           [else
            (define tail (expt (/ xm x) alpha))
            (if 1-p? tail (- 1 tail))]))
   (define (-invcdf self p log? 1-p?)
     (match-define (pareto-dist xm alpha) self)
     (define lq
       (cond [log? (if 1-p? p (logspace- (log 1.0) p))]
             [else (log (if 1-p? p (- 1.0 p)))]))
     (exp (+ (log xm) (* (- (/ alpha)) lq))))
   (define (-support self)
     (real-range (pareto-dist-scale self) +inf.0))
   (define (-mean self)
     (match-define (pareto-dist scale shape) self)
     (if (<= shape 1) +inf.0 (/ (* scale shape) (- shape 1.0))))
   (define (-modes self)
     (list (pareto-dist-scale self)))
   (define (-variance self)
     (match-define (pareto-dist scale shape) self)
     (if (<= shape 2)
         +inf.0
         (/ (* scale scale shape)
            (* (- shape 1.0) (- shape 1.0) (- shape 2.0)))))])

(define-dist-struct student-t-dist
  ([degrees positive-rational? fl]
   [mean rational? fl]
   [scale positive-rational? fl])
  #:extension (ext) ;; #f or math/distribution Student-t-Dist
  #:methods gen:dist
  [(define (-sample self)
     (-t-sample (-t-ext self)))
   (define (-pdf self x log?)
     (-t-pdf (-t-ext self) x log?))]
  #:methods gen:real-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (-t-cdf (-t-ext self) x log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (-t-inv-cdf (-t-ext self) p log? 1-p?))
   (define (-support self)
     (real-range -inf.0 +inf.0))
   (define (-mean self)
     (match-define (student-t-dist degrees mean scale _) self)
     (if (> degrees 1) mean #f))
   (define (-median self)
     (student-t-dist-mean self))
   (define (-modes self)
     (list (student-t-dist-mean self)))
   (define (-variance self)
     (define degrees (student-t-dist-degrees self))
     (cond [(> degrees 2) (/ degrees (- degrees 2.0))]
           [(> degrees 1) +inf.0]
           [else #|undefined|# #f]))])

(define (-t-ext self)
  (or (student-t-dist-ext self)
      (let ()
        (match-define (student-t-dist degrees mean scale _) self)
        (define ext (m:student-t-dist degrees mean scale))
        (set-student-t-dist-ext! self ext)
        ext)))

(module student-t typed/racket/base
  (require math/distributions)
  (provide (all-defined-out))

  (: -t-sample : Student-T-Dist -> Real)
  (define (-t-sample tdist)
    (sample tdist))

  (: -t-pdf : Student-T-Dist Real Any -> Flonum)
  (define (-t-pdf tdist x log?)
    (pdf tdist x log?))

  (: -t-cdf : Student-T-Dist Real Any Any -> Flonum)
  (define (-t-cdf tdist x log? 1-p?)
    (cdf tdist x log? 1-p?))

  (: -t-inv-cdf : Student-T-Dist Real Any Any -> Flonum)
  (define (-t-inv-cdf tdist p log? 1-p?)
    (inv-cdf tdist p log? 1-p?)))
(require (submod "." student-t))


;; ============================================================
;; Discrete integer distributions from math library (infinite)

(define-dist-struct geometric-dist
  ([p probability? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (geometric-dist p) self)
     (exact (flvector-ref (m:flgeometric-sample p 1) 0)))
   (define (-pdf self x log?)
     (match-define (geometric-dist p) self)
     (if (integer? x) (m:flgeometric-pdf p (fl x) log?) (impossible log?)))]
  #:methods gen:integer-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (geometric-dist p) self)
     (m:flgeometric-cdf p (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (geometric-dist p) self)
     (exact (m:flgeometric-inv-cdf p (fl x) log? 1-p?)))
   (define (-support self)
     (integer-range 0 +inf.0))
   (define (-mean self)
     (match-define (geometric-dist p) self)
     (/ (- 1.0 p) p))
   (define (-modes self) '(0))
   (define (-variance self)
     (match-define (geometric-dist p) self)
     (/ (- 1 p) (* p p)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-naturals))])

(define-dist-struct poisson-dist
  ([mean positive-rational? fl]) ;; aka rate, λ
  #:methods gen:dist
  [(define (-sample self)
     (match-define (poisson-dist mean) self)
     (exact (flvector-ref (m:flpoisson-sample mean 1) 0)))
   (define (-pdf self x log?)
     (match-define (poisson-dist mean) self)
     (if (integer? x) (m:flpoisson-pdf mean (fl x) log?) (impossible log?)))]
  #:methods gen:integer-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (poisson-dist mean) self)
     (m:flpoisson-cdf mean (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (poisson-dist mean) self)
     (exact (m:flpoisson-inv-cdf mean (fl x) log? 1-p?)))
   (define (-support self)
     (integer-range 0 +inf.0))
   (define (-mean self) (poisson-dist-mean self))
   (define (-modes self)
     (match-define (poisson-dist mean) self)
     (if (integer? mean)
         (list mean (sub1 mean))
         (list (floor mean))))
   (define (-variance self) (poisson-dist-mean self))]
  #:methods gen:enumerable-dist
  [(define (-enum self)
     (in-naturals))])


;; ============================================================
;; Discrete integer distributions from math library (finite)

(define-dist-struct bernoulli-dist
  ([p probability? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (bernoulli-dist p) self)
     (if (<= (random) p) 1 0))
   (define (-pdf self x log?)
     (match-define (bernoulli-dist p) self)
     (define r (cond [(= x 1) p] [(= x 0) (- 1 p)] [else 0.0]))
     (convert-p r log? #f))
   (define (-count self) 2)]
  #:methods gen:integer-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (bernoulli-dist p) self)
     (define r (cond [(< x 0) 0.0] [(< x 1) (- 1.0 p)] [else 1.0]))
     (convert-p r log? 1-p?))
   (define (-invcdf self r0 log? 1-p?)
     (match-define (bernoulli-dist p) self)
     (define r (unconvert-p r0 log? 1-p?))
     (cond [(<= r p) 1] [else 0]))
   (define (-support self)
     (integer-range 0 1))
   (define (-mean self) (bernoulli-dist-p self))
   (define (-modes self)
     (match-define (bernoulli-dist p) self)
     (cond [(> p 0.5) '(1)] [(< p 0.5) '(0)] [else '(0 1)]))
   (define (-variance self)
     (match-define (bernoulli-dist p) self)
     (* p (- 1 p)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-range 0 2))
   (define (-wsequence self)
     (match-define (bernoulli-dist p) self)
     (in-hash (hash 1 p 0 (- 1.0 p))))])

(define-dist-struct binomial-dist
  ([n exact-nonnegative-integer?]
   [p probability? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (binomial-dist n p) self)
     (exact (flvector-ref (m:flbinomial-sample (fl n) p 1) 0)))
   (define (-pdf self x log?)
     (match-define (binomial-dist n p) self)
     (if (integer? x) (m:flbinomial-pdf (fl n) p (fl x) log?) (impossible log?)))
   (define (-count self)
     (match-define (binomial-dist n p) self)
     (add1 n))]
  #:methods gen:integer-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (binomial-dist n p) self)
     (m:flbinomial-cdf (fl n) p (fl x) log? 1-p?))
   (define (-invcdf self x log? 1-p?)
     (match-define (binomial-dist n p) self)
     (exact (m:flbinomial-inv-cdf (fl n) p (fl x) log? 1-p?)))
   (define (-support self)
     (match-define (binomial-dist n _) self)
     (integer-range 0 n))
   (define (-mean self)
     (match-define (binomial-dist n p) self)
     (* n p))
   (define (-modes self)
     (match-define (binomial-dist n p) self)
     (filter-modes (lambda (x) (m:flbinomial-pdf (fl n) p x #f))
                   (let ([m (exact (floor (* (+ n 1) p)))])
                     (list m (sub1 m)))))
   (define (-variance self)
     (match-define (binomial-dist n p) self)
     (* n p (- 1.0 p)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (match-define (binomial-dist n _) self)
     (in-range 0 (add1 n)))])


;; ============================================================
;; Other integer distributions (infinite)

(define-dist-struct negative-binomial-dist
  ;; Represents number of failures before reaching r successes (p = Pr[success]).
  ([r exact-positive-integer?] [p probability? fl])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (negative-binomial-dist r p) self)
     (define rate (flvector-ref (m:flgamma-sample (fl r) (/ (- 1 p) p) 1) 0))
     (exact (flvector-ref (m:flpoisson-sample rate 1) 0)))
   (define (-pdf self x log?)
     (match-define (negative-binomial-dist r p) self)
     (cond [(and (integer? x) (>= x 0))
            (define k (exact x))
            (cond [log?
                   (define lcoeff (m:fllog-binomial (fl (+ k r -1)) (fl k)))
                   (+ lcoeff (* k (log (- 1.0 p))) (* (fl r) (log p)))]
                  [else
                   (define coeff (m:flbinomial (fl (+ k r -1)) (fl k)))
                   (* coeff (expt (- 1 p) k) (expt p r))])]
           [else (impossible log?)]))]
  #:methods gen:integer-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (negative-binomial-dist r p) self)
     (define k (floor (fl x)))
     (m:flbinomial-cdf (+ k (fl r)) (- 1.0 p) k log? 1-p?))
   (define (-invcdf self px log? 1-p?)
     (match-define (negative-binomial-dist r p) self)
     (if 1-p?
         (cond [(<= px (if log? -inf.0 0.0)) +inf.0]
               [else (find-least-natural (lambda (k) (< (-cdf self k log? #t) px)))])
         (cond [(>= px (if log? 0.0 1.0)) +inf.0]
               [else (find-least-natural (lambda (k) (>= (-cdf self k log? #f) px)))])))
   (define (-support self)
     (integer-range 0 +inf.0))
   (define (-mean self)
     (match-define (negative-binomial-dist r p) self)
     (/ (* r (- 1.0 p)) p))
   (define (-modes self)
     (match-define (negative-binomial-dist r p) self)
     (if (> r 1) (list (exact (floor (/ (* (- r 1.0) (- 1.0 p)) p)))) '(0)))
   (define (-variance self)
     (match-define (negative-binomial-dist r p) self)
     (/ (* r (- 1.0 p)) (sqr p)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-naturals))])

;; ============================================================
;; Other integer distributions (finite)

(define-dist-struct categorical-dist
  ;; support is {0,...,k-1}
  ([weights vector? -categorical-guard-weights])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (categorical-dist ws) self)
     (-categorical-inv-cdf 'dist-sample:categorical-dist ws (random)))
   (define (-pdf self x log?)
     (match-define (categorical-dist ws) self)
     (cond [(and (integer? x) (<= 0 x (sub1 (vector-length ws))))
            (convert-p (vector-ref ws (exact x)) log? #f)]
           [else (impossible log?)]))
   (define (-count self)
     (match-define (categorical-dist ws) self)
     (vector-length ws))]
  #:methods gen:integer-dist []
  #:methods gen:numeric-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (categorical-dist ws) self)
     (define k (exact (floor x)))
     (define p
       (cond [(<= 0 k (sub1 (vector-length ws)))
              (vector-ref (-categorical-cws ws) k)]
             [(< k 0) 0.0]
             [else 1.0]))
     (convert-p p log? 1-p?))
   (define (-invcdf self p0 log? 1-p?)
     (match-define (categorical-dist ws) self)
     (define p (unconvert-p p0 log? 1-p?))
     (-categorical-inv-cdf 'dist-inv-cdf:categorical-dist ws p))
   (define (-support self)
     (match-define (categorical-dist ws) self)
     (integer-range 0 (sub1 (vector-length ws))))
   (define (-mean self)
     (match-define (categorical-dist ws) self)
     (for/sum ([i (in-naturals 0)] [w (in-vector ws)]) (* i w)))
   (define (-modes self)
     (match-define (categorical-dist ws) self)
     (define weights (categorical-dist-weights self))
     (let-values ([(best best-w)
                   (for/fold ([best null] [best-w -inf.0])
                             ([i (in-naturals 0)] [w (in-vector weights)])
                     (cond [(> w best-w)
                            (values (list i) w)]
                           [(= w best-w)
                            (values (cons i best) best-w)]
                           [else (values best best-w)]))])
       (reverse best)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (match-define (categorical-dist ws) self)
     (in-range 0 (vector-length ws)))
   (define (-wsequence self)
     (match-define (categorical-dist ws) self)
     (in-parallel (in-naturals 0) (in-vector ws)))])

(define (-categorical-guard-weights in-ws)
  (normalize-inexact-weights 'categorical-dist in-ws))

;; categorical:ws=>cws : WeakHasheq[ImmVector => ImmVector]
(define categorical:ws=>cws (make-weak-hasheq))

(define (-categorical-cws ws)
  (hash-ref! categorical:ws=>cws ws (lambda () (cumulative-vector ws))))

(define (-categorical-inv-cdf who ws p)
  (define cws (-categorical-cws ws))
  (binary-search/least-geq cws p))

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

(define (digamma x) (m:psi0 x))

;; ============================================================
;; Tests

(module+ test
  (require racket/list rackunit)

  ;; rackunit's check-= fails on -inf.0, etc
  (define-simple-check (check= actual expected tolerance)
    (<= (- expected (abs tolerance)) actual (+ expected (abs tolerance))))

  (define EPS 1e-3)

  (define (check-numeric-dist d continuous? [inv-cdf? continuous?])
    (define (check-value v)
      (if continuous?
          (check-pred inexact? v)
          (check-pred exact? v))
      (define p (dist-pdf d v))
      (check-pred inexact? p)
      (check-true (>= p 0))
      (define lp (dist-pdf d v #t))
      (check-pred inexact? lp)
      (check= lp (log p) EPS)
      (define cp (dist-cdf d v))
      (check-pred inexact? cp)
      (check-pred probability? cp)
      (check= (dist-cdf d v #f #t) (- 1 cp) EPS)
      (check= (dist-cdf d v #t #f) (log cp) EPS)
      (check= (dist-cdf d v #t #t) (log (- 1 cp)) EPS)
      (when inv-cdf?
        ;; Unreliable for integer-valued distributions.
        (check= (dist-inv-cdf d cp #f #f) v EPS)
        (check= (dist-inv-cdf d (log cp) #t #f) v EPS)
        (check= (dist-inv-cdf d (- 1 cp) #f #t) v EPS)
        (check= (dist-inv-cdf d (log (- 1 cp)) #t #t) v EPS))
      (void))
    (define vs (remove-duplicates (for/list ([i 20]) (dist-sample d))))
    (for ([v vs]) (test-case (format "~e, ~e" d v) (check-value v))))

  (let ([d (beta-dist 3 4)])
    (check-numeric-dist d #t))
  (let ([d (cauchy-dist 0 1)])
    (check-numeric-dist d #t))
  (let ([d (exponential-dist 1)])
    (check-numeric-dist d #t))
  (let ([d (gamma-dist 2 3)])
    (check-numeric-dist d #t))
  (let ([d (logistic-dist 0 1)])
    (check-numeric-dist d #t))
  (let ([d (normal-dist 0 1)])
    (check-numeric-dist d #t))
  (let ([d (uniform-dist 0 1)])
    (check-numeric-dist d #t))
  (let ([d (triangle-dist 0 5 3)])
    (check-numeric-dist d #t))
  (let ([d (pareto-dist 1 2)])
    (check-numeric-dist d #t))

  (let ([d (binomial-dist 10 2/3)])
    (check-numeric-dist d #f))
  (let ([d (geometric-dist 2/3)])
    (check-numeric-dist d #f))
  (let ([d (poisson-dist 1)])
    (check-numeric-dist d #f))
  (let ([d (bernoulli-dist 0.2)])
    (check-numeric-dist d #f))
  (let ([d (categorical-dist '#(1/2 1/3 1/6))])
    (check-numeric-dist d #f))
  (let ([d (negative-binomial-dist 3 0.4)])
    (check-numeric-dist d #f))
  (begin))
