;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract/base
         data/order
         racket/dict
         "prob-hooks.rkt"
         "util.rkt")
(provide (contract-out
          [mem (-> procedure? procedure?)])
         ERP
         fail)

;; mem and ERP wrappers

;; NOTE: enum-ERP knows about discrete dists by tag
;; FIXME: add discrete-dist? to math/distributions
(define (mem f) ((current-mem) f))
(define (ERP tag dist) ((current-ERP) tag dist))
(define (fail [reason #f]) ((current-fail) reason))

;; == Finite distributions ==

(provide
 (contract-out
  [flip
   (->* [] [probability?] boolean?)]
  [bernoulli
   (->* [] [probability?] (or/c 1 0))]
  [discrete
   (-> (or/c exact-positive-integer?
             (listof (cons/c any/c (>=/c 0))))
       any)]
  [discrete*
   (-> list? (listof (>=/c 0))
       any)]))

;; flip : Prob -> (U #t #f)
(define (flip [prob 1/2])
  (positive?
   (ERP `(flip ,prob)
        (make-bernoulli-dist prob))))

(define (bernoulli [prob 1/2])
  (inexact->exact
   (ERP `(bernoulli ,prob) (make-bernoulli-dist prob))))

;; discrete : Nat -> Nat
;; discrete : (Listof (Cons A Prob)) -> A
(define (discrete n/dist)
  (cond [(list? n/dist)
         (discrete/weights 'discrete (map car n/dist) (map cdr n/dist))]
        [else
         (discrete-uniform n/dist)]))

;; discrete* : (Listof A) (Listof Prob) -> A
(define (discrete* vals [weights #f])
  (cond [(eq? weights #f)
         (unless (pair? vals)
           (error 'discrete* "empty values list"))
         (list-ref vals (discrete-uniform (length vals)))]
        [else
         (unless (= (length vals) (length weights))
           (error 'discrete*
                  "values list and weights list have unequal lengths\n  values: ~e\n  weights: ~e"
                  vals weights))
         (discrete/weights 'discrete* vals weights)]))

;; discrete-uniform : Nat -> Nat
(define (discrete-uniform n)
  (inexact->exact (floor (ERP `(discrete-uniform ,n)) (make-uniform-dist 0 n))))

;; discrete/weights : Symbol (Listof A) (Listof Prob) -> A
(define (discrete/weights who vals probs)
  (unless (positive? (apply + probs))
    (error who "weights list sum is not positive\n  weights: ~e" probs))
  (list-ref vals (inexact->exact
                  (ERP `(discrete ,vals ,probs)
                       (make-discrete-dist probs)))))

;; == Countable distributions ==

(provide
 (contract-out
  [geometric
   (->* [] [probability?]
        exact-nonnegative-integer?)]
  [poisson
   (->* [(>/c 0)] []
        exact-nonnegative-integer?)]
  [binomial
   (->* [exact-nonnegative-integer? probability?] []
        exact-nonnegative-integer?)]))

;; binomial : Nat Prob -> Integer
;; FIXME: discretizable
(define (binomial n p)
  (inexact->exact
   (ERP `(binomial ,n ,p)
        (make-binomial-dist n p))))

;; geometric : Prob -> Integer
;; FIXME: discretizable
(define (geometric [p 1/2])
  (inexact->exact
   (ERP `(geometric ,p)
        (make-geometric-dist p))))

;; poisson : Real -> Integer
(define (poisson mean)
  (inexact->exact
   (ERP `(poisson ,mean)
        (make-poisson-dist mean))))

;; == Continuous distributions ==

(provide
 (contract-out
  [beta
   (-> (>/c 0) (>/c 0)
       real?)]
  [cauchy
   (->* [] [real? (>/c 0)]
        real?)]
  [exponential
   (->* [] [(>/c 0)]
        real?)]
  [gamma
   (->* [] [(>/c 0) (>/c 0)]
        real?)]
  [logistic
   (->* [] [real? (>/c 0)]
        real?)]
  [normal
   (->* [] [real? (>/c 0)]
        real?)]
  [uniform
   (->* [] [real? real?]
        real?)]
  ))

;; beta : PositiveReal PositiveReal -> Real in [0,1]
(define (beta a b)
  (ERP `(beta ,a ,b)
       (make-beta-dist a b)))

(define (cauchy [mode 0] [scale 1])
  (ERP `(cauchy ,mode ,scale)
       (make-cauchy-dist mode scale)))

;; exponential : PositiveReal -> PositiveReal
;; NOTE: mean aka scale = 1/rate
(define (exponential [mean 1])
  (ERP `(exponential ,mean)
       (make-exponential-dist mean)))

;; gamma : PositiveReal PositiveReal -> Real
;; NOTE: scale = 1/rate
(define (gamma [shape 1] [scale 1])
  (ERP `(gamma ,shape ,scale)
       (make-gamma-dist shape scale)))

;; logistic : Real Real -> Real
(define (logistic [mean 0] [scale 1])
  (ERP `(logistic ,mean ,scale)
       (make-logistic-dist mean scale)))

;; normal : Real PositiveReal -> Real
;; NOTE: stddev = (sqrt variance)
(define (normal [mean 0] [stddev 1])
  (ERP `(normal ,mean ,stddev)
       (make-normal-dist mean stddev)))

;; uniform : Real Real -> Real
(define uniform
  (case-lambda
    [() (uniform 0 1)]
    [(max) (uniform 0 max)]
    [(min max)
     (ERP `(uniform ,min ,max)
          (make-uniform-dist min max))]))

;; ========================================

(provide sampler->discrete-dist
         weighted-sampler->discrete-dist
         sampler->mean+variance
         weighted-sampler->mean+variance
         indicator/value
         indicator/predicate)

(define (sampler->discrete-dist s n)
  (define h (make-hash))
  (for ([i (in-range n)])
    (let ([a (s)])
      (hash-set! h a (add1 (hash-ref h a 0)))))
  (table->discrete-dist h))

(define (weighted-sampler->discrete-dist s n)
  (define h (make-hash))
  (for ([i (in-range n)])
    (let* ([r (s)]
           [a (car r)]
           [p (cadr r)])
      (hash-set! h a (+ p (hash-ref h a 0)))))
  (table->discrete-dist h))

(define (table->discrete-dist h)
  (define total-w
    (for/sum ([(a w) (in-hash h)]) w))
  (define entries
    (for/list ([(a w) (in-hash h)])
      (cons a (exact->inexact (/ w total-w)))))
  (sort entries (order-<? datum-order)))

(define (sampler->mean+variance s n [f values])
  (define-values (sum-f sum-f^2)
    (for/fold ([sum-f 0.0] [sum-f^2 0.0]) ([i (in-range n)])
      (let ([v (f (s))])
        (values (+ sum-f v) (+ sum-f^2 (* v v))))))
  (define Ef (/ sum-f n))
  (values Ef
          (- (/ sum-f^2 n) (* Ef Ef))))

(define (weighted-sampler->mean+variance s n [f values])
  (define-values (sum-w sum-f sum-f^2)
    (for/fold ([sum-w 0.0] [sum-f 0.0] [sum-f^2 0.0]) ([i (in-range n)])
      (let* ([r (s)]
             [v (f (car r))]
             [w (cdr r)])
        (values (+ sum-w w) (+ sum-f (* w v)) (+ sum-f^2 (* w v v))))))
  (define Ef (/ sum-f sum-w))
  (values Ef
          (- (/ sum-f^2 sum-w) (* Ef Ef))))

(define ((indicator/value v) x)
  (if (equal? x v) 1 0))

(define ((indicator/predicate p?) x)
  (if (p? x) 1 0))

(provide
 (contract-out
  [discrete-dist-error
   (-> (listof (cons/c any/c (>=/c 0)))
       (listof (cons/c any/c (>=/c 0)))
       (>=/c 0))]
  [sampler->KS
   (-> (-> real?) exact-positive-integer? dist?
       real?)]))

;; discrete-dist-error : (Dict A Real) (Dict A Real) -> Real
(define (discrete-dist-error a b)
  ;; Why 1/2? Because every error is counted twice: 
  ;; once for being present where it shouldn't be, 
  ;; and again for being absent from where it should be.
  (* 1/2
     ;; a U b = a U (b - a)
     (+ (for/sum ([(aval aweight) (in-dict a)])
          (define bweight (dict-ref b aval 0))
          (abs (- aweight bweight)))
        (for/sum ([(bval bweight) (in-dict b)]
                  #:when (not (dict-ref a bval #f)))
          (abs bweight)))))

;; sampler->KS : Sampler Nat Dist -> Real
;; Compute Kolmogorov–Smirnov statistic for samples and dist.
(define (sampler->KS sampler iters dist)
  (define v (list->vector (sort (repeat sampler iters) <)))
  (KS/samples v (lambda (x) (dist-cdf dist x))))

;; KS/samples : (Vectorof Real) (Real -> Real) Boolean -> Real
;; pre: v is sorted ascending, cdf is monotone nondecreasing w/ onto (0,1)
;; Note: need continuity to calculate sup_{x in (xa,xb)} cdf(x) = cdf(xb)
(define (KS/samples v cdf)
  (define continuous? #f)
  (define n (vector-length v))
  (define (cdf_i i) (cdf (vector-ref v i)))
  (define (ecdf_i i) (/ (add1 i) n)) ;; note, (ecdf_i -1) = 0
  (max (for/fold ([acc 0]) ([i (in-range n)])
         (max acc
              (abs (- (cdf_i i) (ecdf_i i)))
              (if continuous?
                  (abs (- (cdf_i i) (ecdf_i (sub1 i))))
                  0)))
       (if continuous? 
           (abs (- (cdf_i (sub1 n)) 1))
           0)))
