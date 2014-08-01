;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract/base
         racket/class
         data/order
         racket/dict
         "interfaces.rkt"
         "../dist.rkt"
         "util.rkt")
(provide (contract-out
          [mem (-> procedure? procedure?)]
          [sample (-> dist? any)])
         fail)

;; mem and sample wrappers

(define (mem f) (send (current-stochastic-ctx) mem f))
(define (sample dist) (send (current-stochastic-ctx) sample dist))
(define (fail [reason #f]) (send (current-stochastic-ctx) fail reason))

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
   (sample (make-bernoulli-dist prob))))

(define (bernoulli [prob 1/2])
  (inexact->exact
   (sample (make-bernoulli-dist prob))))

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
  (inexact->exact
   (floor
    (sample (make-categorical-dist (make-vector n (/ n)))))))

;; discrete/weights : Symbol (Listof A) (Listof Prob) -> A
(define (discrete/weights who vals probs)
  (unless (positive? (apply + probs))
    (error who "weights list sum is not positive\n  weights: ~e" probs))
  (list-ref vals (inexact->exact
                  (sample (make-categorical-dist probs)))))

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
   (sample (make-binomial-dist n p))))

;; geometric : Prob -> Integer
;; FIXME: discretizable
(define (geometric [p 1/2])
  (inexact->exact
   (sample (make-geometric-dist p))))

;; poisson : Real -> Integer
(define (poisson mean)
  (inexact->exact
   (sample (make-poisson-dist mean))))

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
  (sample (make-beta-dist a b)))

(define (cauchy [mode 0] [scale 1])
  (sample (make-cauchy-dist mode scale)))

;; exponential : PositiveReal -> PositiveReal
;; NOTE: mean aka scale = 1/rate
(define (exponential [mean 1])
  (sample (make-exponential-dist mean)))

;; gamma : PositiveReal PositiveReal -> Real
;; NOTE: scale = 1/rate
(define (gamma [shape 1] [scale 1])
  (sample (make-gamma-dist shape scale)))

;; logistic : Real Real -> Real
(define (logistic [mean 0] [scale 1])
  (sample (make-logistic-dist mean scale)))

;; normal : Real PositiveReal -> Real
;; NOTE: stddev = (sqrt variance)
(define (normal [mean 0] [stddev 1])
  (sample (make-normal-dist mean stddev)))

;; uniform : Real Real -> Real
(define uniform
  (case-lambda
    [() (uniform 0 1)]
    [(max) (uniform 0 max)]
    [(min max)
     (sample (make-uniform-dist min max))]))

;; ========================================

(provide sampler->discrete-dist
         sampler->mean+variance
         indicator/value
         indicator/predicate)

(define (sampler->discrete-dist s n [f values] #:normalize? [normalize? #t])
  (define h (make-hash))
  (cond [(is-a? s sampler<%>)
         (for ([i (in-range n)])
           (let ([a (f (send s sample))])
             (hash-set! h a (add1 (hash-ref h a 0)))))]
        [(is-a? s weighted-sampler<%>)
         (for ([i (in-range n)])
           (let* ([a+p (send s sample/weight)]
                  [a (f (car a+p))]
                  [p (cdr a+p)])
             (hash-set! h a (+ p (hash-ref h a 0)))))]
        [else (raise-argument-error 'sampler->discrete-dist "sampler" s)])
  (table->discrete-dist h normalize?))

(define (table->discrete-dist h normalize?)
  (define total-w
    (for/sum ([(a w) (in-hash h)]) w))
  (define entries
    (for/list ([(a w) (in-hash h)])
      (cons a (exact->inexact (/ w total-w)))))
  (make-discrete-dist entries #:normalize? normalize?))

(define (sampler->mean+variance s n [f values])
  (define-values (sum-w sum-f sum-f^2)
    (cond [(is-a? s sampler<%>)
           (define-values (sum-f sum-f^2)
             (for/fold ([sum-f 0.0] [sum-f^2 0.0]) ([i (in-range n)])
               (let ([v (f (send s sample))])
                 (values (+ sum-f v) (+ sum-f^2 (* v v))))))
           (values n sum-f sum-f^2)]
          [(is-a? s weighted-sampler<%>)
           (for/fold ([sum-w 0.0] [sum-f 0.0] [sum-f^2 0.0]) ([i (in-range n)])
             (let* ([v+w (send s sample/weight)]
                    [v (f (car v+w))]
                    [w (cdr v+w)])
               (values (+ sum-w w) (+ sum-f (* w v)) (+ sum-f^2 (* w v v)))))]
          [else (raise-argument-error 'sampler->mean+variance "sampler" s)]))
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
   (-> discrete-dist? discrete-dist?
       (>=/c 0))]
  [sampler->KS
   (-> (-> real?) exact-positive-integer? dist?
       real?)]))

;; discrete-dist-error : (Discrete-Dist A) (Discrete-Dist A) -> Real
(define (discrete-dist-error a b)
  ;; Why 1/2? Because every error is counted twice: 
  ;; once for being present where it shouldn't be, 
  ;; and again for being absent from where it should be.
  (* 1/2
     ;; a U b = a U (b - a)   --- membership means positive pdf
     (+ (for/sum ([aval (in-vector (discrete-dist-values a))])
          (define aweight (dist-pdf a aval))
          (define bweight (dist-pdf b aval 0))
          (if (positive? aweight)
              (abs (- aweight bweight))
              0))
        (for/sum ([bval (in-vector (discrete-dist-values b))]
                  #:when (zero? (dist-pdf a bval)))
          (define bweight (dist-pdf b bval))
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
