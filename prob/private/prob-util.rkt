;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/flonum
         racket/contract/base
         "prob-hooks.rkt"
         "util.rkt")
(provide rejection-sample
         (contract-out
          [mem (-> procedure? procedure?)])
         ERP)

;; Rejection sampling

(define (rejection-sample thunk pred [project values])
  (let ([v (thunk)])
    (if (pred v)
        (project v)
        (rejection-sample thunk pred project))))

;; mem and ERP wrappers

;; NOTE: enum-ERP knows about discrete dists by tag
;; FIXME: add discrete-dist? to math/distributions
(define (mem f) ((current-mem) f))
(define (ERP tag dist) ((current-ERP) tag dist))

;; ==  Finite distributions ==

(provide
 (contract-out
  [flip
   (->* [] [probability?] boolean?)]
  [d2
   (->* [] [probability?] (or/c 1 0))]
  [discrete-from-enumeration
   (-> (non-empty-listof (list/c any/c (>/c 0)))
       any)])
 discrete)

;; flip : Prob -> (U #t #f)
(define (flip [prob 1/2])
  (positive?
   (ERP `(flip ,prob)
        (make-bernoulli-dist prob))))

;; d2 : Prob -> (U 1 0)
(define (d2 [prob 1/2])
  (bernoulli prob))

(define (bernoulli [prob 1/2])
  (inexact->exact
   (ERP `(bernoulli ,prob) (make-bernoulli-dist prob))))

;; discrete : Nat -> Nat
;; discrete : (listof (list A Prob))) -> A
(define discrete
  (case-lambda
    [(n/vals)
     (cond [(and (list? n/vals) (pair? n/vals))
            (let ([n (length n/vals)])
              (list-ref n/vals
                        (inexact->exact
                         (floor
                          (ERP `(discrete ,n/vals)
                               (make-uniform-dist 0 n))))))]
           [(exact-positive-integer? n/vals)
            (let ([n n/vals])
              (inexact->exact
               (floor
                (ERP `(discrete ,n)
                     (make-uniform-dist 0 n)))))]
           [else
            (raise-argument-error 'discrete
              "(or/c exact-positive-integer? (and/c list? pair?))" 0 n/vals)])]
    [(vals probs)
     (unless (and (list? vals) (pair? vals))
       (raise-argument-error 'discrete "(and/c list? pair?)" 0 vals probs))
     (unless (and (list? probs) (pair? probs) (andmap real? probs) (andmap positive? probs))
       (raise-argument-error 'discrete "(non-empty-listof (>/c 0))" 1 vals probs))
     (unless (= (length vals) (length probs))
       (error 'discrete
              "values and probability weights have different lengths\n  values: ~e\n  weights: ~e"
              vals probs))
     (list-ref vals (inexact->exact
                     (ERP `(discrete ,vals ,probs)
                          (make-discrete-dist probs))))]))

(define (discrete-from-enumeration e)
  (discrete (map car e) (map cadr e)))

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
