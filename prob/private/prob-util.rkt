;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require math/distributions
         racket/flonum
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
        (make-dist bernoulli #:params (prob) #:enum 2))))

;; d2 : Prob -> (U 1 0)
(define (d2 [prob 1/2])
  (bernoulli prob))

(define (bernoulli [prob 1/2])
  (inexact->exact
   (ERP `(bernoulli ,prob)
        (make-dist bernoulli #:params (prob) #:enum 2))))

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
                               (make-dist uniform #:params (0 n) #:enum n))))))]
           [(exact-positive-integer? n/vals)
            (let ([n n/vals])
              (inexact->exact
               (floor
                (ERP `(discrete ,n)
                     (make-dist uniform #:params (0 n) #:enum n)))))]
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
     (define n (length vals))
     (list-ref vals (inexact->exact
                     (ERP `(discrete ,vals ,probs)
                          (let ([prob-sum (apply + probs)])
                            (make-dist discrete #:raw-params (probs prob-sum) #:enum n)))))]))

(define (discrete-from-enumeration e)
  (discrete (map car e) (map cadr e)))

;; Discrete weighted dist functions
(define (fldiscrete-pdf probs prob-sum k log?)
  (/ (list-ref probs (inexact->exact k)) prob-sum))
(define (fldiscrete-cdf probs prob-sum k log? 1-p?)
  (when (or log? 1-p?) (error 'fldiscrete-cdf "unimplemented"))
  (let ([k (inexact->exact k)])
    (/ (for/sum ([i (in-range (add1 k))] [prob (in-list probs)]) prob)
       prob-sum)))
(define (fldiscrete-inv-cdf probs prob-sum p log? 1-p?)
  (when (or log? 1-p?) (error 'fldiscrete-inv-cdf "unimplemented"))
  (let loop ([probs probs] [p (* p prob-sum)] [i 0])
    (cond [(null? probs)
           (error 'fldiscrete-inv-cdf "out of values")]
          [(< p (car probs))
           i]
          [else
           (loop (cdr probs) (- p (car probs)) (add1 i))])))
(define (fldiscrete-sample probs prob-sum n)
  (define v (make-flvector n))
  (for ([i (in-range n)])
    (flvector-set! v i (fldiscrete-inv-cdf probs prob-sum (random) #f #f)))
  v)

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
        (make-dist binomial #:params (n p) #:enum (add1 n)))))

;; geometric : Prob -> Integer
;; FIXME: discretizable
(define (geometric [p 1/2])
  (inexact->exact
   (ERP `(geometric ,p)
        (make-dist geometric #:params (p) #:enum 'lazy))))

;; poisson : Real -> Integer
;; FIXME: probably discretizable (???)
(define (poisson mean)
  (inexact->exact
   (ERP `(poisson ,mean)
        (make-dist poisson #:params (mean) #:enum 'lazy))))

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
       (make-dist beta #:params (a b) #:enum #f)))

(define (cauchy [mode 0] [scale 1])
  (ERP `(cauchy ,mode ,scale)
       (make-dist cauchy #:params (mode scale) #:enum #f)))

;; exponential : PositiveReal -> PositiveReal
;; NOTE: mean aka scale = 1/rate
(define (exponential mean)
  (ERP `(exponential ,mean)
       (make-dist exponential #:params (mean) #:enum #f)))

;; gamma : PositiveReal PositiveReal -> Real
;; NOTE: scale = 1/rate
(define (gamma [shape 1] [scale 1])
  (ERP `(gamma ,shape ,scale)
       (make-dist gamma #:params (shape scale) #:enum #f)))

;; logistic : Real Real -> Real
(define (logistic [mean 0] [scale 1])
  (ERP `(logistic ,mean ,scale)
       (make-dist logistic #:params (mean scale) #:enum #f)))

;; normal : Real PositiveReal -> Real
;; NOTE: stddev = (sqrt variance)
(define (normal [mean 0] [stddev 1])
  (ERP `(normal ,mean ,stddev)
       (make-dist normal #:params (mean stddev) #:enum #f)))

;; uniform : Real Real -> Real
(define uniform
  (case-lambda
    [() (uniform 0 1)]
    [(max) (uniform 0 max)]
    [(min max)
     (ERP `(uniform ,min ,max)
          (make-dist uniform #:params (min max) #:enum #f))]))
