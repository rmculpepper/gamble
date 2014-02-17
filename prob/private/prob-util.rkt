#lang racket/base
(require math/distributions
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
  [discrete
   (-> (or/c exact-positive-integer?
             (listof (list/c any/c (>/c 0))))
       any/c)]))

;; flip : Prob -> (U #t #f)
(define (flip [prob 1/2])
  (ERP `(flip ,prob)
       (discrete-dist '(#t #f) (list prob (- 1 prob)))))

;; d2 : Prob -> (U 1 0)
(define (d2 [prob 1/2])
  (if (flip prob) 1 0))

;; discrete : Nat -> Nat
;; discrete : (listof (list A Prob))) -> A
(define (discrete v)
  (cond [(list? v)
         (ERP `(discrete ,v)
              (lambda () (discrete-dist v)))]
        [else
         (ERP `(discrete ,v)
              (discrete-dist (for/list ([i v]) i)))]))

;; == Countable distributions ==

(provide
 (contract-out
  [geometric
   (->* [] [probability?]
        exact-nonnegative-integer?)]
  [poisson
   (->* [] [(>/c 0)]
        exact-nonnegative-integer?)]
  [binomial
   (->* [exact-nonnegative-integer? probability?] []
        exact-nonnegative-integer?)]))

;; binomial : Nat Prob -> Integer
;; FIXME: discretizable
(define (binomial n p)
  (inexact->exact
   (ERP `(binomial ,n ,p)
        (binomial-dist n p))))

;; geometric : Prob -> Integer
;; FIXME: discretizable
(define (geometric [p 1/2])
  (inexact->exact
   (ERP `(geometric ,p)
        (geometric-dist p))))

;; poisson : Real -> Integer
;; FIXME: probably discretizable (???)
(define (poisson mean)
  (inexact->exact
   (ERP `(poisson ,mean)
        (poisson-dist mean))))

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
       (beta-dist a b)))

(define (cauchy [mode 0] [scale 1])
  (ERP `(cauchy ,mode ,scale)
       (cauchy-dist mode scale)))

;; exponential : PositiveReal -> PositiveReal
;; NOTE: mean aka scale = 1/rate
(define (exponential mean)
  (ERP `(exponential ,mean)
       (exponential-dist mean)))

;; gamma : PositiveReal PositiveReal -> Real
;; NOTE: scale = 1/rate
(define (gamma [shape 1] [scale 1])
  (ERP `(gamma ,shape ,scale)
       (gamma-dist shape scale)))

;; logistic : Real Real -> Real
(define (logistic [mean 0] [scale 1])
  (ERP `(logistic ,mean ,scale)
       (gamma-dist mean scale)))

;; normal : Real PositiveReal -> Real
;; NOTE: stddev = (sqrt variance)
(define (normal [mean 0] [stddev 1])
  (ERP `(normal ,mean ,stddev)
       (normal-dist mean stddev)))

;; uniform : Real Real -> Real
(define uniform
  (case-lambda
    [() (uniform 0 1)]
    [(max) (uniform 0 max)]
    [(min max)
     (ERP `(uniform ,min ,max)
          (uniform-dist min max))]))
