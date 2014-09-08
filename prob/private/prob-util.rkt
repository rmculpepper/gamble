;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         "interfaces.rkt"
         "context.rkt"
         "../dist.rkt"
         "util.rkt")
(provide (all-defined-out))

;; mem and sample wrappers

(define (mem f) (send (current-stochastic-ctx) mem f))
(define (observe-at dist val) (send (current-stochastic-ctx) observe-at dist val))
(define (fail [reason #f]) (send (current-stochastic-ctx) fail reason))
(define (sample* dist) (send (current-stochastic-ctx) sample dist))

(define (sample dist)
  (define obs (observing?))
  (cond [(observation? obs)
         (with-continuation-mark
             obs-mark 'ok ;; overwrite this frame's 'unknown mark
           (let ()
             (define cc (get-observe-context))
             (define value (observe-context-adjust-value cc obs))
             (when (verbose?)
               (eprintf "** CC = ~e\n" cc)
               (eprintf "** sample -> condition: ~e @ ~e\n" dist value))
             (observe-at dist value)
             value))]
        [else (sample* dist)]))

;; == Finite distributions ==

;; flip : Prob -> (U #t #f)
(define (flip [prob 1/2])
  (positive?
   (sample (bernoulli-dist prob))))

;; bernoulli : Prob -> (U 1 0)
(define (bernoulli [prob 1/2])
  (sample (bernoulli-dist prob)))

;; categorical : (Vectorof Prob) -> Nat
(define (categorical weights)
  (sample (categorical-dist weights)))

;; discrete : Nat -> Nat
(define (discrete n/dist)
  (discrete/weights 'discrete (map car n/dist) (map cdr n/dist)))

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
  (sample (categorical-dist (make-vector n (/ n)))))

;; discrete/weights : Symbol (Listof A) (Listof Prob) -> A
(define (discrete/weights who vals probs)
  (unless (positive? (apply + probs))
    (error who "weights list sum is not positive\n  weights: ~e" probs))
  (list-ref vals (inexact->exact
                  (sample (categorical-dist (list->vector probs))))))

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

;; ========================================

(define ((indicator/value v) x)
  (if (equal? x v) 1 0))

(define ((indicator/predicate p?) x)
  (if (p? x) 1 0))

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

;; ----------------------------------------

;; discrete-dist-error : (Discrete-Dist A) (Discrete-Dist A) -> Real
(define (discrete-dist-error a b)
  ;; Why 1/2? Because every error is counted twice: 
  ;; once for being present where it shouldn't be, 
  ;; and again for being absent from where it should be.
  (* 1/2
     ;; a U b = a U (b - a)   --- membership means positive pdf
     (+ (for/sum ([aval (in-vector (discrete-dist-values a))])
          (define aweight (dist-pdf a aval))
          (define bweight (dist-pdf b aval))
          (if (positive? aweight)
              (abs (- aweight bweight))
              0))
        (for/sum ([bval (in-vector (discrete-dist-values b))]
                  #:when (zero? (dist-pdf a bval)))
          (define bweight (dist-pdf b bval))
          (abs bweight)))))
