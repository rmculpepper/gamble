;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         "interfaces.rkt"
         "context.rkt"
         "../dist.rkt")
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
               (vprintf "OBSERVE w/ context = ~e\n" cc)
               (vprintf "  sample -> condition: ~e @ ~e\n" dist value))
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
(define (discrete dict)
  (sample (make-discrete-dist dict)))

;; discrete* : (Listof/Vectorof A) (Listof/Vectorof Prob) -> A
(define (discrete* vals0 [weights0 #f])
  (let ([vals (if (list? vals0) (list->vector vals0) vals0)])
    (cond [(eq? weights0 #f)
           (sample (make-discrete-dist* vals))]
          [else
           (let ([weights (if (list? weights0) (list->vector weights0) weights0)])
             (unless (= (vector-length vals) (vector-length weights))
               (error 'discrete*
                      "values and weights have unequal lengths\n  values: ~e\n  weights: ~e"
                      vals0 weights0))
             (sample (make-discrete-dist* vals weights)))])))

;; discrete-uniform : Nat -> Nat
(define (discrete-uniform n)
  (sample (categorical-dist (make-vector n (/ n)))))

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

(define (inverse-gamma [shape 1] [scale 1])
  (sample (inverse-gamma-dist shape scale)))

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

(define (dirichlet alpha)
  (sample (dirichlet-dist alpha)))

;; ========================================

(define (multi-normal mean cov)
  (sample (multi-normal-dist mean cov)))
(define (wishart n V)
  (sample (wishart-dist n V)))
(define (inverse-wishart n V)
  (sample (inverse-wishart n V)))

;; ========================================

;; factor : Real -> Real
;; Weight the current trace by the given log-factor.
(define (factor ldensity)
  (observe-at (improper-dist ldensity) 0))

;; ========================================

(define ((indicator/value v) x)
  (if (equal? x v) 1 0))

(define ((indicator/predicate p?) x)
  (if (p? x) 1 0))

(define (sampler->discrete-dist s n [f values]
                                #:burn [burn 0]
                                #:thin [thin 0]
                                #:normalize? [normalize? #t])
  (define h (make-hash))
  (define (s*)
    (for ([_ (in-range thin)]) (send s sample/weight))
    (defmatch (cons v w) (send s sample/weight))
    (cons (f v) w))
  (for ([_ (in-range burn)]) (send s sample/weight))
  (for ([i (in-range n)])
    (let* ([a+p (send s sample/weight)]
           [a (car a+p)]
           [p (cdr a+p)])
      (hash-set! h a (+ p (hash-ref h a 0)))))
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

;; ----------------------------------------

;; Sampling

(define (generate-samples s n #:burn [burn 0] #:thin [thin 0])
  (cond [(sampler? s)
         (for ([_i (in-range burn)]) (send s sample))
         (define vs (make-vector n))
         (for ([i (in-range n)])
           (for ([_ (in-range thin)]) (send s sample))
           (vector-set! vs i (send s sample)))
         vs]
        [(weighted-sampler? s)
         (define vs (make-vector n))
         (define ws (make-vector n))
         (for ([i (in-range n)])
           (defmatch (cons v w) (send s sample/weight))
           (vector-set! vs i v)
           (vector-set! ws i w))
         (resample-residual vs ws n)]))

(define (generate-weighted-samples s n #:burn [burn 0] #:thin [thin 0])
  (for ([_i (in-range burn)]) (send s sample))
  (define wvs (make-vector n))
  (for ([i (in-range n)])
    (for ([_ (in-range thin)]) (send s sample))
    (vector-set! wvs i (send s sample)))
  wvs)

;; ----------------------------------------

;; Resampling

(define (resample vs ws count #:alg [alg 'multinomial])
  (case alg
    [(multinomial #f) (resample-multinomial vs ws count)]
    [(residual) (resample-residual vs ws count)]
    [else (error 'resample "bad resampling algorithm\n  got: ~e" alg)]))

(define (resample-multinomial vs ws count)
  (define d (make-discrete-dist* vs ws #:normalize? #f #:sort? #f))
  (define r (make-vector count #f))
  (for ([i (in-range count)])
    (vector-set! r i (dist-sample d)))
  r)

(define (resample-residual vs ws count)
  (define r (make-vector count #f))
  (define wsum (for/sum ([w (in-vector ws)]) w))
  ;; Conceptually, rweights is first scaled s.t. sum is count*,
  ;; then whole parts are taken out, leaving fractions (residuals).
  ;; Actually done in one loop.
  (define rweights (make-vector (vector-length ws) #f))
  (define nextj
    (for/fold ([j 0]) ;; next avail index into r
              ([wi (in-vector ws)]
               [si (in-vector vs)]
               [i (in-naturals)])
      (define scaled (* (/ wi wsum) count))
      (define whole (inexact->exact (floor scaled)))
      (define residual (- scaled (floor scaled)))
      (vector-set! rweights i residual)
      (for ([joffset (in-range whole)])
        (vector-set! r (+ j joffset) si))
      (+ j whole)))
  ;; Now fill in (count* - nextj) final elements of states*.
  (define d (make-discrete-dist* vs ws #:normalize? #f #:sort? #f))
  (for ([j (in-range nextj count)])
    (vector-set! r j (dist-sample d)))
  r)

;; ----------------------------------------

;; Misc utils

(define (repeat thunk times)
  (for/list ([i times]) (thunk)))

(define (probability? x)
  (and (real? x) (<= 0 x 1)))
