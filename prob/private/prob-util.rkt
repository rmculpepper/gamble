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
