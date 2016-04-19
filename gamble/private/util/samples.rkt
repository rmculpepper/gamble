;; Copyright (c) 2016 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/vector
         (rename-in racket/match [match-define defmatch])
         "../interfaces.rkt"
         "../../dist.rkt")
(provide (all-defined-out))

;; ----------------------------------------
;; Unweighted samples

(define (generate-samples s n [f values] #:burn [burn 0] #:thin [thin 0])
  (cond [(sampler? s)
         (for ([_i (in-range burn)]) (send s sample))
         (define vs (make-vector n))
         (for ([i (in-range n)])
           (for ([_ (in-range thin)]) (send s sample))
           (vector-set! vs i (f (send s sample))))
         vs]
        [(weighted-sampler? s)
         (define wvs (generate-weighted-samples s n f #:burn burn #:thin thin))
         (define vs (vector-map car wvs))
         (define ws (vector-map cdr wvs))
         (resample-residual vs ws n)]))

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
;; Weighted samples

(define (generate-weighted-samples s n [f values] #:burn [burn 0] #:thin [thin 0])
  (for ([_i (in-range burn)]) (send s sample/weight))
  (define wvs (make-vector n))
  (for ([i (in-range n)])
    (for ([_ (in-range thin)]) (send s sample/weight))
    (defmatch (cons v w) (send s sample/weight))
    (vector-set! wvs i (cons (f v) w)))
  wvs)
