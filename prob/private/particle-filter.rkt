;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         racket/math
         racket/vector
         "../dist.rkt"
         "interfaces.rkt"
         "prob-syntax.rkt")
(provide (all-defined-out))

(define (make-particles count [initial-state #f])
  (new particles%
       (states (make-vector count initial-state))
       (weights (make-vector count 1))))

(define (particles? ps)
  (is-a? ps particles%))

(define (particles-count ps)
  (vector-length (get-field states ps)))

(define (particles-update ps f [iters 1])
  (send ps update f iters))

(define (particles-resample ps [count (particles-count ps)]
                            #:resample [technique 'multinomial])
  (case technique
    [(multinomial #f) (send ps resample count)]
    [(residual) (send ps resample/residual count)]
    [else (error 'particles-resample "bad algorithm\n  given: ~e" technique)]))

(define (particles-effective-count ps)
  (send ps effective-sample-size))
(define (particles-effective-ratio ps)
  (/ (particles-effective-count ps)
     (particles-count ps)))

(define (particles-states ps)
  (vector-map car (particles-weighted-states ps)))

;; particles-weighted-states : Particles -> (Vectorof (cons State Real))
(define (particles-weighted-states ps)
  (send ps get-weighted-states))

;; in-particles : Particles -> (Sequenceof (Values State Real))
;; FIXME: add in-particles sequence syntax?
(define (in-particles ps)
  (define wstates (send ps get-weighted-states))
  (make-do-sequence
   (lambda ()
     (values
      ;; pos -> element
      (lambda (i) (let ([wst (vector-ref wstates i)]) (values (car wst) (cdr wst))))
      ;; next-pos
      add1
      ;; initial pos
      0
      ;; continue-with/after-pos/val
      (lambda (i) (< i (vector-length wstates)))
      #f #f))))

;; ----

(define particles%
  (class* object% ()
    ;; states : (Vectorof State)
    ;; weights : (Vectorof Real)
    (init-field states weights)
    (unless (= (vector-length states) (vector-length weights))
      (error 'particles% "states and weights have different lengths"))
    (define ctx (new importance-stochastic-ctx%))
    (super-new)

    (define/public (get-states) states)
    (define/public (get-weights) weights)
    (define/public (to-discrete-dist)
      (make-discrete-dist* states weights))

    ;; update : (State -> State) Nat -> ParticleSet
    (define/public (update f iters)
      (cond [(zero? iters)
             this]
            [else
             (define count (vector-length states))
             (define states* (make-vector count #f))
             (define weights* (make-vector count 1))
             (update1 f count states weights states* weights*)
             (for ([i (in-range (sub1 iters))])
               (update1 f count states* weights* states* weights*))
             (new particles% (states states*) (weights weights*))]))

    (define/private (update1 f count states weights states* weights*)
      (for ([i (in-range count)]
            [si (in-vector states)]
            [wi (in-vector weights)])
        (cond [(positive? wi)
               (set-field! weight ctx 1)
               (match (send ctx run (lambda () (f si)))
                 [(cons 'okay si*)
                  (vector-set! states* i si*)
                  (vector-set! weights* i (* wi (get-field weight ctx)))]
                 [(cons 'fail _)
                  (vector-set! weights* i 0)])]
              [else
               (vector-set! weights* i 0)])))

    (define/public (get-weighted-states)
      (for/vector ([si (in-vector states)]
                   [wi (in-vector weights)]
                   #:when (positive? wi))
        (cons si wi)))

    (define/public (effective-sample-size)
      ;; Neff = 1 / (SUM (sqr normalized-weight))
      ;;      = (sqr wsum) / (SUM (sqr unnormalized-weights))
      (/ (sqr (for/sum ([w (in-vector weights)]) w))
         (for/sum ([w (in-vector weights)]) (sqr w))))

    (define/public (resample [count* (vector-length states)])
      (define d (make-discrete-dist* states weights #:normalize? #f))
      (define states* (make-vector count* #f))
      (define weights* (make-vector count* 1))
      (for ([i (in-range count*)])
        (vector-set! states* i (dist-sample d)))
      (new particles% (states states*) (weights weights*)))

    (define/public (resample/residual [count* (vector-length states)])
      (define states* (make-vector count* #f))
      (define wsum (for/sum ([w (in-vector weights)]) w))
      ;; Conceptually, rweights is first scaled s.t. sum is count*,
      ;; then whole parts are taken out, leaving fractions (residuals).
      ;; Actually done in one loop.
      (define rweights (make-vector (vector-length weights) #f))
      (define next-statej
        (for/fold ([statej 0]) ;; next avail index into states*
                  ([wi (in-vector weights)]
                   [si (in-vector states)]
                   [i (in-naturals)])
          (define scaled (* (/ wi wsum) count*))
          (define whole (inexact->exact (floor scaled)))
          (define residual (- scaled (floor scaled)))
          (vector-set! rweights i residual)
          (for ([j (in-range whole)])
            (vector-set! states* (+ statej j) si))
          (+ statej whole)))
      ;; (eprintf "-- ~s guaranteed, ~s residual\n" next-statej (- count* next-statej))
      ;; Now fill in (count* - state-nextj) final elements of states*.
      (define d (make-discrete-dist* states rweights #:normalize? #f))
      (for ([i (in-range next-statej count*)])
        (vector-set! states* i (dist-sample d)))
      (define weights* (make-vector count* 1))
      (new particles% (states states*) (weights weights*)))
    ))
