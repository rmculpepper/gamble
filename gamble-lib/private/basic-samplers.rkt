;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/match
         racket/class
         "dist/base.rkt"
         (submod "dist/util.rkt" density)
         "util/debug.rkt"
         "interfaces.rkt")
(provide (all-defined-out))

;; ============================================================
;; Rejection sampling

(define (rejection-sampler thunk)
  (new rejection-sampler% (thunk thunk)))

(define rejection-sampler%
  (class sampler-base%
    (init-field thunk)
    (field [successes 0]
           [rejections 0])
    (super-new)

    (define/public (info)
      (printf "== Rejection sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections))

    (define/override (sample)
      (define ctx (new rejection-stochastic-ctx%))
      (define v (send ctx run thunk))
      (match (send ctx run thunk)
        [(list v) v]
        [#f (sample)]))
    ))

(define rejection-stochastic-ctx%
  (class plain-stochastic-ctx%
    (super-new)

    (define/override (dscore dn)
      (error 'dscore "not supported by rejection sampler"))
    ))


;; ============================================================
;; Importance sampling

(define (importance-sampler thunk)
  (new importance-sampler% (thunk thunk)))

(define importance-sampler%
  (class* object% (weighted-sampler<%>)
    (init-field thunk)
    (field [successes 0]
           [rejections 0]
           [success-ddim #f]
           [bad-samples 0])
    (super-new)

    (define/public (info)
      (printf "== Importance sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections)
      (unless (zero? successes)
        (printf "Density dimension: ~a\n" success-ddim))
      (unless (zero? bad-samples)
        (printf "Bad samples emitted (wrong density dimension): ~s" bad-samples)))

    (define/public (sample/weight)
      (unless (zero? bad-samples)
        (error 'importance-sampler
               "invalid sampler; observation density dimension varies"))
      (define ctx (new importance-stochastic-ctx%))
      (match (send ctx run thunk)
        [(list v)
         (define obs-dn (send ctx get-observation-density))
         (define ddim (density-ddim obs-dn))
         (define weight (density->real obs-dn #f))
         (cond [(zero? successes)
                (set! success-ddim ddim)]
               [(not (= ddim success-ddim))
                (set! bad-samples (+ bad-samples successes))
                (unless (zero? successes)
                  (error 'importance-sampler
                         (string-append "observation density dimension changed"
                                        "\n  previous: ~e\n  current: ~e")
                         success-ddim ddim))])
         (set! successes (add1 successes))
         (cons v weight)]
        [#f
         (set! rejections (add1 rejections))
         #f]))
    ))

(define importance-stochastic-ctx%
  (class rejection-stochastic-ctx%
    (field [obs-dn one-density])
    (inherit fail)
    (super-new)

    (define/public (get-observation-density) obs-dn)

    (define/override (dscore dn)
      (when (density-zero? dn) (fail))
      (set! obs-dn (density* obs-dn dn)))
    ))
