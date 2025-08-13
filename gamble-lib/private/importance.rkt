;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         "dist/base.rkt"
         "interfaces.rkt"
         "util/density.rkt")
(provide (all-defined-out))

;; ============================================================
;; Importance sampling

(define (importance-sampler thunk)
  (new importance-sampler% (thunk thunk)))

(define importance-sampler%
  (class weighted-sampler-base%
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

    (define/override (sample/weight)
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
         (values v weight)]
        [#f
         (set! rejections (add1 rejections))
         (sample/weight)]))
    ))

(define importance-stochastic-ctx%
  (class plain-stochastic-ctx%
    (field [obs-dn one-density])
    (inherit fail)
    (super-new)

    (define/public (get-observation-density) obs-dn)

    (define/override (dscore dn)
      (when (density-zero? dn) (fail))
      (set! obs-dn (density* obs-dn dn)))
    ))
