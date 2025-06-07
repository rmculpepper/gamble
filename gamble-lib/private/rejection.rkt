;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
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
