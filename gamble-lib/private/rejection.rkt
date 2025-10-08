;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/match
         racket/class
         "base.rkt")
(provide (all-defined-out))

;; ============================================================
;; Rejection sampling

(define (rejection-sampler mdl)
  (unless (or (model? mdl) (procedure? mdl))
    (raise-argument-error 'rejection-sampler "(or/c model? (-> any/c))" mdl))
  (new rejection-sampler% (mdl mdl)))

(define rejection-sampler%
  (class object% (sampler<%>)
    (init-field mdl)
    (super-new)

    (define/public-final (sample)
      (define ctx (new rejection-stochastic-ctx%))
      (match (send ctx run-top mdl)
        [(list v) v]
        [#f (sample)]))

    (define/public (burn n)
      (for ([i (in-range n)]) (sample)))

    (define/public (generate-samples n thin)
      (define vs (make-vector n))
      (for ([i (in-range n)])
        (for ([j (in-range thin)])
          (sample))
        (define v (sample))
        (vector-set! vs i v))
      (hash 'value vs))
    ))

(define rejection-stochastic-ctx%
  (class base-stochastic-ctx%
    (super-new)

    (define/override (-unsupported who)
      (error who "not supported by rejection sampler"))
    ))
