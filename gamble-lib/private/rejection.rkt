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

(define (rejection-sampler mdl)
  (new rejection-sampler% (mdl mdl)))

(define rejection-sampler%
  (class sampler-base%
    (init-field mdl)
    (super-new)

    (define/override (sample)
      (define ctx (new rejection-stochastic-ctx%))
      (match (send ctx run-top mdl)
        [(list v) v]
        [#f (sample)]))
    ))

(define rejection-stochastic-ctx%
  (class plain-stochastic-ctx%
    (super-new)

    (define/override (dscore dn)
      (error 'dscore "not supported by rejection sampler"))
    ))
