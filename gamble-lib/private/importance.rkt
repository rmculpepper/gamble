;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         "dist/base.rkt"
         "base.rkt"
         "util/density.rkt")
(provide (all-defined-out))

;; ============================================================
;; Importance sampling

(define (importance-sampler mdl)
  (new importance-sampler% (mdl mdl)))

(define importance-sampler%
  (class weighted-sampler-base%
    (init-field mdl)
    (super-new)

    (define/override (sample/weight)
      (define ctx (new importance-stochastic-ctx%))
      (match (send ctx run-top mdl)
        [(list v)
         (define obs-dn (send ctx get-observation-density))
         (define weight (density->real obs-dn #f))
         (values v weight)]
        [#f
         (sample/weight)]))
    ))

(define importance-stochastic-ctx%
  (class plain-stochastic-ctx%
    (field [obs-dn one-density])
    (inherit fail)
    (super-new)

    (define/public (get-observation-density) obs-dn)

    (define/override (-dscore who dn)
      (set! obs-dn (density* obs-dn dn))
      (when (density-zero? obs-dn) (fail who)))
    ))
