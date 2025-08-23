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

(define (importance-sampler mdl
                            #:propose [propose #f])
  (new importance-sampler% (mdl mdl) (propose propose)))

(define importance-sampler%
  (class weighted-sampler-base%
    (init-field mdl propose)
    (super-new)

    (define/override (sample/weight)
      (define ctx (new importance-stochastic-ctx% (propose propose)))
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
    (init-field propose) ;; #f or (Label/#f Dist -> Dist/#f)
    (field [obs-dn one-density])
    (inherit fail)
    (super-new)

    (define/public (get-observation-density) obs-dn)

    (define/override (sample dist label)
      (cond [(and propose (propose label dist))
             => (lambda (qdist)
                  (define v (dist-sample qdist))
                  (-dscore 'sample
                           (density/ (dist-density dist v)
                                     (dist-density qdist v)))
                  v)]
            [else (super sample dist label)]))

    (define/override (-dscore who dn)
      (set! obs-dn (density* obs-dn dn))
      (when (density-zero? obs-dn) (fail who)))
    ))
