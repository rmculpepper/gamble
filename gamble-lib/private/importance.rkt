;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         "dist/base.rkt"
         "base.rkt"
         "addr.rkt"
         "util/density.rkt")
(provide (all-defined-out))

;; ============================================================
;; Importance sampling

(define (importance-sampler mdl
                            #:propose [propose #f])
  (unless (or (model? mdl) (procedure? mdl))
    (raise-argument-error 'importance-sampler "(or/c model? (-> any/c))" mdl))
  (new importance-sampler% (mdl mdl) (propose propose)))

(define importance-sampler%
  (class weighted-sampler-base%
    (init-field mdl propose)
    (super-new)

    (define/override (sample/weight)
      (define ctx (new importance-stochastic-ctx% (propose propose)))
      (match (parameterize ((current-stochastic-ctx ctx))
               (send ctx run-top mdl))
        [(list v)
         (define obs-dn (send ctx get-observation-density))
         (define weight (density->real obs-dn #f))
         (values v weight)]
        [#f
         (sample/weight)]))
    ))

(define importance-stochastic-ctx%
  (class scoring-stochastic-ctx%
    (init-field propose) ;; #f or (Tag/#f Dist -> Dist/#f)
    (inherit -dscore)
    (super-new)

    (define/override (sample dist tag addr)
      (cond [(and propose (propose tag dist))
             => (lambda (qdist)
                  (define v (dist-sample qdist))
                  (-dscore 'sample
                           (density/ (dist-density dist v)
                                     (dist-density qdist v)))
                  v)]
            [else (super sample dist tag addr)]))
    ))
