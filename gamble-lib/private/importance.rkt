;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/match
         racket/class
         "dist/base.rkt"
         "base.rkt"
         "addr.rkt"
         "util/dnum.rkt")
(provide (all-defined-out))

;; ============================================================
;; Importance sampling

(define (importance-sampler mdl
                            #:propose [propose #f])
  (unless (or (model? mdl) (procedure? mdl))
    (raise-argument-error 'importance-sampler "(or/c model? (-> any/c))" mdl))
  (new importance-sampler% (mdl mdl) (propose propose)))

(define importance-sampler%
  (class* object% (sampler<%>)
    (init-field mdl propose)
    (super-new)

    (define/public-final (sample/weight)
      (define ctx (new importance-stochastic-ctx% (propose propose)))
      (match (send ctx run-top mdl)
        [(list v) (values v (send ctx get-score-dnum))]
        [#f (sample/weight)]))

    (define/public (burn n)
      (for ([i (in-range n)]) (sample/weight)))

    (define/public (generate-samples n thin)
      (define vs (make-vector n))
      (define ws (make-vector n))
      (for ([i (in-range n)])
        (for ([j (in-range thin)])
          (sample/weight))
        (define-values (v wdn) (sample/weight))
        (vector-set! vs i v)
        (vector-set! ws i (dnum->logspace-real wdn)))
      (hash 'value vs 'log-weight ws))
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
                           (dnum/ (dist-density dist v)
                                  (dist-density qdist v)))
                  v)]
            [else (super sample dist tag addr)]))
    ))
