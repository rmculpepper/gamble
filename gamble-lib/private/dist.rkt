;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base

;; ----------------------------------------
(require "dist/base.rkt")
(provide dist?
         dist-sample
         dist-pdf
         dist-density
         dist-measure
         dist-total-measure
         dist-count
         dist-type

         enumerable-dist?
         finite-dist?
         in-dist
         in-dist-values

         numeric-dist?
         dist-cdf
         dist-inv-cdf
         dist-support
         dist-mean
         dist-median
         dist-modes
         dist-variance

         real-dist?
         integer-dist?

         (struct-out integer-range)
         (struct-out real-range))

;; ----------------------------------------
(require "dist/discrete.rkt")
(provide (struct-out boolean-dist)

         discrete-dist?
         hash->discrete-dist
         empty-discrete-dist
         ;; dirac-dist
         discrete-dist-values
         discrete-dist-weights
         discrete-dist->hash
         ;; in-discrete-dist
         for/discrete-dist
         for*/discrete-dist
         (rename-out [m:discrete-dist discrete-dist])
         make-discrete-dist
         dist-discretize
         discrete-dist-resample
         discrete-distof)

;; ----------------------------------------
(require "dist/monad.rkt")
(provide dist-unit
         dist-fmap
         dist-bind
         dist-bindx
         dist-rescore
         dist-filter
         dist-join)

;; ----------------------------------------
(require "dist/univariate.rkt")
(provide (struct-out beta-dist)
         (struct-out cauchy-dist)
         (struct-out exponential-dist)
         (struct-out gamma-dist)
         (struct-out logistic-dist)
         (struct-out normal-dist)
         (struct-out uniform-dist)
         (struct-out triangle-dist)
         (struct-out pareto-dist)
         (struct-out student-t-dist)

         (struct-out binomial-dist)
         (struct-out geometric-dist)
         (struct-out poisson-dist)
         (struct-out bernoulli-dist)
         (struct-out negative-binomial-dist)
         (struct-out categorical-dist))

;; ----------------------------------------
(require "dist/multivariate.rkt")
(provide (struct-out multinomial-dist)
         (struct-out dirichlet-dist))

;; ----------------------------------------
(require "dist/transformer.rkt")
(provide (struct-out mixture-distx)
         (struct-out affine-distx)
         (struct-out clip-distx)
         (struct-out exp-distx))

;; ----------------------------------------

(module+ meta
  (require racket/match)
  (provide conjugate-dist?
           dist-posterior
           function=>symbol)

  ;; Names of likelihood functions used by dist-{conjugate,posterior}.
  (define function=>symbol
    (hasheq bernoulli-dist   'bernoulli-dist
            beta-dist        'beta-dist
            binomial-dist    'binomial-dist
            boolean-dist     'boolean-dist
            categorical-dist 'categorical-dist
            cauchy-dist      'cauchy-dist
            dirichlet-dist   'dirichlet-dist
            exponential-dist 'exponential-dist
            gamma-dist       'gamma-dist
            geometric-dist   'geometric-dist
            logistic-dist    'logistic-dist
            normal-dist      'normal-dist
            pareto-dist      'pareto-dist
            poisson-dist     'poisson-dist
            student-t-dist   'student-t-dist
            uniform-dist     'uniform-dist
            ))

  ;; dist-posterior : Dist Pattern (Vector X) -> Dist
  (define (dist-posterior dist xdistp xs)
    (cond [(zero? (vector-length xs))
           dist]
          [(and (conjugate-dist? dist)
                (-conjugate dist xdistp xs))
           => values]
          [(uniform-dist? dist)
           (match-define (uniform-dist lo hi) dist)
           (let ([d (likelihood-pattern->dist xdistp xs)])
             (and d (clip-distx d lo hi)))]
          [else #f]))

  ;; likelihood-pattern->dist : Pattern (Vectorof X) -> Dist
  ;; Requires f(x;y,z) = f(y;x,z), where y is '_ parameter.
  ;; Only handle continuous dists.
  (define (likelihood-pattern->dist xdistp xs)
    (match* [xdistp xs]
      [[`(normal-dist _ ,xs-scale) xs]
       (define n (vector-length xs))
       (normal-dist (/ (vector-sum xs) n) (/ xs-scale (sqrt n)))]
      [[`(cauchy-dist _ ,s) (vector x)]
       (cauchy-dist x s)]
      [[`(student-t-dist ,d _ ,s) (vector x)]
       (student-t-dist d x s)]
      [[_ _] #f])))
