;; Copyright 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

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

         real-dist?
         dist-cdf
         dist-inv-cdf
         dist-support

         continuous-dist?
         integer-dist?

         dist-drift1
         dist-drift2
         dist-drift-dist

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
         log-hash->normalized-discrete-dist
         (rename-out [m:discrete-dist discrete-dist]
                     [m:make-discrete-dist make-discrete-dist])
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
         (struct-out exp-distx)
         (struct-out discretize/floor-distx)
         (struct-out discretize/round-distx))
