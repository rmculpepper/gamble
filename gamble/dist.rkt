;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         "private/dist.rkt"
         "dist/univariate.rkt"
         "dist/multivariate.rkt"
         "dist/discrete.rkt"
         "dist/permutation.rkt"
         "dist/mixture.rkt")
(provide dist?
         integer-dist?
         real-dist?
         finite-dist?
         (contract-out
          [dist-param-count (-> dist? exact-nonnegative-integer?)]
          [dist-pdf
           (->* [dist? any/c] [any/c] real?)]
          [dist-cdf
           (->* [dist? any/c] [any/c any/c] real?)]
          [dist-inv-cdf
           (->* [dist? (real-in 0 1)] [any/c any/c] any)]
          [dist-sample
           (-> dist? any)]
          [dist-enum
           (-> dist? any)]
          [dist-has-mass?
           (-> dist? any)]
          [dist-drift
           (-> dist? any/c (>/c 0) any)]
          [dist-support
           (-> dist? any)]
          [dist-mean
           (-> dist? any)]
          [dist-median
           (-> dist? any)]
          [dist-modes
           (-> dist? any)]
          [dist-variance
           (-> dist? any)]
          [dist-energy
           (-> dist? any/c any)]
          [dist-Denergy
           (->* [dist? any/c] [] #:rest list? any)]
          [dist-update-prior
           (-> dist? any/c vector? (or/c dist? #f))])

         (all-from-out "dist/univariate.rkt")
         (all-from-out "dist/multivariate.rkt")
         (all-from-out "dist/discrete.rkt")
         (all-from-out "dist/permutation.rkt")
         (all-from-out "dist/mixture.rkt"))
