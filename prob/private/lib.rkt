;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Aggregating module for library functions and syntax.
;; Doesn't include instrumentor.

#lang racket/base
(require "util.rkt"
         "prob-util.rkt"
         "prob-hooks.rkt"
         "prob-syntax.rkt")
(provide (all-from-out "util.rkt")
         (all-from-out "prob-util.rkt")
         (all-from-out "prob-syntax.rkt"))

;; from prob-hooks.rkt
(provide dist?
         dist-pdf
         dist-cdf
         dist-inv-cdf
         dist-sample
         dist-sample-n
         dist-enum
         make-bernoulli-dist
         make-binomial-dist
         make-geometric-dist
         make-poisson-dist 
         make-beta-dist 
         make-cauchy-dist
         make-exponential-dist
         make-gamma-dist
         make-logistic-dist
         make-normal-dist
         make-uniform-dist
         make-discrete-dist)
