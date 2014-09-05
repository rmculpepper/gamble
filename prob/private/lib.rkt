;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Aggregating module for library functions and syntax.
;; Doesn't include instrumentor.

#lang racket/base
(require racket/contract
         "util.rkt"
         "interfaces.rkt"
         "prob-util.rkt"
         "prob-syntax.rkt")
(provide (all-from-out "util.rkt")
         weighted-sampler?
         sampler?
         (contract-out
          [generate-samples
           (-> sampler? exact-nonnegative-integer? any)]
          [generate-weighted-samples
           (-> weighted-sampler? exact-nonnegative-integer? any)])
         (all-from-out "prob-util.rkt")
         (except-out (all-from-out "prob-syntax.rkt")
                     importance-stochastic-ctx%))
