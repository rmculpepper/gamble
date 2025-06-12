;; Copyright (c) 2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Helper module for observe.rkt --- just defines contracted function.

#lang racket/base
(require racket/contract
         gamble/private/instrument-data)

(define (add1/ctc n) (+ n 1))

(provide/contract
 [add1/ctc (-> exact-integer? exact-integer?)])

(declare-observation-propagator (add1/ctc _)
  exact-integer?
  (lambda (y) (- y 1))
  (lambda (x) 1))
