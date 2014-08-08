;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(provide verbose?
         repeat
         probability?)

;; Parameters

(define verbose? (make-parameter #f))

;; Misc utils

(define (repeat thunk times)
  (for/list ([i times]) (thunk)))

;; Predicate

(define (probability? x)
  (and (real? x) (<= 0 x 1)))
