;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/generic
         (submod racket/performance-hint begin-encourage-inline))
(provide (all-defined-out))

;; ============================================================
;; Logspace

;; http://hips.seas.harvard.edu/blog/2013/01/09/computing-log-sum-exp/
;; http://machineintelligence.tumblr.com/post/4998477107/the-log-sum-exp-trick

(begin-encourage-inline

;; logspace+ : Real Real -> Real
;; Like (log (+ (exp x) (exp y))), but with better precision.
(define (logspace+ x y)
  (let ([M (max x y)])
    (if (= M -inf.0)
        -inf.0 ;; avoid +nan.0 from subtraction
        (+ M (log (+ (exp (- x M)) (exp (- y M))))))))

;; logspace-sum : (Listof Real) -> Real
(define (logspace-sum xs)
  (let ([M (apply max -inf.0 xs)])
    (if (= M -inf.0)
        -inf.0 ;; avoid +nan.0 from subtraction
        (+ M (log (for/sum ([x (in-list xs)]) (exp (- x M))))))))

;; logspace-{zero,nonzero}? : Real -> Boolean
;; Returns #t if (exp ll) is {zero, positive (ie, non-zero)}.
(define (logspace-zero? x) (= x -inf.0))
(define (logspace-nonzero? x) (> x -inf.0))

)

;; ============================================================
