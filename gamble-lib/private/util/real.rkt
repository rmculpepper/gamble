;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require (only-in racket/base
                  [real->double-flonum fl]
                  [exact->inexact inexact] [inexact->exact exact])
         (submod racket/performance-hint begin-encourage-inline))
(provide (all-defined-out) fl exact inexact xexact)

(begin-encourage-inline
  (define (probability? v)
    (and (real? v) (<= 0 v 1)))
  (define (nontrivial-probability? v)
    (and (real? v) (< 0 v 1)))
  (define (nonnegative-real? v)
    (and (real? v) (>= v 0)))
  (define (positive-real? v)
    (and (real? v) (> v 0)))
  (define (nonnegative-rational? v)
    (and (rational? v) (>= v 0)))
  (define (positive-rational? v)
    (and (rational? v) (> v 0)))
  (define (xexact x)
    (if (rational? x) (exact x) x)))

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

;; logspace- : Real Real -> Real
;; Like (log (- (exp x) (exp y))), but with better precision.
(define (logspace- x y)
  (let ([M (max x y)])
    (if (= M -inf.0)
        -inf.0 ;; avoid +nan.0 from subtraction
        (+ M (log (- (exp (- x M)) (exp (- y M))))))))

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

(begin-encourage-inline
  ;; Reference: https://en.wikipedia.org/wiki/Kahan_summation_algorithm

  ;; compensated+ : Flonum Flonum Flonum -> (values Flonum Flonum)
  ;; Add value x to sum s and compensator c, produce new sum and compensator.
  (define (compensated+ x s c)
    (define y (- x c))
    (define t (+ s y))
    (values t (- (- t s) y)))
  ;; compensated-sum : (Listof Flonum) -> (values Flonum Flonum)
  (define (compensated-sum xs)
    (for/fold ([s 0.0] [c 0.0]) ([x (in-list xs)])
      (compensated+ s c x))))
