;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang prob
(require racket/class)

(define (make-lr a b e n)
  (define ys (build-vector n (lambda (x) (+ (* x a) b (normal 0 e)))))
  (mh-sampler
   (define A (normal 0 10))
   (define B (normal 0 10))
   (define E (add1 (gamma 1 1)))
   (define (data x y)
     (observe-at (normal-dist (+ (* A x) B) E) y))
   (for ([x n])
     (data x (vector-ref ys x)))
   (vector A B E)))

(define lr (make-lr 3 12 1 100))

(for ([i #e1e3]) (void (lr))) ;; burn in
(send lr info)

(send lr MAP-estimate #e1e3)
(send lr info)

;;(sampler->means+covariance lr #e1e3)

(sampler->statistics lr #e1e3)
(send lr info)
