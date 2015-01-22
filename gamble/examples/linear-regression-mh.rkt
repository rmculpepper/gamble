;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble
(require racket/class)

(define A 3)
(define B 12)
(define E 1)

(define XS 100)

;; Build data
(define ys (build-vector XS (lambda (x) (+ (* x A) B (normal 0 E)))))

;; Sampler
(define lr
  (mh-sampler
   (define A (normal 0 10))
   (define B (normal 0 10))
   (define E (gamma 2 2))
   (define (f x)
     (normal (+ (* A x) B) E))
   (for ([x XS])
     (observe (f x) (vector-ref ys x)))
   (vector A B E)))

(for ([i #e1e3]) (void (lr))) ;; burn in
(send lr info)

(send lr MAP-estimate #e1e3)
(send lr info)

(sampler->statistics lr #e1e3)
(send lr info)
