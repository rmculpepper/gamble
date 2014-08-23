;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang prob
(require racket/class
         rackunit)

;; This module tests the observe form.

(define CD-EPSILON 0.1)

;; sample in tail position wrt observe
(define s 
  (importance-sampler 
   (define R (normal 10 1))
   (define (S) (normal R 1))
   (observe (S) 9)
   R))

;; sample in (+ v []) wrt observe
(define s2
  (importance-sampler
   (define R (normal 10 1))
   (define (S) (+ R (normal 0 1)))
   (observe (S) 9)
   R))

(let-values ([(mean variance) (sampler->mean+variance s 1000)])
  (check-= mean 9.5 CD-EPSILON))
(let-values ([(mean variance) (sampler->mean+variance s2 1000)])
  (check-= mean 9.5 CD-EPSILON))

;; ----

;; conditioning on lists

(define s-list
  (importance-sampler
   ;; note: annotated version of build-list from prob/private/ho-functions
   (define (data) (build-list 3 (lambda (_i) (bernoulli 1/2))))
   (observe (data) '(0 0 0))))

(check-equal? (send s-list sample/weight)
              '((0 0 0) . 1/8))

;; ----

;; Non-conditionable expressions lead to relatively bad/useless error messages:
;; (observe (values (normal 0 1)) 0)
