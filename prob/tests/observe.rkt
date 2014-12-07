;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang prob
(require racket/class
         (rename-in racket/match [match-define defmatch])
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

(define (sampler->mean s n)
  (define-values (sum weight)
    (for/fold ([sum 0.0] [weight 0.0])
              ([i (in-range n)])
      (defmatch (cons v w) (send s sample/weight))
      (values (+ sum (* v w)) (+ weight w))))
  (/ sum weight))

(let ([mean (sampler->mean s 1000)])
  (check-= mean 9.5 CD-EPSILON))
(let ([mean (sampler->mean s2 1000)])
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

;; works in enumerate
(check-equal? (enumerate (observe (+ 1 (bernoulli)) 2))
              (discrete-dist [2 1]))

;; ----

;; works through mem

(define (mem-test)
  (define f (mem (lambda (i) (bernoulli))))
  (for ([i 10]) (observe (f i) 0))
  (for/and ([i 10]) (= (f i) 0)))

(check-equal? (mem-test) #t)
(check-equal? (enumerate (mem-test))
              (discrete-dist [#t 1]))
(check-equal? ((mh-sampler (mem-test)))
              #t)
(check-equal? (send (importance-sampler (mem-test)) sample/weight)
              (cons #t (expt 1/2 10)))

;; ----

;; Non-conditionable expressions lead to relatively bad/useless error messages:
;; (observe (values (normal 0 1)) 0)

;; ----

;; check-observe -- used to check parts of expected value early

(check-not-exn
 (lambda ()
   (observe (cons (check-observe 1)
                  (cons (check-observe 2)
                        null))
            '(1 2))))

(check-exn
 #rx"check-observe:.*expected: 1.*got: 0"
 (lambda ()
   (observe (cons (check-observe 0) ;; !!!
                  (cons (check-observe 2)
                        null))
            '(1 2))))
