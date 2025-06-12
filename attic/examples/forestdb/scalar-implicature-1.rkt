#lang gamble
(require "church-compat.rkt"
         gamble/viz)

;; Ported from http://forestdb.org/models/scalar-implicature.html
;; 1st code example

;; ----------------------------------------

;; A State is a Nat in [0,3]

(define (state-prior) (discrete* '(0 1 2 3)))

;; A Sentence is Nat -> Bool

(define (sentence-prior)
  (uniform-draw (list all-sprouted some-sprouted none-sprouted)))

(define (all-sprouted state) (= 3 state))
(define (some-sprouted state) (< 0 state))
(define (none-sprouted state) (= 0 state))

(define (speaker state depth)
  (rejection-query
   (define words (sentence-prior))
   words
   #:when
   (equal? state (listener words depth))))

(define (listener words depth)
  (rejection-query
   (define state (state-prior))
   state
   #:when
   (if (= depth 0)
       (words state)
       (equal? words (speaker state (- depth 1))))))

(define depth 1)

(hist (repeat (lambda () (listener some-sprouted depth)) 300))
