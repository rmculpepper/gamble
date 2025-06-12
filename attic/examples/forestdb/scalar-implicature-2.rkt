#lang gamble
(require "church-compat.rkt"
         gamble/viz)

;; Ported from http://forestdb.org/models/scalar-implicature.html
;; 2nd code example

(define (call p) (p))

;; Other changes:
;; - change sample -> call
;; - swap order of args to repeat

;; ----------------------------------------

;; A State is (list Bool Bool Bool)

(define (baserate) 0.8)

(define (substate-priors)
  (list (lambda () (flip (baserate)))
        (lambda () (flip (baserate)))
        (lambda () (flip (baserate)))))

(define (state-prior)
  (map call (substate-priors)))

;; A Belief is (list Bool Bool Bool)

(define (belief actual-state access)
  (map (lambda (ac st pr) (if ac st (call pr)))
       access
       actual-state
       (substate-priors)))

;; A Sentence is (Listof Bool) -> Bool

(define (sentence-prior)
  (uniform-draw (list all-p some-p none-p)))

(define (all-p state) (all state))
(define (some-p state) (any state))
(define (none-p state) (not (some-p state)))

(define (speaker access state depth)
  (rejection-query
   (define sentence (sentence-prior))
   sentence
   #:when
   (equal? (belief state access)
           (listener access sentence depth))))

(define (listener speaker-access sentence depth)
  (rejection-query
   (define state (state-prior))
   state
   #:when
   (if (= 0 depth)
       (sentence state)
       (equal? sentence
               (speaker speaker-access state (- depth 1))))))

(define (num-true state)
  (sum (map (lambda (x) (if x 1 0)) state)))

(define (show thunk)
  (hist (repeat thunk 100)))

;; without full knowledge:
(show (lambda () (num-true (listener '(#t #t #f) some-p 1))))

;; with full knowledge:
(show (lambda () (num-true (listener '(#t #t #t) some-p 1))))
