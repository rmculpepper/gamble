#lang racket/base
(provide verbose?
         repeat
         lag
         probability?)

;; Parameters

(define verbose? (make-parameter #f))

;; Misc utils

(define (repeat thunk times)
  (for/list ([i times]) (thunk)))

(define (lag thunk n)
  (lambda () (for/last ([i n]) (thunk))))

;; Predicate

(define (probability? x)
  (and (real? x) (<= 0 x 1)))
