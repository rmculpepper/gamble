#lang racket
(require "pl1.rkt")

(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (flip) (sum-n-flips (sub1 n)))))

(define (sum-n-flips* n)
  (let loop ([n n] [acc 0])
    (if (zero? n)
        acc
        (loop (sub1 n) (+ acc (flip))))))

;(sum-n-flips 10)

(apply/log sum-n-flips* 10)
