;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         "real.rkt")
(provide (all-defined-out))

;; Dnum = (dnum #f NNReal) | (dnum #t Flonum)
;; A Dnum (dimorphic number) represents a nonnegative real either
;; - in linspace (linear space) -- eg, 1.0 is represented as (dnum #f 1.0)
;; - in logspace (logarithmic space) -- eg, 1.0 is represented as (dnum #t 0.0)
;; Linspace representation can be exact or inexact; logspace must be flonum.
;; Mixed operations coerce to logspace.
(struct dnum (log? x) #:transparent)

(define (dnum-logspace? dn) (dnum-log? dn))
(define (dnum-linear? dn) (not (dnum-log? dn)))

(define (real->dnum x [log? #f])
  (if log? (logspace-dnum x) (linear-dnum x)))

(define (linear-dnum x)
  (unless (and (real? x) (>= x 0))
    (raise-argument-error 'linear-dnum "(>=/c 0)" x))
  (dnum #f x))

(define (logspace-dnum x)
  (unless (real? x)
    (raise-argument-error 'logspace-dnum "real?" x))
  (dnum #t x))

(define (dnum->real dn [log? #f])
  (if log? (dnum->logspace-real dn) (dnum->linear-real dn)))

(define (dnum->linear-real dn)
  (match-define (dnum dnlog? x) dn)
  (if dnlog? (exp x) x))

(define (dnum->logspace-real dn)
  (match-define (dnum dnlog? x) dn)
  (if dnlog? x (log (fl x))))

;; ----------------------------------------

;; dnum-zero? : Dnum -> Boolean
(define (dnum-zero? dn)
  (match-define (dnum log? x) dn)
  (if log? (= x -inf.0) (zero? x)))

;; dnum+ : Dnum Dnum/#f -> Dnum
(define (dnum+ dn1 dn2)
  (match* [dn1 dn2]
    [[(dnum log1? x1) (dnum log2? x2)]
     (dnum (or log1? log2?)
           (cond [(and log1? log2?) (logspace+ x1 x2)]
                 [log1? (logspace+ x1 (log (fl x2)))]
                 [log2? (logspace+ (log (fl x1)) x2)]
                 [else (+ x1 x2)]))]
    [[dn1 #f] dn1]))

;; dnum* : Dnum Dnum -> Dnum
(define (dnum* d1 d2)
  (match* [d1 d2]
    [[(dnum log1? x1) (dnum log2? x2)]
     (dnum (or log1? log2?)
           (cond [(and log1? log2?) (+ x1 x2)]
                 [log1? (+ x1 (log (fl x2)))]
                 [log2? (+ (log (fl x1)) x2)]
                 [else (* x1 x2)]))]))

;; dnum- : Dnum Dnum/#f -> Dnum
(define (dnum- dn1 dn2)
  (define (bad)
    (raise-arguments-error 'dnum- "result would be negative"
                           "dn1" dn1 "dn2" dn2))
  (match* [dn1 dn2]
    [[(dnum #f x1) (dnum #f x2)]
     (unless (>= x1 x2) (bad))
     (dnum #f (- x1 x2))]
    [[(dnum log1? x1) (dnum log2? x2)]
     (define lx1 (if log1? x1 (log (fl x1))))
     (define lx2 (if log2? x2 (log (fl x2))))
     (unless (>= x1 x2) (bad))
     (dnum #t (logspace- lx1 lx2))]
    [[dn1 #f] dn1]))

;; dnum/ : Dnum Dnum -> Dnum
;; Note: if exact linear, can raise division by zero
(define (dnum/ d1 d2)
  (match* [d1 d2]
    [[(dnum #f x1) (dnum #f x2)]
     (when (eqv? x2 0)
       (error 'dnum/ "division by zero\n  x: ~e\n  y: ~e" d1 d2))
     (define q (/ x1 x2))
     (when (eqv? q +nan.0)
       (error 'dnum/ "division produced NaN\n  x: ~e\n  y: ~e" d1 d2))
     (dnum #f q)]
    [[(dnum log1? x1) (dnum log2? x2)]
     (define lx1 (if log1? x1 (log (fl x1))))
     (define lx2 (if log2? x2 (log (fl x2))))
     (define lq (- lx1 lx2))
     (when (eqv? lq +nan.0)
       (error 'dnum/ "division produced NaN\n  x: ~e\n  y: ~e" d1 d2))
     (dnum #t lq)]))

;; dnum-sum : (Listof Dnum) -> Dnum
(define (dnum-sum dns)
  (if (ormap dnum-log? dns)
      (dnum #t (logspace-sum (map dnum->logspace-real dns)))
      (dnum #f (apply + (map dnum->linear-real dns)))))

;; dnum-product : (Listof Dnum) -> Dnum
(define (dnum-product dns)
  (if (ormap dnum-log? dns)
      (dnum #t (apply + (map dnum->logspace-real dns)))
      (dnum #f (apply * (map dnum->linear-real dns)))))

;; dnum<=? : Dnum Dnum -> Boolean
(define (dnum<=? dn1 dn2)
  (match-define (dnum log1? x1) dn1)
  (match-define (dnum log2? x2) dn2)
  (cond [(and log1? log2?) (<= x1 x2)]
        [log1? (<= x1 (log (fl x2)))]
        [log2? (<= (log (fl x1)) x2)]
        [else (<= x1 x2)]))

;; dnum=? : Dnum Dnum -> Boolean
(define (dnum=? dn1 dn2)
  (match-define (dnum log1? x1) dn1)
  (match-define (dnum log2? x2) dn2)
  (cond [(and log1? log2?) (= x1 x2)]
        [log1? (= x1 (log (fl x2)))]
        [log2? (= (log (fl x1)) x2)]
        [else (= x1 x2)]))

;; dnum<? : Dnum Dnum -> Boolean
(define (dnum<? dn1 dn2)
  (match-define (dnum log1? x1) dn1)
  (match-define (dnum log2? x2) dn2)
  (cond [(and log1? log2?) (< x1 x2)]
        [log1? (< x1 (log (fl x2)))]
        [log2? (< (log (fl x1)) x2)]
        [else (< x1 x2)]))
