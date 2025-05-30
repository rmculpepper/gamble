;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         "../util/real.rkt")
(provide (all-defined-out))

;; Density = NNReal | (lebesgue-density NNReal Nat)

(define (density? v)
  (or (and (rational? v) (>= v 0))
      (lebesgue-density? v)))

(define (density p ddim)
  (unless (and (rational? p) (>= p 0))
    (raise-argument-error 'density "(>=/c 0)" p))
  (unless (exact-positive-integer? ddim)
    (raise-argument-error 'density "exact-positive-integer?" ddim))
  (cond [(zero? ddim) p]
        [(and (zero? p) (= ddim 1)) zero-lebesgue-density]
        [else (lebesgue-density p ddim)]))

(struct lebesgue-density (p ddim) #:transparent
  #:guard (lambda (d ddim _name)
            (unless (and (rational? d) (>= d 0))
              (raise-argument-error 'lebesgue-density "(>=/c 0)" d))
            (unless (exact-positive-integer? ddim)
              (raise-argument-error 'lebesgue-density "exact-positive-integer?" ddim))
            (values d ddim)))

(define zero-mass-density 0)
(define zero-lebesgue-density (lebesgue-density 0 1))

(define (density* d1 d2)
  (match* [d1 d2]
    [[(? rational? p1) (? rational? p2)]
     (* p1 p2)]
    [[(? rational? p1) (lebesgue-density p2 ddim2)]
     (lebesgue-density (* p1 p2) ddim2)]
    [[(lebesgue-density p1 ddim1) (? rational? p2)]
     (lebesgue-density (* p1 p2) ddim1)]
    [[(lebesgue-density p1 ddim1) (lebesgue-density p2 ddim2)]
     (lebesgue-density (* p1 p2) (+ ddim1 ddim2))]
    [[_ _]
     (unless (density? d1) (raise-argument-error 'density* "density?" d1))
     (unless (density? d2) (raise-argument-error 'density* "density?" d2))]))

(define (density-product ds)
  (unless (and (list? ds) (andmap density? ds))
    (raise-argument-error 'density-product "(listof density?)" ds))
  (foldl density* 1 ds))

(define (density+ d1 d2)
  (define (bad)
    (error 'density+ "cannot add incompatible densities\n  given: ~e, ~e" d1 d2))
  (match* [d1 d2]
    [[(? rational? p1) (? rational? p2)]
     (+ p1 p2)]
    [[(lebesgue-density p1 ddim1) (lebesgue-density p2 ddim2)]
     (unless (= ddim1 ddim2) (bad))
     (lebesgue-density (+ p1 p2) ddim1)]
    [[_ _]
     (unless (density? d1) (raise-argument-error 'density* "density?" d1))
     (unless (density? d2) (raise-argument-error 'density* "density?" d2))
     (bad)]))

(define (density-sum ds)
  (unless (and (pair? ds) (list? ds) (andmap density? ds))
    (raise-argument-error 'density-sum "(nonempty-listof density?)" ds))
  (foldl density+ (car ds) (cdr ds)))

#|
(define (density-cmp d1 d2)
  (match-define (density n1 ddim1) d1)
  (match-define (density n2 ddim2) d2)
  (cond [(= ddim1 ddim2)
         (cond [(> n1 n2) '>]
               [(< n1 n2) '<]
               [else '=])]
        [else #f]))

;; density-logratio : Density Density -> Real
(define (density-logratio d1 d2)
  (match-define (density _ ll1 ddim1) d1)
  (match-define (density _ ll2 ddim2) d2)
  (cond [(< ddim1 ddim2) +inf.0]
        [(> ddim1 ddim2) -inf.0]
        [else (- ll1 ll2)]))

(define (ilog x) (log (exact->inexact x))) ;; avoid error on exact 0
|#
