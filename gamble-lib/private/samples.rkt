;; Copyright (c) 2016 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/vector
         racket/match
         "dist.rkt"
         (submod "dist/util.rkt" search))
(provide (all-defined-out))

;; ------------------------------------------------------------
;; Empirical CDF

(define (vector->empirical-cdf v)
  (sorted->empirical-cdf (vector-sort v <)))
(define (sorted->empirical-cdf sv)
  (define (ecdf x)
    (cond [(>= (vector-ref sv 0) x)
           (/ (binary-search/least-geq sv x)
              (vector-length sv))]
          [else 0]))
  ecdf)

;; ------------------------------------------------------------
;; Kolmogorov-Smirov statistic

;; KS : (Vectorof Real) (U Dist (Vectorof Real)) -> Real
;; Correct for continuous cdf, may miss supremum for discrete dist.
(define (KS v1 v2)
  (cond [(vector? v2)
         (let ([v1 (vector-sort v1 <)]
               [v2 (vector-sort v2 <)])
           (max (KS* v1 (sorted->empirical-cdf v2))
                (KS* v2 (sorted->empirical-cdf v1))))]
        [else
         (let ([v1 (vector-sort v1 <)])
           (KS* v1 (lambda (x) (dist-cdf v2 x))))]))

(define (KS* v cdf)
  (define n (vector-length v))
  (for/fold ([m 0])
            ([x (in-vector v)]
             [i (in-naturals 1)])
    (define cdfx (cdf x))
    (max m
         (abs (- (/ i n) cdfx))
         (abs (- (/ (sub1 i) n) cdfx)))))
