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

(define vector->empirical-cdf
  (case-lambda
    [(vs)
     (sorted->empirical-cdf (vector-sort vs <))]
    [(vs ws)
     (define svs (for/vector ([v (in-vector vs)] [w (in-vector ws)]) (cons v w)))
     (vector-sort! svs < #:key car)
     (define scws (make-vector (vector-length svs)))
     (for/fold ([sum 0]) ([i (in-naturals)] [vw (in-vector svs)])
       (vector-set! svs i (car vw))
       (vector-set! scws i (+ sum (cdr vw)))
       (+ sum (cdr vw)))
     (sorted->empirical-cdf svs scws)]))

(define (sorted->empirical-cdf svs [scws #f])
  (define (ecdf x)
    (cond [(>= x (vector-ref svs 0))
           (define k (binary-search/least-geq svs x))
           (if scws (vector-ref scws k) (/ (add1 k) (vector-length svs)))]
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
