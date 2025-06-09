;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         "real.rkt")
(provide (all-defined-out))

;; ------------------------------------------------------------
;; Density dimension (ddim)

;; Consider the following probabilistic "model":

;;   X ~ Bernoulli(1/2)
;;   Y ~ if X then Uniform(-1, 1) else Bernoulli(1/2)
;;   observe Y = 0

;; What is the posterior on X given the observation on Y?

;; Naive analysis:
;; If X = 1, then Y is drawn from Unif(-1,1), and the density at 0 is 1/2.
;; If X = 0, then Y is drawn from Bern(1/2), and the density at 0 is 1/2.
;; So the observation changes nothing; the posterior is the same as the prior,
;; so Bernoulli(1/2).

;; But that's absurd. Really, the entire model is absurd.
;; ------------------------------------------------------------

;; Density = (density NNReal Nat #f) | (density ExtReal Nat #t)

(struct density (d ddim log?) #:transparent
  #:guard (lambda (d ddim log? _name)
            (unless (or (rational? d) (eqv? d -inf.0))
              (raise-argument-error 'density "(or/c rational? -inf.0)" d))
            (unless (exact-nonnegative-integer? ddim)
              (raise-argument-error 'density "exact-nonnegative-integer?" ddim))
            (cond [log?
                   (values (fl d) ddim #t)]
                  [else
                   (unless (>= d 0)
                     (error 'density "expected nonnegative rational\n  given: ~e" d))
                   (values d ddim #f)])))

(define one-density (density 1 0 #f))

(define (density-zero? d)
  (match-define (density r _ log?) d)
  (if log? (zero? r) (= r -inf.0)))

(define (density->real d [log? #f])
  (match-define (density d1 _ log1?) d)
  (cond [(and log? log1?) d1]
        [log? (log d1)]
        [log1? (exp d1)]
        [else d1]))

(define (density* d1 d2)
  (match* [d1 d2]
    [[(density d1 ddim1 log1?) (density d2 ddim2 log2?)]
     (density (cond [(and log1? log2?) (+ d1 d2)]
                    [log1? (+ d1 (log (fl d2)))]
                    [log2? (+ (log (fl d1)) d2)]
                    [else (* d1 d2)])
              (+ ddim1 ddim2)
              (or log1? log2?))]))

(define (density+ d1 d2)
  (define (bad-ddim)
    (error 'density+
           "cannot add densities with different dimensions\n  given: ~e, ~e"
           d1 d2))
  (match* [d1 d2]
    [[(density d1 ddim1 log1?) (density d2 ddim2 log2?)]
     (unless (= ddim1 ddim2) (bad-ddim))
     (density (cond [(and log1? log2?) (logspace+ d1 d2)]
                    [log1? (logspace+ d1 (log (fl d2)))]
                    [log2? (logspace+ (log (fl d1)) d2)]
                    [else (+ d1 d2)])
              ddim1
              (or log1? log2?))]
    [[(? density? d1) #f] d1]))

(define (density<=? d1 d2)
  (match-define (density dr1 ddim1 log1?) d1)
  (match-define (density dr2 ddim2 log2?) d2)
  (cond [(= ddim1 ddim2)
         (cond [(and log1? log2?) (<= dr1 dr2)]
               [log1? (<= dr1 (log dr2))]
               [log2? (<= (log dr1) dr2)]
               [else (<= dr1 dr2)])]
        [else (> ddim1 ddim2)]))

#;
;; density-logratio : Density Density -> Real
(define (density-logratio d1 d2)
  (match-define (density _ ll1 ddim1) d1)
  (match-define (density _ ll2 ddim2) d2)
  (cond [(< ddim1 ddim2) +inf.0]
        [(> ddim1 ddim2) -inf.0]
        [else (- ll1 ll2)]))
