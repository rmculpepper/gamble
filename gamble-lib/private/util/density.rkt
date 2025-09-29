;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         "real.rkt")
(provide (all-defined-out))

;; Density = (density #f NNReal) | (density #t ExtReal)

(struct density (log? x) #:transparent
  #:guard (lambda (log? x _name)
            (cond [log?
                   (unless (or (rational? x) (eqv? x -inf.0))
                     (raise-argument-error 'density "(or/c rational? -inf.0)" x))
                   (values #t (fl x))]
                  [else
                   (unless (and (rational? x) (>= x 0))
                     (raise-argument-error 'density "(>=/c 0)" x))
                   (values #f x)])))

(define one-density (density #f 1))

(define (density-zero? dn)
  (match-define (density log? x) dn)
  (if log? (= x -inf.0) (zero? x)))

(define (density->real dn [log? #f])
  (match-define (density log1? x) dn)
  (cond [(and log? log1?) x]
        [log? (log x)]
        [log1? (exp x)]
        [else x]))

;; density* : Density Density -> Density
(define (density* d1 d2)
  (match* [d1 d2]
    [[(density log1? x1) (density log2? x2)]
     (density (or log1? log2?)
              (cond [(and log1? log2?) (+ x1 x2)]
                    [log1? (+ x1 (log (fl x2)))]
                    [log2? (+ (log (fl x1)) x2)]
                    [else (* x1 x2)]))]))

;; density/ : Density Density -> Density
(define (density/ d1 d2)
  (match* [d1 d2]
    [[(density log1? x1) (density log2? x2)]
     (density (or log1? log2?)
              (cond [(and log1? log2?) (- x1 x2)]
                    [log1? (- x1 (log (fl x2)))]
                    [log2? (- (log (fl x1)) x2)]
                    [else (/ x1 x2)]))]))

;; density+ : Density Density/#f -> Density
(define (density+ dn1 dn2)
  (match* [dn1 dn2]
    [[(density log1? x1) (density log2? x2)]
     (density (or log1? log2?)
              (cond [(and log1? log2?) (logspace+ x1 x2)]
                    [log1? (logspace+ x1 (log (fl x2)))]
                    [log2? (logspace+ (log (fl x1)) x2)]
                    [else (+ x1 x2)]))]
    [[dn1 #f] dn1]))

;; density- : Density Density/#f -> Density
(define (density- dn1 dn2)
  (match* [dn1 dn2]
    [[(density log1? x1) (density log2? x2)]
     (density (or log1? log2?)
              (cond [(and log1? log2?) (logspace- x1 x2)]
                    [log1? (logspace- x1 (log (fl x2)))]
                    [log2? (logspace- (log (fl x1)) x2)]
                    [else (- x1 x2)]))]
    [[dn1 #f] dn1]))

(define (density-sum dns)
  (foldr density+ #f dns))

(define (density<=? dn1 dn2)
  (match-define (density log1? x1) dn1)
  (match-define (density log2? x2) dn2)
  (cond [(and log1? log2?) (<= x1 x2)]
        [log1? (<= x1 (log x2))]
        [log2? (<= (log x1) x2)]
        [else (<= x1 x2)]))
