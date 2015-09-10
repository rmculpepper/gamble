;; Copyright (c) 2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         (rename-in racket/match [match-define defmatch])
         racket/math
         racket/generic
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "../private/dist.rkt"
         "../private/interfaces.rkt"
         "../private/dist-define.rkt"
         "../private/dist-impl.rkt")
(provide #| implicit in define-dist-type |#)

;; ============================================================

(define-dist-type clip-distx
  ([dist real-dist?] [a real?] [b real?])
  #:pdf clip-distx-pdf
  #:cdf clip-distx-cdf
  #:inv-cdf clip-distx-inv-cdf
  #:sample clip-distx-sample
  #:support (real-range a b))

(define (clip-distx-pdf d a b x log?)
  (define w (- (dist-cdf d b) (dist-cdf d a)))
  (if log?
      (- (dist-pdf d x #t) (log w))
      (/ (dist-pdf d x #f) w)))

(define (clip-distx-cdf d a b x log? 1-p?)
  (define pa (dist-cdf d a))
  (define pb (dist-cdf d b))
  (define px (dist-cdf d x))
  (define p (/ (- px pa) (- pb pa)))
  (convert-p p log? 1-p?))

(define (clip-distx-inv-cdf d a b p log? 1-p?)
  (define pa (dist-cdf d a))
  (define pb (dist-cdf d b))
  (define p* (unconvert-p p log? 1-p?))
  (dist-inv-cdf d (+ (* (- pb pa) p*) pa) #f #f))

(define CLIP-REJECTION-THRESHOLD 0.25)

(define (clip-distx-sample d a b)
  (define pa (dist-cdf d a))
  (define pb (dist-cdf d b))
  (define w (- pb pa))
  (cond [(< w CLIP-REJECTION-THRESHOLD)
         (dist-inv-cdf d (+ pa (* w (random))))]
        [else
         (let loop ()
           (define x (dist-sample d))
           (if (<= a x b) x (loop)))]))

;; ============================================================

(define-dist-type exp-distx
  ([dist real-dist?])
  #:pdf exp-distx-pdf
  #:cdf exp-distx-cdf
  #:inv-cdf exp-distx-inv-cdf
  #:sample exp-distx-sample
  #:support '#s(real-range 0 +inf.0)) ;; FIXME: depends on d

;; x = exp[t]
;; dx/dt = exp[t] = x
(define (exp-distx-pdf d x log?)
  (cond [(and (rational? x) (> x 0))
         (define t (log x))
         (cond [log? (- (dist-pdf d t #t) t)]
               [else (/ (dist-pdf d t #f) x)])]
        [else (if log? -inf.0 0.0)]))

(define (exp-distx-cdf d x log? 1-p?)
  (cond [(and (rational? x) (> x 0))
         (define t (log x))
         (dist-cdf d t log? 1-p?)]
        [else +nan.0]))

(define (exp-distx-inv-cdf d p log? 1-p?)
  (exp (dist-inv-cdf d p log? 1-p?)))

(define (exp-distx-sample d)
  (expt (dist-sample d)))

;; ============================================================

(define-dist-type log-distx
  ([dist real-dist?])
  #:pdf log-distx-pdf
  #:cdf log-distx-cdf
  #:inv-cdf log-distx-inv-cdf
  #:sample log-distx-sample
  #:support '#s(real-range -inf.0 +inf.0)) ;; FIXME: depends on d

;; x = log[t]
;; dx/dt = 1/t
(define (log-distx-pdf d x log?)
  (cond [(real? x)
         (define t (exp x))
         (cond [log? (+ (dist-pdf d t #t) x)]
               [else (* (dist-pdf d t #f) t)])]
        [else +nan.0]))

(define (log-distx-cdf d x log? 1-p?)
  (cond [(real? x)
         (define t (exp x))
         (dist-cdf d t log? 1-p?)]
        [else +nan.0]))

(define (log-distx-inv-cdf d p log? 1-p?)
  (log (dist-inv-cdf d p log? 1-p?)))

(define (log-distx-sample d)
  (log (dist-sample d)))

;; ============================================================

(define-dist-type inverse-distx
  ([dist real-dist?])
  #:pdf inverse-distx-pdf
  #:cdf inverse-distx-cdf
  #:inv-cdf inverse-distx-inv-cdf
  #:sample inverse-distx-sample
  #:support '#s(real-range -inf.0 +inf.0)) ;; FIXME: depends on d

;; x = 1/t
;; dx/dt = -1/t^2
(define (inverse-distx-pdf d x log?)
  (define t (/ x))
  (cond [log? (+ (dist-pdf d t #t) (* 2 (log t)))]
        [else (* (dist-pdf d t #f) t t)]))

(define (inverse-distx-cdf d x log? 1-p?)
  (cond [(real? x)
         (define t (/ x))
         ;; Let x = f(t)
         (cond [(> x 0)
                ;; then f^-1(-inf.0, x) = (-inf.0,0) U (1/x, +inf.0)
                (define p (+ (dist-cdf d 0 #f #f) (dist-cdf d t #f #t)))
                (convert-p p log? 1-p?)]
               [(< x 0)
                ;; then f^-1(-inf.0, x) = (1/x, 0)
                (define p (- (dist-cdf d 0 #f #f) (dist-cdf d t #f #f)))
                (convert-p p log? 1-p?)]
               [(= x 0)
                ;; then f^-1(-inf.0, 0) = (-inf.0, 0)
                (dist-cdf d 0 log? 1-p?)])]
        [else +nan.0]))

(define (inverse-distx-inv-cdf d p log? 1-p?)
  (define p* (unconvert-p p log? 1-p?))
  (define p0 (dist-cdf d 0))
  (cond [(>= p* p0) ;; non-negative
         ;; p0 + 1 - p* = 1 - (p* - p0)
         (/ (dist-inv-cdf d (- p* p0) #f #t))]
        [else ;; negative
         ;; p0 - p*
         (let ([t (dist-inv-cdf d (- p0 p*) #f #f)])
           (if (zero? t) -inf.0 (/ t)))]))

(define (inverse-distx-sample d)
  (/ (dist-sample d)))

;; ============================================================

(define-dist-type affine-distx
  ([dist real-dist?] [a rational?] [b rational?])
  #:pdf affine-distx-pdf
  #:cdf affine-distx-cdf
  #:inv-cdf affine-distx-inv-cdf
  #:sample affine-distx-sample
  ;; FIXME: mean, variance???
  #:support '#s(real-range -inf.0 +inf.0)) ;; FIXME: depends on d

(define (affine-distx-pdf d a b x log?)
  (define t (/ (- x b) a))
  (cond [log? (- (dist-pdf d t #t) (log a))]
        [else (/ (dist-pdf d t #f) (abs a))]))

(define (affine-distx-cdf d a b x log? 1-p?)
  (define t (/ (- x b) a))
  (dist-cdf d t log? (if (negative? a) (not 1-p?) 1-p?)))

(define (affine-distx-inv-cdf d a b p log? 1-p?)
  (+ (* (dist-inv-cdf d p log? (if (negative? a) (not 1-p?) 1-p?)) a) b))

(define (affine-distx-sample d a b)
  (+ (* (dist-sample d) a) b))

;; ============================================================
