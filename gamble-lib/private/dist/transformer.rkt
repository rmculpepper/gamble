;; Copyright 2015-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require "base.rkt"
         "density.rkt"
         "define.rkt"
         "discrete.rkt")
(provide #| implicit from define-dist-type |#)

;; ============================================================

(define-dist-type affine-distx
  ;; a != 0
  ([d real-dist?] [a real?] [b real?])
  #:lebesgue
  #:pdf affine-distx-pdf
  #:cdf affine-distx-cdf
  #:inv-cdf affine-distx-inv-cdf
  #:sample affine-distx-sample
  ;; FIXME: support
  #:support #f)

(define (affine-forward a b x)
  (+ b (* a x)))

(define (affine-invert a b y)
  (/ (- y b) a))

(define (affine-distx-pdf d a b y log?)
  (define x (affine-invert a b y))
  (cond [(rational? x)
         (define xpdf (dist-pdf d x log?))
         (cond [log? (- xpdf (log a))]
               [else (/ xpdf a)])]
        [else (if log? -inf.0 0)]))

(define (affine-distx-cdf d a b y log? 1-p?)
  (dist-cdf d (affine-invert a b y) log? 1-p?))

(define (affine-distx-inv-cdf d a b r log? 1-p?)
  (affine-forward a b (dist-inv-cdf d r log? (if (< a 0) (not 1-p?) 1-p?))))

(define (affine-distx-sample d a b)
  (affine-forward a b (dist-sample d)))

;; ============================================================

(define-dist-type real-map-distx
  ;; f must be injective, continuous, differentiable, monotonic
  ;; invf returns NaN for out-of-range inputs
  ([d real-dist?] [f procedure?] [invf procedure?] [df procedure?])
  #:lebesgue
  #:pdf map*-distx-pdf
  #:cdf map*-distx-cdf
  #:inv-cdf map*-distx-inv-cdf
  #:sample map*-distx-sample
  ;; FIXME: support
  #:support #f)

(define (map*-distx-pdf d f invf df x log?)
  (define x0 (invf x))
  (cond [(rational? x0)
         (define pdf0 (dist-pdf d x0 log?))
         (define m (abs (df x0)))
         (cond [log? (- pdf0 (log m))]
               [else (if (eqv? m 0) +inf.0 (/ pdf0 m))])]
        [else
         (if log? -inf.0 0)]))

(define (map*-distx-cdf d f invf df x log? 1-p?)
  (dist-cdf d (invf x) log? 1-p?))

(define (map*-distx-inv-cdf d f invf df r log? 1-p?)
  (f (dist-inv-cdf d r log? 1-p?)))

(define (map*-distx-sample d f invf df)
  (f (dist-sample d)))

;; ============================================================

(define-dist-type clip-distx
  ([dist real-dist?] [a real?] [b real?])
  #:lebesgue ;; FIXME?
  #:pdf clip-distx-pdf
  #:cdf clip-distx-cdf
  #:inv-cdf clip-distx-inv-cdf
  #:sample clip-distx-sample
  #:guard (lambda (dist a b _type)
            (let ([a (exact->inexact a)]
                  [b (exact->inexact b)])
              (values dist (min a b) (max a b))))
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

(define-dist-type discretize-distx
  ([dist real-dist?])
  #:counting
  #:pdf discretize-distx-pdf
  #:cdf discretize-distx-cdf
  #:inv-cdf discretize-distx-inv-cdf
  #:sample discretize-distx-sample
  #:support (integer-range -inf.0 +inf.0)) ;; FIXME

(define (discretize-distx-pdf d x log?)
  ;; integer x "unrounds" to [x-0.5, x+0.5]
  (define p (- (dist-cdf d (+ x 0.5) #f #f)
               (dist-cdf d (- x 0.5) #f #f)))
  (convert-p p log? #f))

(define (discretize-distx-cdf d x log? 1-p?)
  (let ([x (round x)])
    (dist-cdf d (+ x 0.5) log? 1-p?)))

(define (discretize-distx-inv-cdf d p log? 1-p?)
  (inexact->exact (round (dist-inv-cdf d p log? 1-p?))))

(define (discretize-distx-sample d)
  (inexact->exact (round (dist-sample d))))

;; ============================================================

(define-dist-type mixture-dist
  ([mix (discrete-dist-of dist?)])
  #:mixture
  #:density mixture-density
  #:sample mixture-sample)

(define (make-mixture-dist ds [ws (make-vector (vector-length ds) 1)])
  (mixture-dist (make-discrete-dist ds ws)))

(define (mixture-density mix x full?)
  (density-sum (for/list ([subdist (in-vector (discrete-dist-vs mix))])
                 (dist-density subdist x full?))
               (discrete-dist-ws mix)))

(define (mixture-sample mix)
  (define d (dist-sample mix))
  (dist-sample d))
