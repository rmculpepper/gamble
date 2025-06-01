;; Copyright 2015-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         "base.rkt"
         (submod "util.rkt" math)
         (submod "util.rkt" density)
         (submod "util.rkt" define)
         "measurable.rkt"
         "discrete.rkt")
(provide (all-defined-out))

;; ============================================================
;; continuous-dist to continuous-dist

;; ----------------------------------------
;; mixture

(define-dist-struct mixture-distx
  ([mix (discrete-dist-of continuous-dist?)])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (mixture-distx mix) self)
     (dist-sample (dist-sample mix)))
   (define (-pdf self x log?)
     (match-define (mixture-distx mix) self)
     (if log?
         (logspace-sum
          (for/list ([(cd w) (in-discrete-dist mix)])
            (+ (log w) (dist-pdf cd x #t))))
         (for/sum ([(cd w) (in-discrete-dist mix)])
           (* w (dist-pdf cd x #f)))))
   (define (-measure self ms)
     (match-define (mixture-distx mix) self)
     (for/sum ([(cd w) (in-discrete-dist mix)])
       (* w (dist-measure cd ms))))
   (define (-total-measure self)
     (match-define (mixture-distx mix) self)
     (for/sum ([(cd w) (in-discrete-dist mix)])
       (* w (dist-total-measure cd))))]
  #:methods gen:continuous-dist []
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (mixture-distx mix) self)
     (if log?
         (logspace-sum
          (for/list ([(cd w) (in-discrete-dist mix)])
            (+ (log w) (dist-cdf cd x #t 1-p?))))
         (for/sum ([(cd w) (in-discrete-dist mix)])
           (* w (dist-cdf cd x #f 1-p?)))))])

;; ----------------------------------------
;; affine transformation

(define-dist-struct affine-distx
  ([d continuous-dist?]
   [a nonzero-rational? inexact]
   [b rational? inexact])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (affine-distx d a b) self)
     (affine-apply a b (dist-sample d)))
   (define (-pdf self y log?)
     (match-define (affine-distx d a b) self)
     (define x (affine-invert a b y))
     (cond [(rational? x)
            (define xpdf (dist-pdf d x log?))
            (cond [log? (- xpdf (log (abs a)))]
                  [else (/ xpdf (abs a))])]
           [else (if log? -inf.0 0)]))
   (define (-measure self ms)
     (match-define (affine-distx d a b) self)
     (define (affine-f x) (affine-apply a b x))
     (match-define (measurable _ ivls) ms)
     (let ([ivls (map affine-f ivls)])
       (let ([ivls (if (< a 0) (reverse ivls) ivls)])
         (dist-measure d (measurable (hash) ivls)))))
   (define (-total-measure self)
     (match-define (affine-distx d a b) self)
     (dist-total-measure d))]
  #:methods gen:continuous-dist []
  #:methods gen:real-dist
  [(define (-cdf self y log? 1-p?)
     (match-define (affine-distx d a b) self)
     (dist-cdf d (affine-invert a b y) log? (if (< a 0) (not 1-p?) 1-p?)))
   (define (-invcdf self r log? 1-p?)
     (match-define (affine-distx d a b) self)
     (define x (dist-inv-cdf d r log? (if (< a 0) (not 1-p?) 1-p?)))
     (affine-apply a b x))

   ])

(define (nonzero-rational? v)
  (and (rational? v) (not (zero? v))))

(define (affine-apply a b x)
  (+ b (* a x)))

(define (affine-invert a b y)
  (/ (- y b) a))

;; ============================================================
#|
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
|#

;; ============================================================

#|
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
|#

;; ============================================================
;; continuous-dist to integer-dist

;; ----------------------------------------
;; discretize

(define-dist-struct discretize-distx
  ([dist continuous-dist?])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (discretize-distx d) self)
     (exact (round (dist-sample d))))
   (define (-pdf d x log?)
     ;; integer x "unrounds" to [x-0.5, x+0.5]
     (define hi (dist-cdf d (+ x 0.5) log? #f))
     (define lo (dist-cdf d (- x 0.5) log? #f))
     (if log? (logspace- hi lo) (- hi lo)))]
  #:methods gen:integer-dist []
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (discretize-distx d) self)
     (define ix (floor x)) ;; floor, not round
     (dist-cdf d (+ ix 0.5) log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (discretize-distx d) self)
     (exact (round (dist-inv-cdf d p log? 1-p?))))])

;; ============================================================
