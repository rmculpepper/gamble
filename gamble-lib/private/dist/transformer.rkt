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
           (* w (dist-cdf cd x #f 1-p?)))))
   (define (-support self)
     (match-define (mixture-distx mix) self)
     (for/fold ([r #t]) ([cd (in-dist-values mix)])
       (if (eq? r #t) cd (support-union r (dist-support cd)))))])

;; ----------------------------------------
;; affine transformation

(define-dist-struct affine-distx
  ([d continuous-dist?]
   [a nonzero-rational? fl]
   [b rational? fl])
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
           [else (impossible log?)]))
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
   (define (-support self)
     (match-define (affine-distx d a b) self)
     (match (dist-support d)
       [(real-range lo hi)
        (let ([flo (affine-apply a b lo)] [fhi (affine-apply a b hi)])
          (real-range (min flo fhi) (max flo fhi)))]
       [_ #f]))])

(define (nonzero-rational? v)
  (and (rational? v) (not (zero? v))))

(define (affine-apply a b x)
  (+ b (* a x)))

(define (affine-invert a b y)
  (/ (- y b) a))

;; ----------------------------------------
;; clip (renormalize)

(define-dist-struct clip-distx
  ;; Represents dist clipped to (a,b) and renormalized.
  ([dist continuous-dist?]
   [a rational? fl]
   [b rational? fl])
  #:extension (pa lpa w lw)
  #:guard (lambda (dist a b)
            (unless (< a b)
              (error 'clip-distx "empty range\n  range: (~e, ~e)" a b))
            (unless (< (dist-cdf dist a #f #f) (dist-cdf dist b #f #f))
              (error 'clib-distx "range has no mass\n  dist: ~e\n  range: (~e, ~e)" dist a b))
            (values dist a b))
  #:methods gen:dist
  [(define (-sample self)
     (match-define (clip-distx d a b pa lpa w lw) (-clip-init self))
     (cond [(< w CLIP-REJECTION-THRESHOLD)
            (dist-inv-cdf d (+ pa (* w (random))))]
           [else
            (let loop ()
              (define x (dist-sample d))
              (if (<= a x b) x (loop)))]))
   (define (-pdf self x log?)
     (match-define (clip-distx d a b pa lpa w lw) (-clip-init self))
     (cond [log? (- (dist-pdf d x #t) lw)]
           [else (/ (dist-pdf d x #f) w)]))]
  #:methods gen:continuous-dist []
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (clip-distx d a b pa lpa w lw) (-clip-init self))
     (cond [(<= x a) (convert-p 0.0 log? 1-p?)]
           [(>= x b) (convert-p 1.0 log? 1-p?)]
           [log? (- (dist-cdf d x #t 1-p?) lw)]
           [else (/ (dist-cdf d x #f 1-p?) w)]))
   (define (-invcdf self p log? 1-p?)
     (match-define (clip-distx d a b pa lpa w lw) (-clip-init self))
     (cond [log?
            (define p* (logspace+ lpa (+ p lw)))
            (dist-inv-cdf d p* #t 1-p?)]
           [else
            (define p* (+ pa (* p w)))
            (dist-inv-cdf d p* #f 1-p?)]))
   (define (-support self)
     (match-define (clip-distx d a b _ _ _ _) self)
     (match (dist-support d)
       [(real-range lo hi)
        (real-range (max a lo) (min b hi))]
       [_ (real-range a b)]))])

(define CLIP-REJECTION-THRESHOLD 0.25)

(define (-clip-init self)
  (unless (clip-distx-lw self)
    (match-define (clip-distx d a b _ _ _ _) self)
    (define pa (dist-cdf d a #f #f))
    (define lpa (dist-cdf d a #t #f))
    (define w (- (dist-cdf d b #f #f) pa))
    (define lw (logspace- (dist-cdf d b #t #f) lpa))
    (set-clip-distx-pa! self pa)
    (set-clip-distx-lpa! self lpa)
    (set-clip-distx-w! self w)
    (set-clip-distx-lw! self lw))
  self)

;; ----------------------------------------
;; exp/log

;; Note: (exp-distx (normal-dist 0 1)) is a "lognormal" distribution, etc.
;; This library names transformations according to their effect on generation.

(define-dist-struct exp-distx
  ([d continuous-dist?])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (exp-distx d) self)
     (exp (dist-sample d)))
   (define (-pdf self y log?)
     (match-define (exp-distx d) self)
     (cond [(and (rational? y) (positive? y))
            (define x (log (fl y)))
            (cond [log? (- (dist-pdf d x #t) x)]
                  [else (/ (dist-pdf d x #f) (fl y))])]
           [else (impossible log?)]))]
  #:methods gen:continuous-dist []
  #:methods gen:real-dist
  [(define (-cdf self y log? 1-p?)
     (match-define (exp-distx d) self)
     (let ([x (if (and (real? y) (positive? y)) (log (fl y)) -inf.0)])
       (dist-cdf d x log? 1-p?)))
   (define (-invcdf self r log? 1-p?)
     (match-define (exp-distx d) self)
     (exp (dist-inv-cdf d r log? 1-p?)))
   (define (-support self)
     (match-define (exp-distx d) self)
     (match (dist-support d)
       [(real-range lo hi)
        (real-range (exp lo) (exp hi))]
       [_ #f]))])

;; ----------------------------------------
;; continuous transformation

(define-dist-struct real-map-distx
  ;; f must be injective, continuous, differentiable, monotonic increasing
  ;; invf returns -inf.0 or +inf.0 for out-of-range inputs
  ([d continuous-dist?] [f procedure?] [invf procedure?] [df procedure?])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (real-map-distx d f invf df) self)
     (f (dist-sample d)))
   (define (-pdf self y log?)
     (match-define (real-map-distx d f invf df) self)
     (define x (and (rational? x) (invf y)))
     (cond [(rational? x)
            (define m (abs (fl (df x))))
            (cond [log? (- (dist-pdf d x #t) (log m))]
                  [else (/ (dist-pdf d x #f) m)])]
           [else (impossible log?)]))]
  #:methods gen:continuous-dist []
  #:methods gen:real-dist
  [(define (-cdf self y log? 1-p?)
     (match-define (real-map-distx d f invf df) self)
     ;; If f is not monotonic increasing, need to flip 1-p?.
     (dist-cdf d (invf y) log? 1-p?))
   (define (-invcdf self r log? 1-p?)
     (match-define (real-map-distx d f invf df) self)
     ;; If f is not monotonic increasing, need to flip 1-p?.
     (f (dist-inv-cdf d r log? 1-p?)))
   (define (-support self)
     (match-define (real-map-distx d f invf df) self)
     (match (dist-support d)
       [(real-range lo hi)
        (let ([flo (f lo)] [fhi (f hi)])
          (real-range (min flo fhi) (max flo fhi)))]
       [_ #f]))])

(define (log:extended x)
  (if (real? x) (if (< x 0) -inf.0 (log (fl x))) +nan.0))

;; ============================================================
;; continuous-dist to integer-dist

;; ----------------------------------------
;; discretize

(define-dist-struct discretize/floor-distx
  ([dist continuous-dist?])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (discretize/floor-distx d) self)
     (exact (floor (dist-sample d))))
   (define (-pdf self x log?)
     (cond [(integer? x)
            (match-define (discretize/floor-distx d) self)
            ;; integer x "unfloors" to [x, x+1)
            (define hi (dist-cdf d (+ x 0.5) log? #f))
            (define lo (dist-cdf d (- x 0.5) log? #f))
            (if log? (logspace- hi lo) (- hi lo))]
           [else (impossible log?)]))]
  #:methods gen:integer-dist []
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (discretize/floor-distx d) self)
     (dist-cdf d (+ (floor x) 1.0) log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (discretize/floor-distx d) self)
     (exact (floor (dist-inv-cdf d p log? 1-p?))))
   (define (-support self)
     (match-define (discretize/floor-distx d) self)
     (match (dist-support d)
       [(real-range lo hi)
        (integer-range (xexact (floor lo)) (xexact (floor hi)))]
       [_ #f]))])

(define-dist-struct discretize/round-distx
  ([dist continuous-dist?])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (discretize/round-distx d) self)
     (exact (round (dist-sample d))))
   (define (-pdf self x log?)
     (cond [(integer? x)
            (match-define (discretize/round-distx d) self)
            ;; integer x "unrounds" to [x-0.5, x+0.5]
            (define hi (dist-cdf d (+ x 0.5) log? #f))
            (define lo (dist-cdf d (- x 0.5) log? #f))
            (if log? (logspace- hi lo) (- hi lo))]
           [else (impossible log?)]))]
  #:methods gen:integer-dist []
  #:methods gen:real-dist
  [(define (-cdf self x log? 1-p?)
     (match-define (discretize/round-distx d) self)
     (define ix (floor x)) ;; floor, not round
     (dist-cdf d (+ ix 0.5) log? 1-p?))
   (define (-invcdf self p log? 1-p?)
     (match-define (discretize/round-distx d) self)
     (exact (round (dist-inv-cdf d p log? 1-p?))))
   (define (-support self)
     (match-define (discretize/floor-distx d) self)
     (match (dist-support d)
       [(real-range lo hi)
        (integer-range (xexact (round lo)) (xexact (round hi)))]
       [_ #f]))])
