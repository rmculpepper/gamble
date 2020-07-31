;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/generic
         (submod racket/performance-hint begin-encourage-inline)
         "density.rkt"
         "../util/real.rkt")
(provide (all-defined-out))

;; NNReal = nonnegative real

;; LogReal = (inexact) Real, probability/score in logspace (may be +/-inf.0)

(begin-encourage-inline
  (define (convert-p p log? 1-p?)
    (define p* (if 1-p? (- 1 p) p))
    (if log? (log (exact->inexact p*)) p*))
  (define (unconvert-p p log? 1-p?)
    (define p* (if log? (exp p) p))
    (if 1-p? (- 1 p*) p*)))

;; ------------------------------------------------------------

(define-generics dist/sample
  ;; type X
  (*sample dist/sample) ;; Dist -> X
  (*wsample dist/sample) ;; Dist -> (values X NNReal/#f LogReal) -- FIXME, Density?
  #:fallbacks
  [(define (*wsample self) (values (dist-sample self) 1 0.0))])

(define (dist-sample d)
  (*sample d))

;; ------------------------------------------------------------

(define-generics dist
  ;; type X    -- element type

  ;; Meta
  (*type dist)                ; Dist -> Symbol
  (*params dist)              ; Dist -> (vector Param ...)

  ;; Density/mass
  ;; density/pdf accepts any value, gives 0/-inf.0 if not in support
  ;; cdf, inv-cdf may raise error if not in support
  (*density dist x full?)     ; Dist X Boolean -> Density
  (*pdf dist x log?)          ; Dist X Boolean -> Real -- beware, omits ddim!
  (*cdf dist x log? 1-p?)     ; Dist X Boolean Boolean -> Real (or error)
  (*inv-cdf dist x log? 1-p?) ; Dist Real Boolean Boolean -> X (or error)
  (*total-mass dist)          ; Dist -> NNReal or #f if unknown
  (*support dist)             ; Dist -> DistSupport
  (*Denergy dist x . d/dts)   ; Dist X Param ... -> Real
  (*enum dist)                ; Dist -> #f or ??
  (*conjugate dist ddist data); Dist KernelSexpr Any -> Dist/#f

  ;; Real-valued dists (X = Real)
  (*mean dist)                ; Dist -> Real/#f/NaN
  (*median dist)              ; Dist -> Real/#f/NaN
  (*modes dist)               ; Dist -> (Listof Real) or #f if unknown
  (*variance dist)            ; Dist -> Real/#f/NaN

  #:fallbacks
  [;; Density/mass
   (define (*pdf d x log?)
     (define dn (dist-density d x))
     (if log? (density-l dn) (density-n dn)))
   (define (*cdf d x log? 1-p?)
     (error 'dist-cdf "not defined for distribution\n  given: ~e" d))
   (define (*inv-cdf d x log? 1-p?)
     (error 'dist-inv-cdf "not defined for distribution\n  given: ~e" d))
   (define (*total-mass d) #f)
   (define (*support d) #f)
   (define (*Denergy d x . d/dts) #f)
   (define (*enum d) #f)
   (define (*conjugate d data-d data) #f)
   ;; Real dists
   (define (*mean d) #f)
   (define (*median d) #f)
   (define (*modes d) #f)
   (define (*variance d) #f)])

(define (dists-same-type? da db)
  (equal? (*type da) (*type db)))

(define (dist-density d x [full? #t])
  (*density d x full?))
(define (dist-density* d xs [full? #t])
  (density-product (for/list ([x xs]) (dist-density d x full?))))
(define (dist-pdf d x [log? #f])
  (*pdf d x log?))
(define (dist-cdf d x [log? #f] [1-p? #f])
  (*cdf d x log? 1-p?))
(define (dist-inv-cdf d x [log? #f] [1-p? #f])
  (*inv-cdf d x log? 1-p?))

(define (dist-total-mass d)
  (*total-mass d))

(define (dist-energy d x) ;; Energy = -log(pdf(d,x))
  (- (dist-pdf d x #t)))
(define (dist-Denergy d x . d/dts)
  ;; derivative of energy(d,x) wrt t, treating x and params(d) as functions of t
  ;; d/dts = dx/dt (default 1), dparam1/dt (default 0), ...
  (apply *Denergy d x d/dts))

(define (dist-support d)
  (*support d))

(define (dist-enum d)
  (*enum d))

;; DistSupport is one of
;; - #f        -- unknown/unrestricted
;; - 'finite   -- unknown but finite
;; - #s(integer-range Min Max)  -- inclusive
;; - #s(real-range Min Max)     -- inclusive (may overapprox)
;; - TODO: #s(product (Vectorof Support)), ...
(struct integer-range (min max) #:prefab)
(struct real-range (min max) #:prefab)

;; Returns #t if dist is necessarily {integer,real}-valued.
;; Note: a discrete-dist that happens to have integer values is NOT integer-dist?.
(define (integer-dist? d)
  (and (dist? d) (integer-range? (dist-support d))))
(define (real-dist? d)
  (and (dist? d) (real-range? (dist-support d))))
(define (finite-dist? d)
  (define support (and (dist? d) (dist-support d)))
  (or (eq? support 'finite)
      (and (integer-range? support)
           (> (integer-range-min support) -inf.0)
           (< (integer-range-max support) +inf.0))))

;; FIXME: also compute normalizing constant (as density)
(define (dist-conjugate d data-d data)
  (or (*conjugate d data-d data)
      (error 'dist-conjugate "combination not supported\n  dist: ~e\n  kernel: ~e\n  data: ~e"
             d data-d data)))

;; dist-{mean,median,variance} : Dist -> Real | #f | NaN
;; #f means unknown; NaN means known to be undefined
(define (dist-mean d)     (*mean d))
(define (dist-median d)   (*median d))
(define (dist-variance d) (*variance d))

;; dist-modes : Dist -> (Listof Real) | #f
(define (dist-modes d)    (*modes d))
