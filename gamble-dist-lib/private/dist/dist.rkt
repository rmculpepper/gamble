;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/generic
         (submod racket/performance-hint begin-encourage-inline))
(provide (all-defined-out))

;; LogReal = Real, probability/score in logspace.

(begin-encourage-inline
  (define (convert-p p log? 1-p?)
    (define p* (if 1-p? (- 1 p) p))
    (if log? (log (exact->inexact p*)) p*))
  (define (unconvert-p p log? 1-p?)
    (define p* (if log? (exp p) p))
    (if 1-p? (- 1 p*) p*)))

;; ------------------------------------------------------------

(define-generics sampler
  ;; type X
  (*sample sampler) ;; Dist -> X
  (*wsample sampler) ;; Dist -> (values X LogReal)
  #:fallbacks
  [(define (*wsample self) (values (dist-sample self) 0.0))])

(define (dist-sample d)
  (*sample d))

;; ------------------------------------------------------------

(define-generics dist
  ;; type X    -- element type

  ;; Meta
  (*type dist)                ; Dist -> Symbol
  (*params dist)              ; Dist -> (vector Param ...)

  ;; Density/mass
  ;; pdf accepts any value, gives 0/-inf.0 if not in support
  ;; cdf, inv-cdf may raise error if not in support
  (*pdf dist x log?)          ; Dist X Boolean -> Real
  (*cdf dist x log? 1-p?)     ; Dist X Boolean Boolean -> Real (or error)
  (*inv-cdf dist x log? 1-p?) ; Dist Real Boolean Boolean -> X (or error)
  (*ddim dist)                ; Dist -> Nat  -- 0 for discrete measure, 1 for Lebesgue
  (*total-mass dist)          ; Dist -> NNReal or #f if unknown
  (*support dist)             ; Dist -> DistSupport
  (*Denergy dist x . d/dts)   ; Dist X Param ... -> Real
  (*enum dist)                ; Dist -> #f or ??

  ;; Real-valued dists (X = Real)
  (*mean dist)                ; Dist -> Real/#f/NaN
  (*median dist)              ; Dist -> Real/#f/NaN
  (*modes dist)               ; Dist -> (Listof Real) or #f if unknown
  (*variance dist)            ; Dist -> Real/#f/NaN

  #:fallbacks
  [;; Density/mass
   (define (*cdf d x log? 1-p?)
     (error 'dist-cdf "not defined for distribution\n  given: ~e" d))
   (define (*inv-cdf d x log? 1-p?)
     (error 'dist-inv-cdf "not defined for distribution\n  given: ~e" d))
   (define (*total-mass d) #f)
   (define (*support d) #f)
   (define (*Denergy d x . d/dts)
     (error 'dist-Denergy "not implemented"))   
   (define (*enum d) #f)
   ;; Real dists
   (define (*mean d) #f)
   (define (*median d) #f)
   (define (*modes d) #f)
   (define (*variance d) #f)])

(define (dists-same-type? da db)
  (equal? (*type da) (*type db)))

(define (dist-pdf d x [log? #f])
  (*pdf d x log?))
(define (dist-cdf d x [log? #f] [1-p? #f])
  (*cdf d x log? 1-p?))
(define (dist-inv-cdf d x [log? #f] [1-p? #f])
  (*inv-cdf d x log? 1-p?))

(define (dist-energy d x) ;; Energy = -log(pdf(d,x))
  (- (dist-pdf d x #t)))
(define (dist-Denergy d x . d/dts)
  ;; derivative of energy(d,x) wrt t, treating x and params(d) as functions of t
  ;; d/dts = dx/dt (default 1), dparam1/dt (default 0), ...
  (apply *Denergy d x d/dts))

(define (dist-has-mass? d)
  (zero? (*ddim d)))

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

;; dist-{mean,median,variance} : Dist -> Real | #f | NaN
;; #f means unknown; NaN means known to be undefined
(define (dist-mean d)     (*mean d))
(define (dist-median d)   (*median d))
(define (dist-variance d) (*variance d))

;; dist-modes : Dist -> (Listof Real) | #f
(define (dist-modes d)    (*modes d))

;; ============================================================

#|
(require (for-syntax racket/base))

(define-syntax define-*-dist-type
  (syntax-rules (lambda)
    [(define-dist-type s ([f fpred? fconv] ...)
       [gen-id method-binding ...] ...)
     ;; =>
     (struct s (f ...)
       #:guard
       (lambda (f ... name)
         (define (bad fname) (maker-error 's '(f ...) '(fpred? ...) fname f ...))
         (unless (fpred? f) (bad 'f))
         (values (fconv f) ...))
       #:methods gen:meta
       [(define (*type self) 's)
        (define (*params self) (match self [(s f ...) (vector-immutable f ...)]))]
       (~@
        #:methods gen-id
        [(do-method (s f ...) method-binding)
         ...])
       ...)]))

(define-syntax define-fl-dist-type
  (syntax-rules ()
    [(define-fl-dist-type s ([f fpred?] ...) . more)
     (define-dist-type s ([f fpred? exact->inexact] ...) . more)]))

(define-syntax define-dist-type
  (syntax-rules ()
    [(define-fl-dist-type s ([f fpred?] ...) . more)
     (define-dist-type s ([f fpred? begin] ...) . more)]))

(define-syntax do-method
  (syntax-rules (=> =)
    [(_ (s f ...) [method-id body])
     (define (method-id self)
       (match self [(s f ...) body]))]
    [(_ (s f ...) [method-id (arg ...) => proc])
     (define (method-id self arg ...)
       (match self [(s f ...) (proc f ... arg ...)]))]
    [(_ (s f ...) [method-id = (lambda (arg ...) body ...)])
     (define (method-id self arg ...)
       (match self [(s f ...) body ...]))]))

(define (maker-error sname fnames fpreds bad-fname . fvalues)
  (for ([fname (in-list fnames)] [fpred (in-list fpreds)] [index (in-naturals)])
    (when (eq? fname bad-fname)
      (apply raise-argument-error sname (format "~s" fpred) index fvalues))))
|#
