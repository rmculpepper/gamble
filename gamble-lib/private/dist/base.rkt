;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/generic
         racket/sequence
         "measurable.rkt"
         (submod "util.rkt" density))
(provide (all-defined-out))

(define-generics dist
  ;; type X
  (-sample dist)                   ;; Dist -> X
  (-pdf dist x log?)               ;; Dist X Boolean -> Real/ExtReal
  (-density dist x log?)           ;; Dist X Boolean -> Density
  (-measure dist ms)               ;; Dist Measurable -> NNReal
  (-total-measure dist)            ;; Dist -> NNReal
  #:fallbacks
  [(define (-density self x log?)
     (cond [(continuous-dist? self)
            (density (dist-pdf self x log?) 1 log?)]
           [(integer-dist? self)
            (density (dist-pdf self x log?) 0 log?)]
           [else (raise-support-error 'dist-density self)]))
   (define (-measure self ms)
     (match-define (measurable atoms ivls) ms)
     (cond [(continuous-dist? self)
            (let loop ([ivls ivls] [acc 0])
              (match ivls
                [(list* lo hi ivls)
                 (loop ivls (+ acc (- (-cdf self hi #f #f) (-cdf self lo #f #f))))]
                [(list) acc]))]
           [(integer-dist? self)
            (define ivls-mass
              (let loop ([ivls ivls] [acc 0])
                (match ivls
                  [(list* lo hi ivls)
                   (define mass
                     ;; Interval is (lo,hi) but cdf includes endpoint; adjust if necessary.
                     (let ([hi* (if (integer? hi) (- hi 0.5) hi)])
                       (- (-cdf self hi* #f #f) (-cdf self lo #f #f))))
                   (loop ivls (+ acc mass))]
                  [(list) acc])))
            (define atoms-mass
              (for/sum ([v (in-hash-keys atoms)])
                (dist-pdf self v #f)))
            (+ ivls-mass atoms-mass)]
           [else (raise-support-error 'dist-measure self)]))
   (define (-total-measure self)
     (cond [(continuous-dist? self) 1]
           [(integer-dist? self) 1]
           [else (raise-support-error 'dist-total-measure self)]))])

(define (dist-sample d)
  (unless (dist? d) (raise-argument-error 'dist-sample "dist?" d))
  (-sample d))
(define (dist-pdf d x [log? #f])
  (unless (dist? d) (raise-argument-error 'dist-pdf "dist?" d))
  (-pdf d x (and log? #t)))
(define (dist-density d x [log? #f])
  (unless (dist? d) (raise-argument-error 'dist-density "dist?" d))
  (-density d x (and log? #t)))
(define (dist-measure d ms)
  (unless (dist? d) (raise-argument-error 'dist-measure "dist?" d))
  (unless (measurable? ms) (raise-argument-error 'dist-measure "measurable?" ms))
  (-measure d ms))
(define (dist-total-measure d)
  (unless (dist? d) (raise-argument-error 'dist-total-measure "dist?" d))
  (-total-measure d))

(define-generics enumerable-dist   ;; extends dist
  ;; Represents discrete, enumerable distributions.
  (-sequence enumerable-dist)      ;; Dist -> Sequence[X]
  (-wsequence enumerable-dist)     ;; Dist -> Sequence[(values X NNReal)]
  #:fallbacks
  [(define (-wsequence self)
     (sequence-map (lambda (v) (values v (dist-pdf self v #f)))
                   (in-dist-values self)))])

(define (in-dist d)
  (unless (enumerable-dist? d) (raise-argument-error 'in-dist "enumerable-dist?" d))
  (-wsequence d))
(define (in-dist-values d)
  (unless (enumerable-dist? d) (raise-argument-error 'in-dist-values "enumerable-dist?" d))
  (-sequence d))

(define-generics real-dist ;; extends dist; comprises {continuous,integer}-dist
  ;; Represents normalized real-valued distributions.
  ;; If ddim = 1, continuous wrt Lebesgue measure.
  ;; type X = Real
  (-cdf real-dist x log? 1-p?)     ;; Dist X Boolean Boolean -> NNReal
  (-invcdf real-dist x log? 1-p?)  ;; Dist Real Bool Bool -> X
  (-support real-dist)             ;; Dist -> (cons ExtReal ExtReal) or #f
  (-mean real-dist)                ;; Dist -> Real or #f
  (-median real-dist)              ;; Dist -> Real or #f
  (-modes real-dist)               ;; Dist -> (Listof Real) or #f
  (-variance real-dist)            ;; Dist -> Real or #f
  #:fallbacks
  [(define (-support d) '(-inf.0 . +inf.0))
   (define (-mean d) #f)
   (define (-median d) #f)
   (define (-modes d) #f)
   (define (-variance d) #f)])

(define (dist-cdf d x [log? #f] [1-p? #f])
  (unless (real-dist? d) (raise-argument-error 'dist-cdf "real-dist?" d))
  (unless (real? x) (raise-argument-error 'dist-cdf "real?" x))
  (-cdf d x (and log? #t) (and 1-p? #t)))

;; dist-inv-cdf : Real-Dist Real Boolean Boolean -> Real
;; (dist-inv-cdf d p) returns least x such that Pr[X <= x] >= p, where X ~ d.
(define (dist-inv-cdf d p [log? #f] [1-p? #f])
  (unless (real-dist? d) (raise-argument-error 'dist-inv-cdf "real-dist?" d))
  (unless (real? p) (raise-argument-error 'dist-inv-cdf "real?" p))
  (-invcdf d p (and log?) (and 1-p?)))

(define-generics continuous-dist   ;; extends real-dist
  ;; Represents normalized, continuous real-valued distributions.
  ;; (-denergy real-dist x . d/dts)   ;; Dist Real Param ... -> Real
  #:fallbacks [])

(define-generics integer-dist      ;; extends real-dist, enumerable-dist
  ;; Represents discrete, normalized integer-valued distributions.
  #:fallbacks [])

;; ============================================================

;; ;; DistSupport is one of
;; ;; - #f        -- unknown/unrestricted
;; ;; - #s(integer-range Min Max)  -- inclusive
;; ;; - #s(real-range Min Max)     -- inclusive (may overapprox)
(struct integer-range (lo hi) #:prefab)
(struct real-range (lo hi) #:prefab)
