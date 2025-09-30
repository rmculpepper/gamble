;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/generic
         racket/sequence
         "measurable.rkt"
         "../util/dnum.rkt"
         (submod "util.rkt" math)
         (submod "util.rkt" define))
(provide (all-defined-out))

(define (dist-type d)
  (and (meta-dist? d) (-type d)))

(define-generics dist
  ;; type X
  (-sample dist)                   ;; Dist -> X
  (-pdf dist x log?)               ;; Dist X Boolean -> Real/ExtReal
  (-measure dist ms)               ;; Dist Measurable -> NNReal
  (-total-measure dist)            ;; Dist -> NNReal
  (-count dist)                    ;; Dist -> (U Nat +inf.0), upper bound
  #:fallbacks
  [(define (-measure self ms)
     (match-define (measurable atoms ivls) ms)
     (cond [(real-dist? self)
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
     (cond [(real-dist? self) 1]
           [(integer-dist? self) 1]
           [else (raise-support-error 'dist-total-measure self)]))
   (define (-count self) +inf.0)
   (define (-type self) #f)])

(define (dist-sample d)
  (unless (dist? d) (raise-argument-error 'dist-sample "dist?" d))
  (-sample d))
(define (dist-pdf d x [log? #f])
  (unless (dist? d) (raise-argument-error 'dist-pdf "dist?" d))
  (cond [(numeric-dist? d)
         (if (rational? x) (-pdf d x (and log? #t)) (if log? -inf.0 0))]
        [else (-pdf d x (and log? #t))]))
(define (dist-density d x [log? #f])
  (unless (dist? d) (raise-argument-error 'dist-density "dist?" d))
  (real->dnum (dist-pdf d x log?) log?))
(define (dist-measure d ms)
  (unless (dist? d) (raise-argument-error 'dist-measure "dist?" d))
  (unless (measurable? ms) (raise-argument-error 'dist-measure "measurable?" ms))
  (-measure d ms))
(define (dist-total-measure d)
  (unless (dist? d) (raise-argument-error 'dist-total-measure "dist?" d))
  (-total-measure d))
(define (dist-count d)
  (unless (dist? d) (raise-argument-error 'dist-count "dist?" d))
  (-count d))

(define-generics enumerable-dist   ;; extends dist
  ;; Represents discrete, enumerable distributions.
  (-sequence enumerable-dist)      ;; Dist -> Sequence[X]
  (-wsequence enumerable-dist)     ;; Dist -> Sequence[(values X NNReal)]
  #:fallbacks
  [(define (-count self) +inf.0)
   (define (-wsequence self)
     (sequence-map (lambda (v) (values v (dist-pdf self v #f)))
                   (in-dist-values self)))])

(define (finite-dist? d)
  (and (enumerable-dist? d) (< (-count d) +inf.0)))

(define (in-dist d)
  (unless (enumerable-dist? d) (raise-argument-error 'in-dist "enumerable-dist?" d))
  (-wsequence d))
(define (in-dist-values d)
  (unless (enumerable-dist? d) (raise-argument-error 'in-dist-values "enumerable-dist?" d))
  (-sequence d))

(define-generics numeric-dist ;; extends dist; comprises {real,integer}-dist
  ;; Represents normalized real-valued distributions.
  ;; type X = Real
  (-cdf numeric-dist x log? 1-p?)     ;; Dist X Boolean Boolean -> NNReal
  (-invcdf numeric-dist x log? 1-p?)  ;; Dist Real Bool Bool -> X
  (-support numeric-dist)             ;; Dist -> DistSupport
  (-mean numeric-dist)                ;; Dist -> Real or #f
  (-median numeric-dist)              ;; Dist -> Real or #f
  (-modes numeric-dist)               ;; Dist -> (Listof Real) or #f
  (-variance numeric-dist)            ;; Dist -> Real or #f
  #:fallbacks
  [(define (-support d) #f)
   (define (-mean d) #f)
   (define (-median d) #f)
   (define (-modes d) #f)
   (define (-variance d) #f)])

(define (dist-cdf d x [log? #f] [1-p? #f])
  (unless (numeric-dist? d) (raise-argument-error 'dist-cdf "numeric-dist?" d))
  (unless (real? x) (raise-argument-error 'dist-cdf "real?" x))
  (-cdf d x (and log? #t) (and 1-p? #t)))

;; dist-inv-cdf : Numeric-Dist Real Boolean Boolean -> Real
;; (dist-inv-cdf d p) returns least x such that Pr[X <= x] >= p, where X ~ d.
(define (dist-inv-cdf d p [log? #f] [1-p? #f])
  (unless (numeric-dist? d) (raise-argument-error 'dist-inv-cdf "numeric-dist?" d))
  (unless (real? p) (raise-argument-error 'dist-inv-cdf "real?" p))
  (-invcdf d p (and log?) (and 1-p?)))

;; dist-support : Numeric-Dist -> DistSupport
(define (dist-support d)
  (unless (numeric-dist? d) (raise-argument-error 'dist-support "numeric-dist?" d))
  (-support d))

(define-generics real-dist   ;; extends numeric-dist
  ;; Represents normalized, continuous real-valued distributions.
  ;; (-denergy numeric-dist x . d/dts)   ;; Dist Real Param ... -> Real
  #:fallbacks [])

(define-generics integer-dist      ;; extends numeric-dist, enumerable-dist
  ;; Represents discrete, normalized integer-valued distributions.
  #:fallbacks [])

(define-generics driftable                     ;; extends dist
  (-drift1 driftable x params? scale)          ;; Dist X Bool Real -> (cons X Real)/#f
  (-drift2 driftable old-dist x params? scale) ;; Dist Dist X Bool Real -> (cons X Real)/#f
  (-drift-dist driftable x params? scale)      ;; Dist X Bool Real -> Dist/#f
  #:fallbacks
  [(define (-drift1 self old-value params? scale-factor)
     (dist-drift2 self self old-value params? scale-factor))
   (define (-drift2 new-dist old-dist old-value params? scale-factor)
     (cond [(dist-drift-dist new-dist old-value params? scale-factor)
            => (lambda (fd)
                 (define new-value (dist-sample fd))
                 (cond [(dist-drift-dist old-dist new-value params? scale-factor)
                        => (lambda (rd)
                             (define lF (dist-pdf fd new-value #t))
                             (define lR (dist-pdf rd old-value #t))
                             (cons new-value (- lR lF)))]
                       [else #f]))]
           [else #f]))
   (define (-drift-dist self x params? scale) #f)])

(define (dist-drift1 dist x [params? #t] [scale 1.0])
  (unless (dist? dist)
    (raise-argument-error 'dist-drift1 "dist?" dist))
  (unless (positive-rational? scale)
    (raise-argument-error 'dist-drift1 "positive-rational?" scale))
  (cond [(not (driftable? dist)) #f]
        [(and (integer-dist? dist) (not (integer? x))) #f]
        [(and (real-dist? dist) (not (rational? x))) #f]
        [else (-drift1 dist x params? scale)]))

(define (dist-drift2 dist old-dist x [params? #t] [scale 1.0])
  (unless (dist? dist)
    (raise-argument-error 'dist-drift2 "dist?" dist))
  (unless (dist? old-dist)
    (raise-argument-error 'dist-drift2 "dist?" old-dist))
  (unless (positive-rational? scale)
    (raise-argument-error 'dist-drift2 "positive-rational?" scale))
  (cond [(not (and (driftable? dist) (driftable? old-dist))) #f]
        [(and (integer-dist? dist) (not (integer? x))) #f]
        [(and (real-dist? dist) (not (rational? x))) #f]
        [else (-drift2 dist old-dist x params? scale)]))

(define (dist-drift-dist dist x [params? #t] [scale 1.0])
  (unless (dist? dist)
    (raise-argument-error 'dist-drift-dist "dist?" dist))
  (unless (positive-rational? scale)
    (raise-argument-error 'dist-drift-dist "positive-rational?" scale))
  (cond [(not (driftable? dist)) #f]
        [(and (integer-dist? dist) (not (integer? x))) #f]
        [(and (real-dist? dist) (not (rational? x))) #f]
        [else (-drift-dist dist x params? scale)]))

(define-generics conjugate-dist           ;; extends dist
  ;; Represents conjugate priors. The `-conjugate` operation assumes
  ;; that the data are have non-zero likelihood. The `data` argument
  ;; is a vector for (currently) no good reason.
  (-conjugate conjugate-dist spec data))  ;; Dist DistSpec Vector -> Dist/#f

;; ============================================================

;; ;; DistSupport is one of
;; ;; - #f        -- unknown/unrestricted
;; ;; - #s(integer-range Min Max)  -- inclusive
;; ;; - #s(real-range Min Max)     -- inclusive (may overapprox)
(struct integer-range (lo hi) #:prefab)
(struct real-range (lo hi) #:prefab)

(define (support-union s1 s2)
  (match s1
    [(real-range lo1 hi1)
     (match s2
       [(real-range lo2 hi2)
        (real-range (min lo1 lo2) (max hi1 hi2))]
       [_ #f])]
    [(integer-range lo1 hi1)
     (match s2
       [(integer-range lo2 hi2)
        (integer-range (min lo1 lo2) (max hi1 hi2))]
       [_ #f])]
    [_ #f]))
