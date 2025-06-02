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

(define (dist-sample d) (-sample d))
(define (dist-pdf d x [log? #f]) (-pdf d x log?))
(define (dist-density d x [log? #f]) (-density d x log?))
(define (dist-measure d ms) (-measure d ms))
(define (dist-total-measure d) (-total-measure d))

(define-generics enumerable-dist   ;; extends dist
  ;; Represents discrete, enumerable distributions.
  (-sequence enumerable-dist)      ;; Dist -> Sequence[X]
  (-wsequence enumerable-dist)     ;; Dist -> Sequence[(values X NNReal)]
  #:fallbacks
  [(define (-wsequence self)
     (sequence-map (lambda (v) (values v (dist-pdf self v #f)))
                   (in-dist-values self)))])

(define (in-dist dist) (-wsequence dist))
(define (in-dist-values dist) (-sequence dist))

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
  #:defaults
  ([(lambda (v) (continuous-dist? v))
    (define (-ddim self) 1)]
   [(lambda (v) (integer-dist? v))
    (define (-ddim self) 0)])
  #:fallbacks
  [(define (-support d) '(-inf.0 . +inf.0))
   (define (-mean d) #f)
   (define (-median d) #f)
   (define (-modes d) #f)
   (define (-variance d) #f)])

(define (dist-cdf d x [log? #f] [1-p? #f])
  (-cdf d x log? 1-p?))
(define (dist-inv-cdf d r [log? #f] [1-p? #f])
  (-invcdf d r log? 1-p?))

(define-generics continuous-dist   ;; extends real-dist
  ;; Represents normalized, continuous real-valued distributions.
  ;; (-denergy real-dist x . d/dts)   ;; Dist Real Param ... -> Real
  #:fallbacks [])

(define-generics integer-dist      ;; extends real-dist, enumerable-dist
  ;; Represents discrete, normalized integer-valued distributions.
  #:fallbacks [])

;; FIXME: real^2-dist
;; FIXME: real^n-dist

;; ============================================================

;; (define-generics dist/sample
;;   ;; type X
;;   (*sample dist/sample) ;; Dist -> X
;;   (*wsample dist/sample) ;; Dist -> (values X NNReal/#f LogReal) -- FIXME, Density?
;;   #:fallbacks
;;   [(define (*wsample self) (values (dist-sample self) 1 0.0))])

;; (define (dist-sample d)
;;   (*sample d))

;; (define-generics dist
;;   ;; type X    -- element type

;;   ;; Meta
;;   (*type dist)                ; Dist -> Symbol
;;   (*params dist)              ; Dist -> (vector Param ...)

;;   ;; Density/mass
;;   ;; density/pdf accepts any value, gives 0/-inf.0 if not in support
;;   ;; cdf, inv-cdf may raise error if not in support
;;   (*density dist x full?)     ; Dist X Boolean -> Density
;;   (*pdf dist x log?)          ; Dist X Boolean -> Real -- beware, omits ddim!
;;   (*cdf dist x log? 1-p?)     ; Dist X Boolean Boolean -> Real (or error)
;;   (*inv-cdf dist x log? 1-p?) ; Dist Real Boolean Boolean -> X (or error)
;;   (*total-mass dist)          ; Dist -> NNReal or #f if unknown
;;   (*support dist)             ; Dist -> DistSupport
;;   (*Denergy dist x . d/dts)   ; Dist X Param ... -> Real
;;   (*enum dist)                ; Dist -> #f or ??
;;   (*conjugate dist ddist data); Dist KernelSexpr Any -> Dist/#f

;;   ;; Real-valued dists (X = Real)
;;   (*mean dist)                ; Dist -> Real/#f/NaN
;;   (*median dist)              ; Dist -> Real/#f/NaN
;;   (*modes dist)               ; Dist -> (Listof Real) or #f if unknown
;;   (*variance dist)            ; Dist -> Real/#f/NaN

;;   #:fallbacks
;;   [;; Density/mass
;;    (define (*pdf d x log?)
;;      (define dn (dist-density d x))
;;      (if log? (density-l dn) (density-n dn)))
;;    (define (*cdf d x log? 1-p?)
;;      (error 'dist-cdf "not defined for distribution\n  given: ~e" d))
;;    (define (*inv-cdf d x log? 1-p?)
;;      (error 'dist-inv-cdf "not defined for distribution\n  given: ~e" d))
;;    (define (*total-mass d) #f)
;;    (define (*support d) #f)
;;    (define (*Denergy d x . d/dts) #f)
;;    (define (*enum d) #f)
;;    (define (*conjugate d data-d data) #f)
;;    ;; Real dists
;;    (define (*mean d) #f)
;;    (define (*median d) #f)
;;    (define (*modes d) #f)
;;    (define (*variance d) #f)])

;; (define (dists-same-type? da db)
;;   (equal? (*type da) (*type db)))

;; (define (dist-density d x [full? #t])
;;   (*density d x full?))
;; (define (dist-density* d xs [full? #t])
;;   (density-product (for/list ([x xs]) (dist-density d x full?))))
;; (define (dist-pdf d x [log? #f])
;;   (*pdf d x log?))
;; (define (dist-cdf d x [log? #f] [1-p? #f])
;;   (*cdf d x log? 1-p?))
;; (define (dist-inv-cdf d x [log? #f] [1-p? #f])
;;   (*inv-cdf d x log? 1-p?))

;; (define (dist-total-mass d)
;;   (*total-mass d))

;; (define (dist-energy d x) ;; Energy = -log(pdf(d,x))
;;   (- (dist-pdf d x #t)))
;; (define (dist-Denergy d x . d/dts)
;;   ;; derivative of energy(d,x) wrt t, treating x and params(d) as functions of t
;;   ;; d/dts = dx/dt (default 1), dparam1/dt (default 0), ...
;;   (apply *Denergy d x d/dts))

;; (define (dist-support d)
;;   (*support d))

;; (define (dist-enum d)
;;   (*enum d))

;; ;; DistSupport is one of
;; ;; - #f        -- unknown/unrestricted
;; ;; - 'finite   -- unknown but finite
;; ;; - #s(integer-range Min Max)  -- inclusive
;; ;; - #s(real-range Min Max)     -- inclusive (may overapprox)
;; ;; - TODO: #s(product (Vectorof Support)), ...
;; (struct integer-range (min max) #:prefab)
;; (struct real-range (min max) #:prefab)

;; ;; Returns #t if dist is necessarily {integer,real}-valued.
;; ;; Note: a discrete-dist that happens to have integer values is NOT integer-dist?.
;; (define (integer-dist? d)
;;   (and (dist? d) (integer-range? (dist-support d))))
;; (define (real-dist? d)
;;   (and (dist? d) (real-range? (dist-support d))))
;; (define (finite-dist? d)
;;   (define support (and (dist? d) (dist-support d)))
;;   (or (eq? support 'finite)
;;       (and (integer-range? support)
;;            (> (integer-range-min support) -inf.0)
;;            (< (integer-range-max support) +inf.0))))

;; ;; FIXME: also compute normalizing constant (as density)
;; (define (dist-conjugate d data-d data)
;;   (or (*conjugate d data-d data)
;;       (error 'dist-conjugate "combination not supported\n  dist: ~e\n  kernel: ~e\n  data: ~e"
;;              d data-d data)))

;; ;; dist-{mean,median,variance} : Dist -> Real | #f | NaN
;; ;; #f means unknown; NaN means known to be undefined
;; (define (dist-mean d)     (*mean d))
;; (define (dist-median d)   (*median d))
;; (define (dist-variance d) (*variance d))

;; ;; dist-modes : Dist -> (Listof Real) | #f
;; (define (dist-modes d)    (*modes d))

#;
(define (finite-dist->hash who d)
  (cond [(discrete-dist? d)
         (discrete-dist-h d)]
        [(dist-enum d)
         => (lambda (enum)
              (cond [(integer? enum)
                     (for/fold ([h (hash)]) ([i (in-range enum)])
                       (hash-set h i (dist-pdf d i)))]
                    [(vector? enum)
                     (for/fold ([dh (hash)]) ([v (in-vector enum)])
                       (dhash-add dh v (dist-pdf d v)))]
                    [else
                     (error who "internal error: non-enumerable finite dist\n  dist: ~e" d)]))]
        [else (raise-argument-error who "finite-dist?" d)]))
