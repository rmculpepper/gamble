;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         racket/contract
         racket/math
         racket/generic
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions))
(provide dist?
         dist-pdf
         dist-cdf
         dist-inv-cdf
         dist-sample
         dist-enum)

;; FIXME: discrete dists
;; - categorical has support 1..N
;; - discrete has arbitrary support
;;   - print nicely (sort)
;;   - enumerate etc should return discrete dist instead of list
;;   - cdf ? -- could, using datum-order, but expensive to check, precludes eg HO dist
;; - ??? normalized vs unnormalized?
;; - mode -> modes, return list?

;; TODO:
;; - support

;; Distributions from math/distributions have performance penalty in untyped code
;; (Also, no discrete-dist? predicate.)

;; A Dist is (pdist pdf cdf inv-cdf sample enum)
;; - pdf : Real Boolean -> Flonum
;; - cdf : Real Boolean Boolean -> Flonum
;; - inv-cdf : Probability -> Flonum
;; - sample : Nat -> FlVector
;; - enum : PosInt    -- {0 ... n-1}
;;        | 'lazy     -- {0 ...}
;;        | list      -- the list elements
;;        | #f        -- not enumerable
(struct pdist () #:transparent)

(define-generics dist
  (*pdf dist x log?)
  (*cdf dist x log? 1-p?)
  (*inv-cdf dist x log? 1-p?)
  (*sample dist)
  (*type dist)
  (*params dist)
  (*enum dist)
  (*support dist)
  (*mean dist)
  (*median dist)
  (*mode dist)
  (*variance dist))

(define (dist-pdf d x [log? #f])
  (*pdf d x log?))
(define (dist-cdf d x [log? #f] [1-p? #f])
  (*cdf d x log? 1-p?))
(define (dist-inv-cdf d x [log? #f] [1-p? #f])
  (*inv-cdf d x log? 1-p?))
(define (dist-sample d)
  (*sample d))
(define (dist-enum d)
  (*enum d))

(define (dist-support d)
  (*support d))
(define (dist-mean d)
  (*mean d))
(define (dist-median d)
  (*median d))
(define (dist-mode d)
  (*mode d))
(define (dist-variance d)
  (*variance d))

;; Support is one of
;; - list  -- those elements
;; - #s(integer-range Min Max)
;; - #s(real-range Min Max)
;; - #f    -- unknown/unrestricted
;; - #s(product #(Support ...))
(struct integer-range (min max) #:prefab)
(struct real-range (min max) #:prefab)
(struct product (ranges) #:prefab)

;; ----

(define-syntax (define-dist-type stx)

  (define-syntax-class param-spec
    (pattern [param:id ctc:expr]))

  ;; A Dist-Kind is one of
  ;; - #:nat   -- exact domain/support, inexact pdf/cdf
  ;; - #:real  -- inexact domain/support, inexact pdf/cdf
  ;; - #:any   -- unrestricted domain/support, exact or inexact pdf/cdf
  (define-syntax-class dist-kind
    (pattern #:nat)
    (pattern #:real)
    (pattern #:any))

  (syntax-parse stx
    [(define-dist-type name (p:param-spec ...)
       (~and kind-kw (~or #:nat #:real #:any))
       (~or (~optional (~seq #:enum enum:expr) #:defaults ([enum #'#f]))
            (~optional (~seq #:guard guard-fun:expr) #:defaults ([guard-fun #'#f]))
            (~optional (~seq #:support support:expr) #:defaults ([support #'#f]))
            (~optional (~seq #:mean mean:expr) #:defaults ([mean #'#f]))
            (~optional (~seq #:median median:expr) #:defaults ([median #'#f]))
            (~optional (~seq #:mode mode:expr) #:defaults ([mode #'#f]))
            (~optional (~seq #:variance variance:expr) #:defaults ([variance #'#f])))
       ...
       extra-clause ...)
     (define kind
       (case (syntax->datum #'kind-kw)
         [(#:nat) 'nat] [(#:real) 'real] [(#:any) 'any]))
     (define prefix (case kind [(nat real) "m:fl"] [(any) "raw"]))
     (with-syntax ([name-dist (format-id #'name "~a-dist" #'name)]
                   [make-name-dist (format-id #'name "make-~a-dist" #'name)]
                   [(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name "~a-dist-~a" #'name param))]
                   [fl-pdf (format-id #'name "~a~a-pdf" prefix #'name)]
                   [fl-cdf (format-id #'name "~a~a-cdf" prefix #'name)]
                   [fl-inv-cdf (format-id #'name "~a~a-inv-cdf" prefix #'name)]
                   [fl-sample (format-id #'name "~a~a-sample" prefix #'name)]
                   [kind kind])
       #'(begin
           (struct name-dist pdist (p.param ...)
                   #:extra-constructor-name make-name-dist
                   #:guard
                   (or guard-fun
                       (make-guard-fun (p.param ...) kind-kw))
                   #:methods gen:dist
                   [(define (*pdf d x log?)
                      (fl-pdf (get-param d) ... (exact->inexact x) log?))
                    (define (*cdf d x log? 1-p?)
                      (fl-cdf (get-param d) ... (exact->inexact x) log? 1-p?))
                    (define (*inv-cdf d x log? 1-p?)
                      (fl-inv-cdf (get-param d) ... (exact->inexact x) log? 1-p?))
                    (define (*sample d)
                      (case 'kind
                        [(nat) (inexact->exact (flvector-ref (fl-sample (get-param d) ... 1) 0))]
                        [(real) (flvector-ref (fl-sample (get-param d) ... 1) 0)]
                        [(any) (fl-sample (get-param d) ...)]))
                    (define (*type d) 'name)
                    (define (*params d) (vector (get-param d) ...))
                    (define (*enum d) (let ([p.param (get-param d)] ...) enum))
                    (define (*support d) (let ([p.param (get-param d)] ...) support))
                    (define (*mean d) (let ([p.param (get-param d)] ...) mean))
                    (define (*median d) (let ([p.param (get-param d)] ...) median))
                    (define (*mode d) (let ([p.param (get-param d)] ...) mode))
                    (define (*variance d) (let ([p.param (get-param d)] ...) variance))]
                   extra-clause ...
                   #:transparent)
           (provide/contract [struct name-dist ([p.param p.ctc] ...)])))]))

(define-syntax (make-guard-fun stx)
  (syntax-parse stx
    [(make-guard-fun (param ...) #:any)
     #'#f]
    [(make-guard-fun (param ...) #:nat)
     #'(lambda (param ... _name) (values (exact->inexact param) ...))]
    [(make-guard-fun (param ...) #:real)
     #'(lambda (param ... _name) (values (exact->inexact param) ...))]))

;; ----

;;(define-dist-type bernoulli   ([prob (real-in 0 1)])        #:nat #:enum 2)
(define-dist-type bernoulli   ([p (real-in 0 1)])
  #:any #:enum 2
  #:support '(0 1)
  #:mean p
  #:median (cond [(> p 1/2) 1] [(= p 1/2) 1/2] [else 0])
  #:mode (cond [(> p 1/2) 1] [(= p 1/2) '(0 1)] [else 0])
  #:variance (* p (- 1 p)))
(define-dist-type binomial    ([n exact-positive-integer?] [p (real-in 0 1)])
  #:nat #:enum (add1 n)
  #:support (integer-range 0 n)
  #:mean (* n p)
  #:variance (* n p (- 1 p)))
(define-dist-type geometric   ([p (real-in 0 1)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean (/ (- 1 p) p)
  #:mode 0
  #:variance (/ (- 1 p) (* p p)))
(define-dist-type poisson     ([mean (>/c 0)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean mean
  #:variance mean)
(define-dist-type beta        ([a (>=/c 0)] [b (>=/c 0)])
  #:real
  #:support '#(real-range 0 1) ;; [0,1]
  #:mean (/ a (+ a b))
  #:mode (and (> a 1) (> b 1) (/ (+ a -1) (+ a b -2)))
  #:variance (/ (* a b) (* (+ a b) (+ a b) (+ a b 1))))
(define-dist-type cauchy      ([mode real?] [scale (>/c 0)])
  #:real
  #:support '#s(real-range -inf.0 +inf.0)
  #:mode mode)
(define-dist-type exponential ([mean (>/c 0)])
  #:real
  #:support '#s(real-range 0 +inf.0) ;; [0, inf)
  #:mean mean
  #:mode 0
  #:variance (expt mean -2))
(define-dist-type gamma       ([shape (>/c 0)] [scale (>/c 0)])
  #:real
  #:support '#s(real-range 0 +inf.0) ;; (0,inf)
  #:mean (* shape scale)
  #:mode (and (> shape 1) (* (- shape 1) scale))
  #:variance (* shape scale scale))
(define-dist-type logistic    ([mean real?] [scale (>/c 0)])
  #:real
  #:support '#s(real-range -inf.0 +inf.0)
  #:mean mean
  #:median mean #:mode mean
  #:variance (* scale scale pi pi 1/3))
(define-dist-type normal      ([mean real?] [stddev (>/c 0)])
  #:real
  #:support '#s(real-range (-inf.0 +inf.0))
  #:mean mean
  #:median mean
  #:mode mean
  #:variance (* stddev stddev))
(define-dist-type uniform     ([min real?] [max real?])
  #:real
  #:support (real-range min max)
  #:mean (/ (+ min max) 2))

(define-dist-type categorical ([weights (vectorof (>/c 0))])
  #:any #:enum (length weights)
  #:guard (lambda (weights _name) (validate/normalize-weights 'categorical-dist weights)))

#|
(define-dist-type discrete (vals weights) #:any #:enum vals
  #:guard (lambda (vals weights _name)
            (unless (and (vector? vals) (vector? weights)
                         (= (vector-length vals) (vector-length weights)))
              (raise-arguments-error 'discrete-dist
                "values and weights have unequal lengths\n  values: ~e\n  weights: ~e"
                vals weights))
            (define weights* (validate-weights 'discrete-dist weights))
            (values (vector->immutable-vector vals) weights*)))
|#

(define (validate/normalize-weights who weights)
  (unless (and (vector? weights)
               (for/and ([w (in-vector weights)])
                 (and (rational? w) (>= w 0))))
    (raise-argument-error 'categorical-dist "(vectorof (>=/c 0))" weights))
  (define weight-sum (for/sum ([w (in-vector weights)]) w))
  (unless (> weight-sum 0)
    (error 'categorical-dist "weights sum to zero\n  weights: ~e" weights))
  (if (= weight-sum 1)
      (vector->immutable-vector weights)
      (vector-map (lambda (w) (/ w weight-sum)) weights)))

#|
(define (make-discrete-dist probs)
  (let ([n (length probs)]
        [prob-sum (apply + probs)])
    (make-dist discrete #:raw-params (probs prob-sum) #:enum n)))
|#


;; ============================================================
;; Bernoulli dist functions
;; -- math/dist version converts exact->inexact

(define (rawbernoulli-pdf prob v log?)
  (define p
    (cond [(= v 0) (- 1 prob)]
          [(= v 1) prob]
          [else 0]))
  (convert-p p log? #f))
(define (rawbernoulli-cdf prob v log? 1-p?)
  (define p
    (cond [(< v 0) 0]
          [(< v 1) (- 1 prob)]
          [else 1]))
  (convert-p p log? 1-p?))
(define (rawbernoulli-inv-cdf prob p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (cond [(<= p (- 1 prob)) 0]
        [else 1]))
(define (rawbernoulli-sample prob)
  (if (<= (random) prob) 0 1))

;; ============================================================
;; Categorical weighted dist functions
;; -- Assume weights are nonnegative, normalized.

(define (rawcategorical-pdf probs k log?)
  (unless (< k (vector-length probs))
    (error 'categorical-pdf "index out of bounds\n  index: ~e\n  bounds: [0,~s]"
           k (sub1 (vector-length probs))))
  (define l (vector-ref probs k))
  (if log? (log l) l))
(define (rawcategorical-cdf probs k log? 1-p?)
  (define p (for/sum ([i (in-range (add1 k))] [prob (in-list probs)]) prob))
  (convert-p p log? 1-p?))
(define (rawcategorical-inv-cdf probs p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (let loop ([probs probs] [p p] [i 0])
    (cond [(null? probs)
           (error 'rawcategorical-inv-cdf "out of values")]
          [(< p (car probs))
           i]
          [else
           (loop (cdr probs) (- p (car probs)) (add1 i))])))
(define (rawcategorical-sample probs)
  (rawcategorical-inv-cdf probs (random) #f #f))

(define (unconvert-p p log? 1-p?)
  (define p* (if log? (exp p) p))
  (if 1-p? (- 1 p*) p*))

(define (convert-p p log? 1-p?)
  (define p* (if 1-p? (- 1 p) p))
  (if log? (log p*) p*))
