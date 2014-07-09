;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     racket/syntax)
         racket/contract
         racket/match
         racket/math
         racket/pretty
         racket/dict
         racket/generic
         racket/flonum
         racket/vector
         data/order
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "private/dist.rkt")
(provide dist?
         integer-dist?
         real-dist?
         finite-dist?
         (contract-out
          [dist-pdf
           (->* [dist? any/c] [any/c] real?)]
          [dist-cdf
           (->* [dist? any/c] [any/c any/c] real?)]
          [dist-inv-cdf
           (->* [dist? (real-in 0 1)] [any/c any/c] any)]
          [dist-sample
           (-> dist? any)]
          [dist-enum
           (-> dist? any)]
          [dist-support
           (-> dist? any)]
          [dist-mean
           (-> dist? any)]
          [dist-median
           (-> dist? any)]
          [dist-mode
           (-> dist? any)]
          [dist-variance
           (-> dist? any)]
          [dist-update-prior
           (-> dist? any/c vector? (or/c dist? #f))])

         ;; Discrete dists
         discrete-dist
         discrete-dist?
         (contract-out
          [make-discrete-dist
           (-> dict? discrete-dist?)]
          [make-discrete-dist*
           (-> vector?
               (vectorof (>=/c 0))
               any)]
          [normalize-discrete-dist
           (-> discrete-dist? discrete-dist?)]))

;; FIXME:
;; - mode -> modes, return list?

;; Distributions from math/distributions have performance penalty in untyped code
;; (Also, no discrete-dist? predicate.)

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
       kind-kw:dist-kind
       (~or (~optional (~seq #:enum enum:expr))
            (~optional (~seq #:guard guard-fun:expr))
            (~optional (~seq #:support support:expr))
            (~optional (~seq #:mean mean:expr))
            (~optional (~seq #:median median:expr))
            (~optional (~seq #:mode mode:expr))
            (~optional (~seq #:variance variance:expr))
            (~optional (~seq #:conjugate conj-fun:expr))
            (~optional (~and no-provide #:no-provide)))
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
                   [d (datum->syntax stx 'this-dist)] ;; !!! unhygienic capture
                   [kind kind])
       (quasitemplate
        (begin
          (struct name-dist (p.param ...)
                  #:extra-constructor-name make-name-dist
                  #:guard (?? guard-fun (make-guard-fun (p.param ...) kind-kw))
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
                   (?? (define (*enum d) (let ([p.param (get-param d)] ...) enum)))
                   (?? (define (*support d) (let ([p.param (get-param d)] ...) support)))
                   (?? (define (*mean d) (let ([p.param (get-param d)] ...) mean)))
                   (?? (define (*median d) (let ([p.param (get-param d)] ...) median)))
                   (?? (define (*mode d) (let ([p.param (get-param d)] ...) mode)))
                   (?? (define (*variance d) (let ([p.param (get-param d)] ...) variance)))
                   (?? (define (*conj d data-d data)
                         (let ([p.param (get-param d)] ...)
                           (and conj-fun (conj-fun data-d data)))))]
                  extra-clause ...
                  #:transparent)
          #,(if (attribute no-provide)
                #'(begin)
                #'(provide (contract-out [struct name-dist ([p.param p.ctc] ...)]))))))]))

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

(define-dist-type bernoulli
  ([p (real-in 0 1)])
  #:any #:enum 2
  #:support '#s(integer-range 0 1)
  #:mean p
  #:median (cond [(> p 1/2) 1] [(= p 1/2) 1/2] [else 0])
  #:mode (cond [(> p 1/2) 1] [(= p 1/2) '(0 1)] [else 0])
  #:variance (* p (- 1 p)))

(define-dist-type binomial
  ([n exact-positive-integer?]
   [p (real-in 0 1)])
  #:nat #:enum (add1 n)
  #:support (integer-range 0 n)
  #:mean (* n p)
  #:mode (filter-modes this-dist
                       (let ([m (inexact->exact (floor (* (+ n 1) p)))])
                         (list m (sub1 m))))
  #:variance (* n p (- 1 p)))

(define-dist-type geometric
  ([p (real-in 0 1)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean (/ (- 1 p) p)
  #:mode 0
  #:variance (/ (- 1 p) (* p p)))

(define-dist-type poisson
  ([mean (>/c 0)])
  #:nat #:enum 'lazy
  #:support '#s(integer-range 0 +inf.0)
  #:mean mean
  #:mode (if (integer? mean)
             (list (inexact->exact mean) (sub1 (inexact->exact mean)))
             (inexact->exact (floor mean)))
  #:variance mean)

(define-dist-type beta
  ([a (>=/c 0)]
   [b (>=/c 0)])
  #:real
  #:support '#s(real-range 0 1) ;; [0,1]
  #:mean (/ a (+ a b))
  #:mode (and (> a 1) (> b 1) (/ (+ a -1) (+ a b -2)))
  #:variance (/ (* a b) (* (+ a b) (+ a b) (+ a b 1)))
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(bernoulli-dist _)
                   (beta-dist (+ a (for/sum ([x data] #:when (= x 1)) 1))
                              (+ b (for/sum ([x data] #:when (= x 0)) 0)))]
                  [`(binomial-dist ,n _)
                   (beta-dist (+ a (vector-sum data))
                              (+ b (for/sum ([x (in-vector data)]) (- n x))))]
                  [`(geometric-dist _)
                   (beta-dist (+ a (vector-length data))
                              (+ b (vector-sum data)))]
                  [_ #f])))

(define-dist-type cauchy
  ([mode real?]
   [scale (>/c 0)])
  #:real
  #:support '#s(real-range -inf.0 +inf.0)
  #:mean +nan.0
  #:mode mode
  #:variance +nan.0)

(define-dist-type exponential
  ([mean (>/c 0)])
  #:real
  #:support '#s(real-range 0 +inf.0) ;; [0, inf)
  #:mean mean
  #:mode 0
  #:variance (expt mean -2))

(define-dist-type gamma
  ([shape (>/c 0)]
   [scale (>/c 0)])
  #:real
  #:support '#s(real-range 0 +inf.0) ;; (0,inf)
  #:mean (* shape scale)
  #:mode (if (> shape 1) (* (- shape 1) scale) +nan.0)
  #:variance (* shape scale scale)
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(poisson-dist _)
                   (gamma-dist (+ shape (vector-sum data))
                               (/ scale (add1 (* (vector-length data) scale))))]
                  [`(exponential-dist _)
                   (gamma-dist (+ shape (vector-length data))
                               (/ (+ (/ scale) (vector-sum data))))]
                  [`(gamma-dist ,data-shape _)
                   (gamma-dist (+ shape (* data-shape (vector-length data)))
                               (/ (+ (/ scale) (vector-sum data))))]
                  [`(inverse-gamma-dist ,data-shape _)
                   (gamma-dist (+ shape (* (vector-length data) data-shape))
                               (/ (+ (/ scale) (for/sum ([x (in-vector data)]) (/ x)))))]
                  [`(normal-dist ,data-mean _)
                   (gamma-dist (+ shape (/ (vector-length data) 2))
                               (/ (+ (/ scale)
                                     (* 1/2 (for/sum ([x (in-vector data)])
                                              (sqr (- x data-mean)))))))]
                  [_ #f])))

(define-dist-type inverse-gamma
  ([shape (>/c 0)]
   [scale (>/c 0)])
  #:real
  #:support '#s(real-range 0 +inf.0) ;; (0,inf)
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(normal-dist ,data-mean _)
                   (inverse-gamma-dist
                    (+ shape (/ (vector-ref data) 2))
                    (/ (+ (/ scale)
                          (* 1/2 (for/sum ([x (in-vector data)])
                                   (sqr (- x data-mean)))))))]
                  [_ #f])))

(define-dist-type logistic
  ([mean real?]
   [scale (>/c 0)])
  #:real
  #:support '#s(real-range -inf.0 +inf.0)
  #:mean mean #:median mean #:mode mean
  #:variance (* scale scale pi pi 1/3))

(define-dist-type normal
  ([mean real?]
   [stddev (>/c 0)])
  #:real
  #:support '#s(real-range (-inf.0 +inf.0))
  #:mean mean
  #:median mean
  #:mode mean
  #:variance (* stddev stddev)
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(normal-dist _ ,data-stddev)
                   (normal-dist (/ (+ (/ mean (sqr stddev))
                                      (/ (vector-sum data)
                                         (sqr data-stddev)))
                                   (+ (/ (sqr stddev))
                                      (/ (vector-length data)
                                         (sqr data-stddev))))
                                (/ (+ (/ (sqr stddev))
                                      (/ (vector-length data)
                                         (sqr data-stddev)))))]
                  [_ #f])))

(define-dist-type uniform
  ([min real?]
   [max real?])
  #:real
  #:support (real-range min max)
  #:mean (/ (+ min max) 2)
  #:median (/ (+ min max) 2)
  #:variance (* (- max min) (- max min) 1/12)
  #:guard (lambda (a b _name)
            (unless (< a b)
              (error 'uniform-dist
                     "lower bound is not less than upper bound\n  lower: ~e\n  upper: ~e"
                     a b))
            (values a b)))

(define-dist-type categorical
  ([weights (vectorof (>=/c 0))])
  #:any #:enum (length weights)
  #:support (integer-range 0 (sub1 (vector-length weights)))
  #:mean (for/sum ([i (in-naturals)] [w (in-vector weights)]) (* i w))
  #:mode (let-values ([(best best-w)
                       (for/fold ([best null] [best-w -inf.0])
                           ([i (in-naturals)] [w (in-vector weights)])
                         (cond [(> w best-w)
                                (values (list i) w)]
                               [(= w best-w)
                                (values (cons i best) best-w)]
                               [else (values best best-w)]))])
           (reverse best))
  #:guard (lambda (weights _name) (validate/normalize-weights 'categorical-dist weights)))

;; FIXME: cache computations of B(alpha) normalizing factor for pdf
;; FIXME: flag for symmetric alphas, can sample w/ fewer gamma samplings
(define-dist-type dirichlet
  ([alpha (vectorof (>/c 0))])
  #:any #:enum #f
  ;; #:support ;; [0,1]^n
  ;; (product (make-vector (vector-length concentrations) '#s(real-range 0 1)))
  #:mean (let ([alphasum (vector-sum alpha)])
           (for/vector ([ai (in-vector alpha)]) (/ ai alphasum)))
  #:mode (if (for/and ([ai (in-vector alpha)]) (> ai 1))
             (let ([denom (for/sum ([ai (in-vector alpha)]) (sub1 ai))])
               (for/vector ([ai (in-vector alpha)]) (/ (sub1 ai) denom)))
             +nan.0)
  #:variance (let* ([a0 (vector-sum alpha)]
                    [denom (* a0 a0 (add1 a0))])
               (for/vector ([ai (in-vector alpha)])
                 (/ (* ai (- a0 ai)) denom)))
  #:guard (lambda (alpha _name)
            (vector->immutable-vector (vector-map exact->inexact alpha))))


;; ============================================================
;; Discrete distribution

;; Categorical dist has support 1..N; discrete has arbitrary values as support.
;; Prints nicely (sort), but non-standard constructor, can't use as match pattern.

;; Not automatically normalized.
;; Uses equal? to distinguish elements of support.
;; Does not detect duplicates! (FIXME?)

(define-dist-type *discrete
  ([vs vector?]
   [ws vector?]
   [wsum real?])
  #:any #:enum (hash-keys hash)
  #:no-provide
  #:property prop:custom-write
  (lambda (obj port mode)
    (print-discrete-dist (*discrete-dist-vs obj) (*discrete-dist-ws obj) port mode)))

(define (discrete-dist? x) (*discrete-dist? x))

(define-syntax (discrete-dist stx)
  (define-syntax-class vwpair
    #:description "pair of value and weight expressions"
    (pattern [value:expr weight]
             #:declare weight (expr/c #'(>=/c 0))))
  (syntax-parse stx
    [(discrete-dist p:vwpair ...)
     #'(make-discrete-dist* (vector p.value ...) (vector p.weight ...))]))

(define (make-discrete-dist dict)
  (define len (dict-count dict))
  (define vs (make-vector len #f))
  (define ws (make-vector len #f))
  (for ([(v w) (in-dict dict)]
        [i (in-naturals)])
    (vector-set! vs i v)
    (vector-set! ws i w))
  (for ([w (in-vector ws)])
    (unless (and (rational? w) (>= w 0))
      (raise-argument-error 'dict->discrete-dist "(dict/c any/c (>=/c 0))" dict)))
  (make-discrete-dist* vs ws))

(define (make-discrete-dist* vs ws)
  (unless (= (vector-length vs) (vector-length ws))
    (error 'make-discrete-dist
           "values and weights vectors have different lengths\n  values: ~e\n  weights: ~e"
           vs ws))
  (define vs* (vector->immutable-vector vs))
  (define ws* (vector->immutable-vector ws))
  (*discrete-dist vs* ws* (for/sum ([w (in-vector ws*)]) w)))

(define (normalize-discrete-dist d)
  (define vs (*discrete-dist-vs d))
  (define ws (*discrete-dist-ws d))
  (define wsum (*discrete-dist-wsum d))
  (*discrete-dist vs (vector-map (lambda (w) (/ w wsum)) ws) 1))

(define (print-discrete-dist vs ws port mode)
  (define (recur x p)
    (case mode
      ((#t) (write x p))
      ((#f) (display x p))
      ((0 1) (print x p mode))))

  ;; Only two cases: 0 vs everything else
  (define (print-prefix p)
    (case mode
      [(0) (write-string "(discrete-dist" p)]
      [else (write-string "#<discrete-dist:" p)]))
  (define (print-suffix p)
    (case mode
      [(0) (write-string ")" p)]
      [else (write-string ">" p)]))

  (define (print-contents p leading-space)
    (let ([lead (if leading-space (make-string (add1 leading-space) #\space) " ")])
      (for ([v+w (in-list (sort (for/list ([v (in-vector vs)] [w (in-vector ws)]) (cons v w))
                                (order-<? datum-order)
                                #:key car))])
        (when leading-space
          (pretty-print-newline p (pretty-print-columns)))
        (write-string lead p)
        (write-string "[" p)
        (recur (car v+w) p)
        (write-string " " p)
        (recur (cdr v+w) p)
        (write-string "]" p))))

  (define (print/one-line p)
    (print-prefix p)
    (print-contents p #f)
    (print-suffix p))

  (define (print/multi-line p)
    (let-values ([(line col pos) (port-next-location p)])
      (print-prefix p)
      (print-contents p col)
      (print-suffix p)))

  (cond [(and (pretty-printing)
              (integer? (pretty-print-columns)))
         ((let/ec esc
            (letrec ([tport
                      (make-tentative-pretty-print-output-port
                       port
                       (- (pretty-print-columns) 1)
                       (lambda () 
                         (esc
                          (lambda ()
                            (tentative-pretty-print-port-cancel tport)
                            (print/multi-line port)))))])
              (print/one-line tport)
              (tentative-pretty-print-port-transfer tport port))
            void))]
        [else
         (print/one-line port)])
  (void))


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
  (define p (for/sum ([i (in-range (add1 k))] [prob (in-vector probs)]) prob))
  (convert-p p log? 1-p?))
(define (rawcategorical-inv-cdf probs p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (let loop ([i 0] [p p])
    (cond [(>= i (vector-length probs))
           (error 'categorical-dist:inv-cdf "out of values")]
          [(< p (vector-ref probs i))
           i]
          [else
           (loop (add1 i) (- p (vector-ref probs i)))])))
(define (rawcategorical-sample probs)
  (rawcategorical-inv-cdf probs (random) #f #f))


;; ============================================================
;; Discrete dist support functions
;; -- Weights are not normalized

(define (raw*discrete-pdf vs ws wsum x log?)
  (or (for/or ([v (in-vector vs)]
               [w (in-vector ws)])
        (and (equal? x v) (/ w wsum)))
      0))
(define (raw*discrete-cdf vs ws wsum x log? 1-p?)
  (error 'discrete-dist:cdf "undefined"))
(define (raw*discrete-inv-cdf vs ws wsum x log? 1-p?)
  (error 'discrete-dist:inv-cdf "undefined"))
(define (raw*discrete-sample vs ws wsum)
  (define p (* (random) wsum))
  (let loop ([i 0] [p p])
    (unless (< i (vector-length ws))
      (error 'discrete-dist:sample "out of values"))
    (cond [(< p (vector-ref ws i))
           (vector-ref vs i)]
          [else
           (loop (add1 i) (- p (vector-ref ws i)))])))


;; ============================================================
;; Inverse gamma distribution

(define (m:flinverse-gamma-pdf shape scale x log?)
  (m:flgamma-pdf shape scale (/ x) log?))
(define (m:flinverse-gamma-cdf shape scale x log? 1-p?)
  (m:flgamma-cdf shape scale (/ x) log? 1-p?))
(define (m:flinverse-gamma-inv-cdf shape scale x log? 1-p?)
  (/ (m:flgamma-inv-cdf shape scale x log? 1-p?)))
(define (m:flinverse-gamma-sample shape scale n)
  (define flv (m:flgamma-sample shape scale n))
  (for ([i (in-range n)])
    (flvector-set! flv i (/ (flvector-ref flv i))))
  flv)


;; ============================================================
;; Dirichlet distribution

(define (rawdirichlet-pdf alpha x)
  (/ (for/product ([xi (in-vector x)] [ai (in-vector alpha)]) (expt xi (sub1 ai)))
     ;; FIXME: cache multinomial-beta, since fixed!
     (multinomial-beta alpha)))
(define (multinomial-beta alpha)
  (/ (for/product ([ai (in-vector alpha)]) (m:gamma ai))
     (m:gamma (for/sum ([ai (in-vector alpha)]) ai))))
(define (rawdirichlet-cdf alpha x log? 1-p?)
  (error 'dirichlet-dist:cdf "not implemented"))
(define (rawdirichlet-inv-cdf alpha x log? 1-p?)
  (error 'dirichlet-dist:inv-cdf "not implemented"))
(define (rawdirichlet-sample alpha)
  ;; TODO: batch gamma sampling when all alphas same?
  (define n (vector-length alpha))
  (define x (make-vector n))
  (for ([a (in-vector alpha)] [i (in-range n)])
    (vector-set! x i (flvector-ref (m:flgamma-sample a 1.0 1) 0)))
  (define gsum (for/sum ([g (in-vector x)]) g))
  (for ([i (in-range n)])
    (vector-set! x i (/ (vector-ref x i) gsum)))
  x)


;; ============================================================
;; Utils

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

(define (unconvert-p p log? 1-p?)
  (define p* (if log? (exp p) p))
  (if 1-p? (- 1 p*) p*))

(define (convert-p p log? 1-p?)
  (define p* (if 1-p? (- 1 p) p))
  (if log? (log p*) p*))

(define (filter-modes d ms)
  (define-values (best best-p)
    (for/fold ([best null] [best-p -inf.0])
        ([m (in-list ms)])
      (define m-p (dist-pdf d m))
      (cond [(> m-p best-p)
             (values (list m) m-p)]
            [(= m-p best-p)
             (values (cons m best) best-p)]
            [else
             (values best best-p)])))
  (reverse best))

(define (vector-sum v) (for/sum ([x (in-vector v)]) x))
