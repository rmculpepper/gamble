;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/transformer)
         racket/contract
         racket/sequence
         racket/match
         racket/pretty
         racket/struct
         racket/vector
         scramble/struct
         "base.rkt"
         (submod "util.rkt" math)
         (submod "util.rkt" density)
         (submod "util.rkt" weights)
         "measurable.rkt")
(provide (all-defined-out))

;; The empty dist is represented as a discrete dist with no elements.

;; ============================================================
;; Discrete distribution

;; Categorical dist has support 1..N; discrete has arbitrary values as support.
;; Prints nicely (sort), but non-standard constructor, can't use as match pattern.

;; Not necessarily normalized.
;; Uses equal? to distinguish elements of support.

;; DiscreteDist[X]:
(struct discrete-dist (h wsum)
  #:transparent
  #:property prop:custom-write
  (lambda (self port mode)
    (define h (discrete-dist-h self))
    (print-discrete-dist 'discrete-dist h port mode))
  #:methods gen:dist
  [(define (-sample self)
     (-discrete-sample self))
   (define (-density self x log?)
     (density (-discrete-pdf self x log?) 0 log?))
   (define (-pdf self x log?)
     (-discrete-pdf self x log?))
   (define (-measure self ms)
     (-discrete-measure self ms))
   (define (-total-measure self)
     (discrete-dist-wsum self))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-hash-keys (discrete-dist-h self)))
   (define (-wsequence self)
     (in-hash (discrete-dist-h self)))])

;; ----------------------------------------
;; Constructor

;; -discrete-intern-table : EphHashalw[Hash => DiscreteDist]
;; Needs ephemeron, since dist contains hash. Use hashalw to avoid
;; issues if hash has mutable keys.
(define -discrete-intern-table (make-ephemeron-hashalw))

(define (hash->discrete-dist h)
  (define who 'hash->discrete-dist)
  (define (bad) (raise-argument-error who "(hash/c any/c (>=/c 0))" h))
  (let loop ([h h])
    (define (copy f)
      (for/fold ([dh (hash)]) ([(v w) (in-hash h)] #:when (> w 0))
        (hash-set dh v (f w))))
    (cond [(hash-ref -discrete-intern-table h #f)
           => values]
          [(and (hash? h) (immutable? h) (not (impersonator? h))
                (hash-equal? h) (hash-strong? h))
           (define-values (wsum any-zero? any-exact?)
             (for/fold ([s 0] [any-zero? #f] [any-exact? #f])
                       ([w (in-hash-values h)])
               (unless (and (rational? w) (>= w 0)) (bad))
               (values (+ s w)
                       (or any-zero? (zero? w))
                       (or any-exact? (exact? w)))))
           (cond [(and any-exact? (inexact? wsum))
                  (loop (copy inexact))]
                 [any-zero?
                  (loop (copy values))]
                 [else
                  (define dist (discrete-dist h wsum))
                  (hash-set! -discrete-intern-table h dist)
                  dist])]
          [(hash? h)
           (loop (copy values))]
          [else (bad)])))

(define empty-discrete-dist (hash->discrete-dist '#hash()))

;; ----------------------------------------
;; DDExt

;; DDExt is (ddext (Vectorof X) (Vectorof PosReal) (Vectorof PosReal))
;; - cws[k] is sum of ws[0..k] (inclusive)
(struct ddext (vs ws cws) #:transparent)

;; -discrete-ext-table : WeakHasheq[DiscreteDist => DDExt]
(define -discrete-ext-table (make-weak-hasheq))

(define (-discrete-ext dist)
  (define (calc-ext)
    (define h (discrete-dist-h dist))
    (define len (hash-count h))
    (define vs (make-vector len))
    (define ws (make-vector len))
    (define cws (make-vector len))
    (for/fold ([s 0]) ([(v w) (in-hash h)] [i (in-naturals)])
      (vector-set! vs i v)
      (vector-set! ws i w)
      (vector-set! cws i (+ s w))
      (+ s w))
    (ddext (vector->immutable-vector vs)
           (vector->immutable-vector ws)
           (vector->immutable-vector cws)))
  (hash-ref! -discrete-ext-table dist (lambda () (calc-ext))))

(void (-discrete-ext empty-discrete-dist))

;; ----------------------------------------
;; More constructors

(define (dirac v [w 1])
  (discrete-dist (hash v w) w))

(define make-discrete-dist
  (case-lambda
    [(vs)
     (unless (vector? vs) (raise-argument-error 'make-discrete-dist "vector?" vs))
     (define w (/ (max 1 (vector-length vs))))
     (for/discrete-dist ([v (in-vector vs)])
       (values v w))]
    [(vs ws)
     (define (badws)
       (raise-argument-error 'make-discrete-dist "(vectorof (>=/c 0))" ws))
     (unless (vector? vs) (raise-argument-error 'make-discrete-dist "vector?" vs))
     (unless (vector? ws) (badws))
     (unless (= (vector-length vs) (vector-length ws))
       (error 'make-discrete-dist
              (string-append
               "values vector and weights vectors have different lengths"
               "\n  values: ~e\n  weights: ~e")
              vs ws))
     (for/discrete-dist ([v (in-vector vs)] [w (in-vector ws)])
       (unless (and (rational? w) (>= w 0)) (badws))
       (values v w))]))

;; ----------------------------------------
;; Operations

(define (discrete-dist-values dist)
  (ddext-vs (-discrete-ext dist)))
(define (discrete-dist-weights dist)
  (ddext-ws (-discrete-ext dist)))
(define (discrete-dist->hash dist)
  (discrete-dist-h dist))

(define LINEAR-SAMPLE-LIMIT 10)

(define (-discrete-sample dist)
  (match-define (discrete-dist h wsum) dist)
  (define n (hash-count h))
  (when (zero? n) (error 'dist-sample:discrete-dist "empty distribution"))
  (cond [(< n LINEAR-SAMPLE-LIMIT)
         (-discrete-sample/linear h wsum)]
        [else
         (match-define (ddext vs ws cws) (-discrete-ext dist))
         (define p (* (random) wsum))
         (vector-ref vs (binary-search/least-geq cws p))]))

(define (-discrete-sample/linear h wsum)
  (define p (* (random) wsum))
  (let loop ([p p] [iter (hash-iterate-first h)])
    (unless iter (error 'dist-sample:discrete-dist "internal error: out of values"))
    (define w (hash-iterate-value h iter))
    (cond [(> p w) (loop (- p w) (hash-iterate-next h iter))]
          [else (hash-iterate-key h iter)])))

(define (-discrete-pdf dist x log?)
  (define h (discrete-dist-h dist))
  (convert-p (hash-ref h x 0) log? #f))

(define (-discrete-measure dist ms)
  (define h (discrete-dist-h dist))
  (match-define (measurable atoms ivls) ms)
  (for/sum ([(v w) (in-hash h)])
    (cond [(hash-has-key? atoms v) w]
          [(and (rational? v) (ivls-contains? ivls v)) w]
          [else 0])))

(define (-discrete-normalize dist)
  (match-define (discrete-dist h wsum) dist)
  (cond [(or (zero? wsum) (= wsum 1)) dist]
        [else (for/discrete-dist ([(v w) (in-hash h)])
                (values v (/ w wsum)))]))

;; ------------------------------------------------------------
;; in-discrete-dist

(define-sequence-syntax in-discrete-dist
  (lambda () #'in-discrete-dist*)
  (lambda (stx)
    (syntax-case stx ()
      [[(v w) (in-X d-expr)]
       #'[(v w) (in-hash (in-dd/get-hash 'in-X d-expr))]]
      [_ #f])))

(define (in-discrete-dist* d)
  (in-hash (in-dd/get-hash 'in-discrete-dist d)))

(define (in-dd/get-hash who d)
  (if (discrete-dist? d)
      (discrete-dist-h d)
      (raise-argument-error who "discrete-dist?" d)))

;; ----------------------------------------
;; for/discrete-dist

(define-syntaxes (for/discrete-dist for*/discrete-dist)
  (let ()
    (define ((transformer for/derived) stx)
      (syntax-parse stx
        [(for/dd clauses . body)
         (with-syntax ([for/derived for/derived])
           #`(for/derived #,stx
                          ([dh (hash)] [wsum 0] #:result (discrete-dist dh wsum))
                          clauses
               (let-values ([(v w) (let () . body)])
                 (unless (and (rational? w) (>= w 0))
                   (for/dd-bad-weight 'for/dd v w))
                 (values (for/dd-hash-add dh v w) (+ wsum w)))))]))
    (values (transformer #'for/fold/derived)
            (transformer #'for*/fold/derived))))

(define (for/dd-hash-add h v w) ;; h : Hash[X => PosReal]
  (if (> w 0) (hash-set h v (+ w (hash-ref h v 0))) h))

(define (for/dd-bad-weight who v w)
  (error who
         (string-append "body produced invalid weight, expected nonnegative rational"
                        "\n  value: ~e\n  weight: ~e")
         v w))

;; ----------------------------------------
;; discrete-dist

(define-syntax (m:discrete-dist stx)
  (define-syntax-class vwpair
    #:attributes (value weight)
    #:description "pair of value and weight expressions"
    (pattern [value:expr weight:expr]))
  (syntax-parse stx
    [(discrete-dist p:vwpair ...)
     #'(hash->discrete-dist (hash (~@ p.value p.weight) ...))]))

;; ----------------------------------------
;; make-discrete-dist (match-expander)

(define-match-expander m:make-discrete-dist
  (lambda (stx)
    (syntax-parse stx
      [(_ vs:expr ws:expr)
       (syntax/loc stx
         (? discrete-dist?
            (app discrete-dist-values vs)
            (app discrete-dist-weights ws)))]))
  (make-variable-like-transformer #'make-discrete-dist))

;; ----------------------------------------
;; printer

(define (print-discrete-dist name h port mode)
  (define (recur x p)
    (case mode
      ((#t) (write x p))
      ((#f) (display x p))
      ((0 1) (print x p mode))))

  ;; Only two cases: 0 vs everything else
  (define (print-prefix p)
    (case mode
      [(0) (fprintf p "(~a" name)]
      [else (fprintf p "#<~a:" name)]))
  (define (print-suffix p)
    (case mode
      [(0) (write-string ")" p)]
      [else (write-string ">" p)]))

  (define (print-contents p leading-space)
    (define lead (if leading-space (make-string (add1 leading-space) #\space) " "))
    (define vals (hash-keys h #t))
    (for ([v (in-list vals)])
      (define w (hash-ref h v))
      (when leading-space
        (pretty-print-newline p (pretty-print-columns)))
      (write-string lead p)
      (write-string "[" p)
      (recur v p)
      (write-string " " p)
      (recur w p)
      (write-string "]" p)))

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

;; ----------------------------------------
;; flat contract

;; FIXME
(struct discrete-dist-of (pred)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'discrete-dist-of)
   (lambda (self) (list (discrete-dist-of-pred self))))
  #:property prop:procedure
  (lambda (self d)
    (match-define (discrete-dist-of pred) self)
    (match d
      [(discrete-dist h _)
       (for/and ([v (in-hash-keys h)]) (pred v))]
      [_ #f])))
