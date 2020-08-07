;; Copyright 2014-2020 Ryan Culpepper
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
         "base.rkt"
         "define.rkt"
         "sort.rkt")
(provide (all-defined-out))

;; ============================================================
;; Discrete distribution

;; Categorical dist has support 1..N; discrete has arbitrary values as support.
;; Prints nicely (sort), but non-standard constructor, can't use as match pattern.

;; Not necessarily normalized.
;; Uses equal? to distinguish elements of support.

(define-dist-type discrete-dist
  ([vs vector?]
   [ws vector?]
   [wsum real?])
  #:counting
  #:pdf discrete-pdf
  #:sample discrete-sample
  #:total-mass wsum
  #:enum vs
  #:support 'finite
  #:provide (begin)
  #:extra
  [#:property prop:custom-write
   (lambda (obj port mode)
     (match-define (discrete-dist vs ws _) obj)
     (print-discrete-dist 'discrete-dist vs ws port mode))])

(define (discrete-dist->inexact d)
  (match d
    [(discrete-dist vs ws wsum)
     (define ws* (vector-map exact->inexact ws))
     (make-discrete-dist vs ws* #:normalize? #f)]))

(define-syntax (discrete-dist:m stx)
  (define-splicing-syntax-class maybe-normalize
    (pattern (~seq #:normalize? normalize?:expr))
    (pattern (~seq) #:with normalize? #'#f))
  (define-syntax-class vwpair
    #:description "pair of value and weight expressions"
    (pattern [value:expr weight]
             #:declare weight (expr/c #'(>=/c 0))))
  (syntax-parse stx
    [(discrete-dist :maybe-normalize p:vwpair ...)
     #'(make-discrete-dist #:normalize? normalize?
                           (vector p.value ...) (vector p.weight ...))]))

(define (alist->discrete-dist alist #:normalize? [normalize? #f])
  (define len (length alist))
  (define vs (make-vector len))
  (define ws (make-vector len))
  (for ([v+w (in-list alist)]
        [i (in-naturals)])
    (unless (and (pair? v+w) (let ([w (cdr v+w)]) (and (rational? w) (>= w 0))))
      (raise-argument-error 'alist->discrete-dist "(listof (cons/c any/c (>=/c 0))" alist))
    (vector-set! vs i (car v+w))
    (vector-set! ws i (cdr v+w)))
  (make-discrete-dist vs ws #:normalize? normalize?))

(define (make-discrete-dist vs
                            [ws (let ([len (vector-length vs)])
                                  (make-vector len (/ len)))]
                            #:normalize? [normalize? #f])
  (unless (= (vector-length vs) (vector-length ws))
    (error 'make-discrete-dist
           "values and weights vectors have different lengths\n  values: ~e\n  weights: ~e"
           vs ws))
  (define-values (vs1 ws1)
    (combine-duplicates vs ws))
  (define vs* (vector->immutable-vector vs1))
  (define-values (ws* wsum*)
    (let ([wsum (for/sum ([w (in-vector ws1)]) w)])
      (when normalize?
        (unless (and (rational? wsum) (positive? wsum)) ;; rational = finite real
          (error 'make-discrete-dist "improper weights\n  weights: ~e" ws)))
      (if normalize?
          (values (vector->immutable-vector
                   (vector-map (lambda (w) (/ w wsum)) ws1))
                  1)
          (values (vector->immutable-vector ws1)
                  wsum))))
  (discrete-dist vs* ws* wsum*))

(define-module-boundary-contract make-discrete-dist:c make-discrete-dist
  (->* [vector?] [(vectorof (>= 0)) #:normalize? boolean?] any))

(define-match-expander make-discrete-dist:m
  (lambda (stx)
    (syntax-parse stx
      [(_ vs:expr ws:expr) (syntax/loc stx (discrete-dist vs ws _))]))
  (make-variable-like-transformer #'make-discrete-dist:c))

(define (normalize-discrete-dist d)
  (cond [(= (discrete-dist-wsum d) 1)
         d]
        [(zero? (discrete-dist-wsum d))
         (error 'normalize-discrete-dist "cannot normalize empty measure")]
        [else
         (make-discrete-dist (discrete-dist-vs d) (discrete-dist-ws d) #:normalize? #t)]))

(define (print-discrete-dist name vs ws port mode)
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
    (let ([lead (if leading-space (make-string (add1 leading-space) #\space) " ")])
      (for ([v (in-vector vs)]
            [w (in-vector ws)])
        (when leading-space
          (pretty-print-newline p (pretty-print-columns)))
        (write-string lead p)
        (write-string "[" p)
        (recur v p)
        (write-string " " p)
        (recur w p)
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

;; ------------------------------------------------------------

(struct discrete-dist-of (pred)
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (self) 'discrete-dist-of)
                                  (lambda (self) (list (discrete-dist-of-pred self))))
  #:property prop:procedure
  (lambda (self d)
    (match-define (discrete-dist-of pred) self)
    (match d
      [(discrete-dist vs _ _)
       (for/and ([v (in-vector vs)]) (pred v))]
      [_ #f])))

;; ------------------------------------------------------------

(begin-for-syntax
  (define (in-dist-transformer stx normalize?)
    (syntax-case stx ()
      [[(v w) (in-X d-expr)]
       (let ()
         (unless (identifier? #'v)
           (raise-syntax-error #'in-X "expected identifier" stx #'v))
         (unless (identifier? #'w)
           (raise-syntax-error #'in-X "expected identifier" stx #'w))
         (with-syntax ([normalize? normalize?])
           #'[(v w)
              (:do-in
               ([(get-v get-w len) (in-dist:extract 'in-X d-expr normalize?)])
               (void)
               ([i 0])
               (< i len)
               ([(v) (get-v i)] [(w) (get-w i)])
               #t
               #t
               ((add1 i)))]))]
      [_ #f])))

(define-sequence-syntax in-dist
  (lambda () #'in-dist*)
  (lambda (stx) (in-dist-transformer stx #f)))

(define (in-dist* d)
  (define-values (get-v get-w len) (in-dist:extract 'in-dist d #f))
  (in-parallel (sequence-map get-v (in-range len))
               (sequence-map get-w (in-range len))))

(define (in-dist:extract who d normalize?)
  (cond [(discrete-dist? d)
         (match-define (discrete-dist vs ws wsum) d)
         (values (lambda (i) (vector-ref vs i))
                 (if normalize?
                     (lambda (i) (/ (vector-ref ws i) wsum))
                     (lambda (i) (vector-ref ws i)))
                 (vector-length vs))]
        [(finite-dist? d)
         (define enum (dist-enum d))
         (cond [(integer? enum)
                (define (get-w i) (dist-pdf d i))
                (values values get-w enum)]
               [(vector? enum)
                (define len (vector-length enum))
                (define (get-w i) (dist-pdf d (vector-ref enum i)))
                (values enum get-w len)]
               [else
                (error who "internal error: non-enumerable finite dist\n  dist: ~e" d)])]
        [else (raise-argument-error who "finite-dist?" d)]))

;; ============================================================
;; Discrete dist support functions
;; -- Weights are not normalized

(define (discrete-pdf vs ws wsum x log?)
  (define p
    (or (for/or ([v (in-vector vs)]
                 [w (in-vector ws)])
          (and (equal? x v) (/ w wsum)))
        0))
  (convert-p p log? #f))

(define (discrete-sample vs ws wsum)
  (define p (* (random) wsum))
  (let loop ([i 0] [p p])
    (unless (< i (vector-length ws))
      (error 'discrete-sample "out of values"))
    (cond [(< p (vector-ref ws i))
           (vector-ref vs i)]
          [else
           (loop (add1 i) (- p (vector-ref ws i)))])))