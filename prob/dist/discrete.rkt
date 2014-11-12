;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/pretty
         racket/dict
         racket/vector
         "../private/dist.rkt"
         "../private/dist-define.rkt"
         "../private/dist-impl.rkt"
         "../private/sort.rkt")
(provide discrete-dist
         discrete-dist?
         (contract-out
          [make-discrete-dist
           (->* [dict?] [#:normalize? any/c] discrete-dist?)]
          [make-discrete-dist*
           (->* [vector?]
                [(vectorof (>=/c 0))
                 #:normalize? any/c #:sort? any/c]
                discrete-dist?)]
          [normalize-discrete-dist
           (-> discrete-dist? discrete-dist?)]
          [discrete-dist-values
           (-> discrete-dist? vector?)]
          [discrete-dist-weights
           (-> discrete-dist? vector?)]))

;; ============================================================
;; Discrete distribution

;; Categorical dist has support 1..N; discrete has arbitrary values as support.
;; Prints nicely (sort), but non-standard constructor, can't use as match pattern.

;; Not automatically normalized.
;; Uses equal? to distinguish elements of support.

(define-dist-type *discrete-dist
  ([vs vector?]
   [ws vector?]
   [wsum real?])
  #:pdf discrete-pdf
  #:sample discrete-sample
  #:enum vs
  #:support 'finite
  #:no-provide
  #:extra
  [#:property prop:custom-write
   (lambda (obj port mode)
     (print-discrete-dist (*discrete-dist-vs obj) (*discrete-dist-ws obj) port mode))])

(define (discrete-dist? x) (*discrete-dist? x))
(define (discrete-dist-values d) (*discrete-dist-vs d))
(define (discrete-dist-weights d) (*discrete-dist-ws d))

(define-syntax (discrete-dist stx)
  (define-splicing-syntax-class maybe-normalize
    (pattern (~seq #:normalize? normalize?:expr))
    (pattern (~seq) #:with normalize? #'#t))
  (define-syntax-class vwpair
    #:description "pair of value and weight expressions"
    (pattern [value:expr weight]
             #:declare weight (expr/c #'(>=/c 0))))
  (syntax-parse stx
    [(discrete-dist :maybe-normalize p:vwpair ...)
     #'(make-discrete-dist* #:normalize? normalize?
                            #:sort? #t
                            (vector p.value ...) (vector p.weight ...))]))

(define (make-discrete-dist dict #:normalize? [normalize? #t])
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
  (make-discrete-dist* vs ws #:normalize? normalize? #:sort? #t))

(define (make-discrete-dist* vs
                             [ws (let ([len (vector-length vs)])
                                   (make-vector len (/ len)))]
                             #:normalize? [normalize? #t]
                             #:sort? [sort? #t])
  (unless (= (vector-length vs) (vector-length ws))
    (error 'make-discrete-dist
           "values and weights vectors have different lengths\n  values: ~e\n  weights: ~e"
           vs ws))
  (define-values (vs1 ws1)
    (if sort?
        (combine-duplicates vs ws)
        (values vs ws)))
  (define vs* (vector->immutable-vector vs1))
  (define-values (ws* wsum*)
    (if normalize?
        (let ([wsum (vector-sum ws1)])
          (values (vector->immutable-vector
                   (vector-map (lambda (w) (/ w wsum)) ws1))
                  1))
        (values (vector->immutable-vector ws1)
                (vector-sum ws1))))
  (*discrete-dist vs* ws* wsum*))

(define (normalize-discrete-dist d)
  (make-discrete-dist* (*discrete-dist-vs d) (*discrete-dist-ws d) #:normalize? #t))

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


;; ============================================================

;; ------------------------------------------------------------
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
      (error 'discrete-dist:sample "out of values"))
    (cond [(< p (vector-ref ws i))
           (vector-ref vs i)]
          [else
           (loop (add1 i) (- p (vector-ref ws i)))])))
