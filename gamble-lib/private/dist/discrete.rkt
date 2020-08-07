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
  ([h hash?] ;; Hash[X => PosReal]
   [wsum real?])
  #:counting
  #:pdf discrete-pdf
  #:sample discrete-sample
  #:total-mass wsum
  #:enum (hash-keys h)
  #:support 'finite
  #:provide (begin)
  #:extra
  [#:property prop:custom-write
   (lambda (obj port mode)
     (match-define (discrete-dist h _) obj)
     (print-discrete-dist 'discrete-dist h port mode))])

;; dhash[X] = Hash[X => PosReal]
(define (dhash-add h v w)
  (if (> w 0) (hash-set h v (+ w (hash-ref h v 0))) h))

(define-syntaxes (for/discrete-dist for*/discrete-dist)
  (let ()
    (define ((transformer for/derived) stx)
      (syntax-parse stx
        [(_ clauses . body)
         (with-syntax ([for/derived for/derived])
           #`(for/derived #,stx
                          ([dh (hash)] [wsum 0]
                           #:result (discrete-dist dh wsum))
                          clauses
                          (let-values ([(v w) (let () . body)])
                            (values (dhash-add dh v w) (+ wsum w)))))]))
    (values (transformer #'for/fold/derived)
            (transformer #'for*/fold/derived))))

(define (hash->discrete-dist h)
  (cond [(and (immutable? h) (hash-equal? h))
         (define wsum (for/sum ([(v w) (in-hash h)]) w))
         (discrete-dist h wsum)]
        [else
         (hash->discrete-dist (immutable-hash h))]))

(define (immutable-hash h)
  ;; FIXME: preserve weak?
  (for/fold ([dh (hash)]) ([(v w) (in-hash h)])
    (dhash-add dh v w)))

(define (alist->discrete-dist alist)
  (for/discrete-dist ([v+w (in-list alist)])
    (match-define (cons v w) v+w)
    (values v w)))

(define-syntax (discrete-dist:m stx)
  (define-syntax-class vwpair
    #:description "pair of value and weight expressions"
    (pattern [value:expr weight]
             #:declare weight (expr/c #'(>=/c 0))))
  (syntax-parse stx
    [(discrete-dist p:vwpair ...)
     #'(make-discrete-dist (vector p.value ...) (vector p.weight ...))]))

(define make-discrete-dist
  (case-lambda
    [(vs)
     (define w (/ (max 1 (vector-length vs))))
     (for/discrete-dist ([v (in-vector vs)])
       (values v w))]
    [(vs ws)
     (unless (= (vector-length vs) (vector-length ws))
       (error 'make-discrete-dist
              "values and weights vectors have different lengths\n  values: ~e\n  weights: ~e"
              vs ws))
     (for/discrete-dist ([v (in-vector vs)] [w (in-vector ws)])
       (values v w))]))

(define-module-boundary-contract make-discrete-dist:c make-discrete-dist
  (->* [vector?] [(vectorof (>= 0))] any))

(define-match-expander make-discrete-dist:m
  (lambda (stx)
    (syntax-parse stx
      [(_ vs:expr ws:expr) (syntax/loc stx (discrete-dist vs ws _))]))
  (make-variable-like-transformer #'make-discrete-dist:c))

(define (normalize-discrete-dist d)
  (match-define (discrete-dist h wsum) d)
  (cond [(zero? wsum)
         (error 'normalize-discrete-dist "cannot normalize empty distribution")]
        [(= wsum 1) d]
        [else
         (for/discrete-dist ([(v w) (in-hash h)])
           (values v (/ w wsum)))]))

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
    (let ([lead (if leading-space (make-string (add1 leading-space) #\space) " ")])
      (for ([e (sort (hash-map h cons) value< #:key car)])
        (match-define (cons v w) e)
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
      [(discrete-dist h _)
       (for/and ([v (in-hash-keys h)]) (pred v))]
      [_ #f])))

;; ------------------------------------------------------------

(begin-for-syntax
  (define (in-dist-transformer stx)
    (syntax-case stx ()
      [[(v w) (in-X d-expr)]
       (let ()
         (unless (identifier? #'v)
           (raise-syntax-error #'in-X "expected identifier" stx #'v))
         (unless (identifier? #'w)
           (raise-syntax-error #'in-X "expected identifier" stx #'w))
         #'[(v w) (in-hash (finite-dist->hash 'in-X d-expr))])]
      [_ #f])))

(define-sequence-syntax in-dist
  (lambda () #'in-dist*)
  (lambda (stx) (in-dist-transformer stx)))

(define (in-dist* d)
  (in-hash (finite-dist->hash 'in-dist d)))

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

;; ============================================================
;; Discrete dist support functions
;; -- Weights are not normalized

(define (discrete-pdf h wsum x log?)
  (define p (hash-ref h x 0))
  (convert-p p log? #f))

(define LINEAR-SAMPLE-LIMIT 10)
(define hash=>sample-vector (make-weak-hasheq))

(define (discrete-sample h wsum)
  (cond [(zero? wsum) (error 'discrete-sample "empty distribution")]
        [(< (hash-count h) LINEAR-SAMPLE-LIMIT)
         (discrete-sample/linear h wsum)]
        [else
         ((hash-ref! hash=>sample-vector h
                     (lambda () (build-discrete-sampler h wsum))))]))

(define (build-discrete-sampler h wsum)
  (define sv (make-vector (* 2 (hash-count h))))
  (for/fold ([cw 0]) ([i (in-naturals)] [(v w) (in-hash h)])
    (vector-set! sv (+ i i 0) v)
    (vector-set! sv (+ i i 1) cw)
    (+ cw w))
  (lambda ()
    (define (value-at k) (vector-ref sv k))
    (define (weight-at k) (vector-ref sv (+ k 1)))
    (define p (* (random) wsum))
    ;; looking for greatest index k st cw(k) <= p
    (let loop ([a 0] [b (vector-length sv)]) ;; a,b even, a < b, a valid, b invalid
      (cond [(= (+ a 2) b) (value-at a)]
            [else
             (define m (* 2 (quotient (+ a b) 4)))
             (cond [(<= (weight-at m) p) (loop m b)]
                   [else (loop a m)])]))))

(define (discrete-sample/linear h wsum)
  (define p (* (random) wsum))
  (car (or (for/or ([(v w) (in-hash h)])
             (cond [(> p w) (set! p (- p w)) #f]
                   [else (list v)]))
           (error 'discrete-sample "internal error: out of values"))))
