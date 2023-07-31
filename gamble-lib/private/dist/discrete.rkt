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
         "measurable.rkt"
         "sort.rkt")
(provide (all-defined-out))

;; The empty dist is represented as a discrete dist with no elements.

;; ============================================================
;; Discrete distribution

;; Categorical dist has support 1..N; discrete has arbitrary values as support.
;; Prints nicely (sort), but non-standard constructor, can't use as match pattern.

;; Not necessarily normalized.
;; Uses equal? to distinguish elements of support.

(define-dist-struct discrete-dist
  ([h hash?] ;; Hash[X => PosReal]
   [wsum real?])
  #:property prop:custom-write
  (lambda (obj port mode)
    (match-define (discrete-dist h _) obj)
    (print-discrete-dist 'discrete-dist h port mode))
  #:methods gen:dist
  [(define (-sample self)
     (match self [(discrete-dist h wsum) (discrete-sample h wsum)]))
   (define (-density self x)
     (match-define (discrete-dist h wsum) self)
     (hash-ref h x 0))
   (define (-measure self ms)
     (match-define (discrete-dist h wsum) self)
     (match ms
       [(measurable atoms ivls)
        (+ (for/sum ([v (in-hash-keys atoms)])
             (hash-ref h v 0))
           (if (null? ivls)
               0
               (for/sum ([(v w) (in-hash h)])
                 (if (ivls-contains? ivls v) w 0))))]))
   (define (-total-measure self)
     (match self [(discrete-dist h wsum) wsum]))]
  #:methods gen:enum-dist
  [(define (-enum self)
     (match self [(discrete-dist h wsum) (hash-keys h)]))
   (define (-finite? self) #t)
   (define (-infinite? self) #f)])

;; ----------------------------------------

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
                            (unless (and (rational? w) (>= w 0))
                              (error 'for/discrete-dist
                                     "expected (>=/c 0) for weight, got: ~e" w))
                            (values (dhash-add dh v w) (+ wsum w)))))]))
    (values (transformer #'for/fold/derived)
            (transformer #'for*/fold/derived))))

(define (hash->discrete-dist h)
  (cond [(and (immutable? h) (hash-equal? h) (not (impersonator? h))
              (for/fold ([s 0]) ([w (in-hash-values h)])
                (and (rational? w) (> w 0) (+ s w))))
         => (lambda (wsum) (discrete-dist h wsum))]
        [else
         (define-values (dh wsum)
           (for/fold ([dh (hash)] [wsum 0]) ([(v w) (in-hash h)])
             (unless (and (rational? w) (> w 0))
               (raise-argument-error 'hash->discrete-dist "(hash/c any/c (>/c 0))" h))
             (values (hash-set dh v (+ w (hash-ref dh v 0))) (+ wsum w))))
         (discrete-dist dh wsum)]))

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

(define-sequence-syntax in-discrete-dist
  (lambda () #'in-discrete-dist*)
  (lambda (stx)
    (syntax-case stx ()
      [[(v w) (in-X d-expr)]
       #'[(v w) (in-hash (get-hash/in-discrete-dist 'in-X d-expr))]]
      [_ #f])))

(define (in-discrete-dist* d)
  (in-hash (get-hash/in-discrete-dist 'in-discrete-dist d)))

(define (get-hash/in-discrete-dist who d)
  (if (discrete-dist? d)
      (discrete-dist-h d)
      (raise-argument-error who "discrete-dist?" d)))

;; ============================================================
;; Discrete dist support functions
;; -- Weights are not normalized

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
