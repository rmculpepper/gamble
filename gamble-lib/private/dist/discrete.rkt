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
         (submod "util.rkt" weights)
         (submod "util.rkt" search)
         (submod "util.rkt" define)
         "../util/density.rkt"
         "measurable.rkt")
(provide (all-defined-out))

;; ============================================================
;; Boolean Bernoulli distribution

(define-dist-struct boolean-dist
  ([p probability?])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (boolean-dist p) self)
     (<= (random) p))
   (define (-pdf self x log?)
     (match-define (boolean-dist p) self)
     (define r (cond [(eq? x #t) p] [(eq? x #f) (- 1 p)] [else 0]))
     (if log? (log (fl r)) r))
   (define (-measure self ms)
     (match-define (boolean-dist p) self)
     (match-define (measurable atoms _) ms)
     (+ (if (hash-has-key? atoms #t) p 0)
        (if (hash-has-key? atoms #f) (- 1 p) 0)))
   (define (-total-measure self) 1)
   (define (-count self) 2)]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-list '(#t #f)))
   (define (-wsequence self)
     (match-define (boolean-dist p) self)
     (in-hash (hash #t p #f (- 1 p))))])

;; ============================================================
;; Discrete distribution

;; Not necessarily normalized.
;; The empty dist is represented as a discrete dist with no elements.

;; Uses equal? to distinguish elements of support.

;; DiscreteDist[X]:
(struct discrete-dist (h wsum)
  #:property prop:custom-write
  (lambda (self port mode)
    (define h (discrete-dist-h self))
    (print-discrete-dist 'discrete-dist h port mode))
  #:methods gen:meta-dist
  [(define (-type self) 'discrete-dist)
   (define (-params self) (list (discrete-dist-h self)))]
  #:methods gen:dist
  [(define (-sample self)
     (-discrete-sample self))
   (define (-density self x log?)
     (density log? (-discrete-pdf self x log?) #;0))
   (define (-pdf self x log?)
     (-discrete-pdf self x log?))
   (define (-measure self ms)
     (-discrete-measure self ms))
   (define (-total-measure self)
     (discrete-dist-wsum self))
   (define (-count self)
     (hash-count (discrete-dist-h self)))]
  #:methods gen:enumerable-dist
  [(define (-sequence self)
     (in-hash-keys (discrete-dist-h self)))
   (define (-wsequence self)
     (in-hash (discrete-dist-h self)))])

;; ----------------------------------------
;; Constructor

(define (hash->discrete-dist h #:normalize? [normalize? #t])
  (define who 'hash->discrete-dist)
  (define (bad) (raise-argument-error who "(hash/c any/c (>=/c 0))" h))
  (cond [(and (hash? h) (immutable? h) (not (impersonator? h))
              (hash-equal? h) (hash-strong? h))
         (define-values (dh ws any-exact?)
           (for/fold ([dh h] [ws 0] [any-exact? #f])
                     ([(v w) (in-hash h)])
             (unless (and (rational? w) (>= w 0)) (bad))
             (values (if (zero? w) (hash-remove dh v) dh)
                     (+ ws w)
                     (or any-exact? (and (exact? w) (not (zero? w)))))))
         (-hash->discrete-dist h ws any-exact? normalize?)]
        [(hash? h)
         (define-values (dh ws any-exact?)
           (for/fold ([dh (hash)] [ws 0] [any-exact? #f])
                     ([(v w) (in-hash h)])
             (unless (and (rational? w) (>= w 0)) (bad))
             (values (if (zero? w) dh (hash-set dh v (+ w (hash-ref dh v 0))))
                     (+ ws w)
                     (or any-exact? (and (exact? w) (not (zero? w)))))))
         (-hash->discrete-dist h ws any-exact? normalize?)]
        [else (bad)]))

(define (-hash->discrete-dist h wsum any-exact? normalize?)
  (cond [(and any-exact? (inexact? wsum))
         (define-values (dh ws)
           (for/fold ([dh (hash)] [ws 0.0]) ([(v w) (in-hash h)])
             (values (hash-set dh v (fl w)) (+ ws (fl w)))))
         (-hash->discrete-dist dh ws #f normalize?)]
        [(and normalize? (not (or (= wsum 1) (= wsum 0))))
         (define-values (dh ws)
           (for/fold ([dh (hash)] [ws 0]) ([(v w) (in-hash h)])
             (define w* (/ w wsum))
             (values (hash-set dh v w*) (+ ws w*))))
         (discrete-dist dh ws)]
        [else (discrete-dist h wsum)]))

(define empty-discrete-dist (discrete-dist '#hash() 0))

(define (log-hash->normalized-discrete-dist lh)
  (define who 'log-hash->normalized-discrete-dist)
  (define lwmax (for/fold ([lwmax -inf.0]) ([(v lw) (in-hash lh)]) (max lwmax lw)))
  (define lnwsum (log (for/sum ([lw (in-hash-values lh)]) (exp (- lw lwmax)))))
  (define h
    (for/fold ([h (hash)]) ([(v lw) (in-hash lh)])
      (define w (exp (- lw lwmax lnwsum)))
      (if (> w -inf.0) (hash-set h v w) h)))
  (discrete-dist h 1.0))

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

;; ----------------------------------------
;; More constructors

(define (dirac-dist v [w 1])
  (discrete-dist (hash v w) w))

(define (make-discrete-dist vs [ws #f] #:normalize? [normalize? #t])
  (define who 'make-discrete-dist)
  (define (badws) (raise-argument-error who "(or/c #f (vectorof (>=/c 0)))" ws))
  (unless (vector? vs) (raise-argument-error who "vector?" vs))
  (unless (or (not ws) (vector? ws)) (badws))
  (when ws
    (unless (= (vector-length vs) (vector-length ws))
      (error who (string-append
                  "values vector and weights vectors have different lengths"
                  "\n  values: ~e\n  weights: ~e")
             vs ws)))
  (cond [(zero? (vector-length vs))
         empty-discrete-dist]
        [(eq? ws #f)
         (for/discrete-dist #:normalize? normalize?
                            ([v (in-vector vs)])
           (values v 1))]
        [else
         (for/discrete-dist #:normalize? normalize?
                            ([v (in-vector vs)] [w (in-vector ws)])
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
        [else (-hash->discrete-dist h wsum #f #t)]))

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
      (define-splicing-syntax-class maybe-normalize
        (pattern (~seq #:normalize? normalize?:expr))
        (pattern (~seq) #:with normalize? #'(quote #t)))
      (syntax-parse stx
        [(for/dd :maybe-normalize (clause ...) . body)
         (with-syntax ([for/derived for/derived])
           #`(for/derived #,stx
                          ([dh (hash)]
                           [wsum 0]
                           [any-exact? #f]
                           #:result (-hash->discrete-dist dh wsum any-exact? normalize?))
                          (clause ...)
               (let-values ([(v w) (let () . body)])
                 (unless (and (rational? w) (>= w 0))
                   (for/dd-bad-weight 'for/dd v w))
                 (values (for/dd-hash-add dh v w)
                         (+ wsum w)
                         (or any-exact? (and (exact? w) (not (zero? w))))))))]))
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
;; resampling

;; discrete-dist-resample : DiscreteDist Nat -> Vector
(define (discrete-dist-resample dist n #:mode [mode 'multinomial])
  (when (zero? (discrete-dist-wsum dist))
    (error 'discrete-dist-resample "empty dist"))
  (define r (make-vector n #f))
  (case mode
    [(multinomial #f)
     (for ([i (in-range n)])
       (vector-set! r i (dist-sample dist)))]
    [(residual)
     (match-define (discrete-dist h wsum) dist)
     (define ww (/ wsum n))
     (define-values (h* wsum* next-index)
       (for/fold ([h h] [wsum 0] [i 0]) ([(v w) (in-hash h)])
         (define whole (floor (/ w ww)))
         (cond [(zero? whole) (values h (+ wsum w) i)]
               [else
                (for ([j (in-range i (+ i (exact whole)))])
                  (vector-set! r j v))
                (define wrem (max 0 (- w (* whole ww))))
                (values (hash-set h v wrem) (+ wsum wrem) (+ i whole))])))
     (define dist* (discrete-dist h* wsum*))
     (for ([j (in-range next-index n)])
       (vector-set! r j (dist-sample dist*)))]
    [else (error 'discrete-dist-resample "bad resampling mode: ~e" mode)])
  r)


;; ----------------------------------------
;; flat contract

;; FIXME
(struct discrete-distof (pred)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'discrete-distof)
   (lambda (self) (list (discrete-distof-pred self))))
  #:property prop:procedure
  (lambda (self d)
    (match-define (discrete-distof pred) self)
    (match d
      [(discrete-dist h _)
       (for/and ([v (in-hash-keys h)]) (pred v))]
      [_ #f])))

;; ============================================================

(module+ test
  (require rackunit)

  (let ([ed (hash->discrete-dist (hash 'a 1/2 'b 1/3 'c 1/6))])
    (check-equal? (dist-pdf ed 'a #f) 1/2)
    (check-equal? (dist-pdf ed 'a #t) (log 1/2))
    (check-equal? (dist-pdf ed 'z #f) 0)
    (check-equal? (dist-pdf ed 'z #t) -inf.0)
    (check-equal? (dist-density ed 'a) (density #f 1/2 #;0))
    (check-equal? (dist-measure ed (measurable (hash 'a #t 'c #t) null)) (+ 1/2 1/6))
    (check-equal? (dist-total-measure ed) 1)
    (check-equal? (for/hash ([v (in-vector (discrete-dist-values ed))]
                             [w (in-vector (discrete-dist-weights ed))])
                    (values v w))
                  (discrete-dist->hash ed))
    (check-equal? (for/hash ([(v w) (in-dist ed)]) (values v w))
                  (discrete-dist->hash ed))
    (check-equal? (for/hash ([v (in-dist-values ed)]) (values v (dist-pdf ed v)))
                  (discrete-dist->hash ed))
    (check-equal? (for/hash ([(v w) (in-discrete-dist ed)]) (values v w))
                  (discrete-dist->hash ed))
    (check-equal? (for/discrete-dist ([(v w) (in-discrete-dist ed)]) (values v w)) ed)
    (void))
  
  (let ([md (hash->discrete-dist (hash 'a 0.5 'b 1/3 'c 1/6))])
    (check-equal? (dist-pdf md 'a #f) 0.5)
    (check-equal? (dist-pdf md 'b #f) #i1/3)
    (check-equal? (dist-pdf md 'z #f) 0)
    (check-equal? (dist-measure md (measurable (hash 'a #t 'c #t) null)) (+ #i1/2 #i1/6))
    (check-equal? (dist-total-measure md) 1.0))

  (let ([pd (hash->discrete-dist (hash 'x 1/5 'y 2/5))]) ;; partial
    (check-equal? (dist-pdf pd 'x #f) 1/5)
    (check-equal? (dist-total-measure pd) 3/5))

  (let ([id (make-discrete-dist (vector 1 2 3 4 5))])
    (check-equal? (dist-pdf id 3 #f) 1/5)
    (check-equal? (dist-measure id (measurable (hash) '(1 5))) 3/5)
    (check-equal? (dist-measure id (measurable (hash 1 #t) '(1 5))) 4/5))

  (begin))
