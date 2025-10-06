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
         "../util/dnum.rkt"
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
   (define (-density self x)
     (linear-dnum (-pdf self x #f)))
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

;; DiscreteDist[X] = (discrete-dist (Hash X PosReal) NNReal DDExt/#f)
(struct discrete-dist (h wsum [ext #:mutable])
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
   (define (-pdf self x log?)
     (-discrete-pdf self x log?))
   (define (-density self x)
     (linear-dnum (-pdf self x #f)))
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

;; hash->discrete-dist : (Hash X NNReal) -> DiscreteDist
;; First need to ensure immutable authentic strong hash-equal.
;; If all exact, can compute wsum in this pass; if inexact, will need
;; compensated sum, leave to second pass. Assume zero weights rare.
(define (hash->discrete-dist h #:normalize? [normalize? #t])
  (define who 'hash->discrete-dist)
  (define (bad) (raise-argument-error who "(hash/c any/c (>=/c 0))" h))
  (cond [(and (hash? h) (immutable? h) (not (impersonator? h))
              (hash-equal? h) (hash-strong? h))
         (define-values (dh ewsum) ;; Hash, NNExactReal/#f
           (for/fold ([dh h] [ewsum 0]) ([(v w) (in-hash h)])
             (unless (and (rational? w) (>= w 0)) (bad))
             (cond [(zero? w) (values (hash-remove dh v) ewsum)]
                   [else (values dh (and ewsum (exact? w) (+ ewsum w)))])))
         (-hash->discrete-dist h ewsum normalize?)]
        [(hash? h)
         (define-values (dh ewsum)
           (for/fold ([dh (hash)] [ewsum 0]) ([(v w) (in-hash h)])
             (unless (and (rational? w) (>= w 0)) (bad))
             (cond [(zero? w) (values dh ewsum)]
                   [else (values (hash-set dh v w)
                                 (and ewsum (exact? w) (+ ewsum w)))])))
         (-hash->discrete-dist h ewsum normalize?)]
        [else (bad)]))

;; -hash->discrete-dist : (Hash X PosReal) NNExactReal/#f Boolean -> DiscreteDist
;; If ewsum is false, then some inexact weight; else ewsum is exact weight sum.
;; Prioritize all-exact and all-inexact cases.
(define (-hash->discrete-dist h ewsum normalize?)
  (cond [ewsum ;; no inexact weights
         (cond [(and normalize? (not (= ewsum 1)) (not (= ewsum 0)))
                (define dh (for/hash ([(v w) (in-hash h)])
                             (values v (/ w ewsum))))
                (discrete-dist dh 1 #f)]
               [else (discrete-dist h ewsum #f)])]
        [else ;; need to compute inexact sum, use compensated addition
         (define-values (dh iwsum)
           (for/fold ([dh h] [s 0.0] [c 0.0] #:result (values dh s))
                     ([(v w) (in-hash h)])
             (define flw (fl w))
             (define-values (s* c*) (compensated+ flw s c))
             (define dh* (if (exact? w) (hash-set dh v flw) dh))
             (values dh* s* c*)))
         (cond [(and normalize? (not (= iwsum 1.0))) ;; can't be zero
                (define inv-iwsum (/ iwsum))
                (define-values (new-dh new-iwsum)
                  (for/fold ([ddh (hash)] [s 0.0] [c 0.0] #:result (values ddh s))
                            ([(v w) (in-hash dh)])
                    (define w* (* w inv-iwsum))
                    (define-values (s* c*) (compensated+ w* s c))
                    (values (hash-set ddh v w*) s* c*)))
                (discrete-dist new-dh new-iwsum #f)]
               [else (discrete-dist dh iwsum #f)])]))

;; ----------------------------------------
;; DDExt

;; DDExt is (ddext (Vectorof X) (Vectorof PosReal) (Vectorof PosFlonum))
;; - cws[k] is sum of ws[0..k] (inclusive)
(struct ddext (vs ws cws) #:transparent)

(define (-discrete-ext dist)
  (define (calc-ext)
    (define h (discrete-dist-h dist))
    (define len (hash-count h))
    (define vs (make-vector len))
    (define ws (make-vector len))   ;; exact or flonum
    (for ([(v w) (in-hash h)] [i (in-naturals)])
      (vector-set! vs i v)
      (vector-set! ws i w))
    (define cws (make-cws ws))      ;; always flonums
    (ddext (vector->immutable-vector vs)
           (vector->immutable-vector ws)
           cws))
  (or (discrete-dist-ext dist)
      (let ([ext (calc-ext)])
        (set-discrete-dist-ext! dist ext)
        ext)))

(define (make-uniform-ddext vs w)
  (define n (vector-length vs))
  (define ws (make-vector n w))
  (define cws (make-cws ws))
  (ddext vs (vector->immutable-vector ws) cws))

(define (make-cws ws)
  (define n (vector-length ws))
  (define cws (make-vector n 0))
  (for/fold ([s 0.0] [c 0.0]) ([i (in-range n)] [w (in-vector ws)])
    (define-values (s* c*) (compensated+ (fl w) s c))
    (vector-set! cws i s*)
    (values s* c*))
  (vector->immutable-vector cws))

;; ----------------------------------------
;; More constructors

(define empty-discrete-dist
  (discrete-dist '#hash() 0 (ddext '#() '#() '#())))

(define (dirac-dist v)
  (discrete-dist (hash v 1) 1 #f))

(define (make-discrete-dist vs [ws #f]
                            #:log-weight? [log-weight? #f]
                            #:normalize? [normalize? #t])
  (define who 'make-discrete-dist)
  (define (badws)
    (if log-weight?
        (raise-argument-error who "(or/c #f (vectorof flonum?))" ws)
        (raise-argument-error who "(or/c #f (vectorof (>=/c 0)))" ws)))
  (unless (vector? vs) (raise-argument-error who "vector?" vs))
  (unless (or (not ws) (vector? ws)) (badws))
  (when ws
    (unless (= (vector-length vs) (vector-length ws))
      (error who (string-append
                  "value vector and weight vector have different lengths"
                  "\n  values: ~e\n  weights: ~e")
             vs ws)))
  (define n (vector-length vs))
  (cond [(zero? n) empty-discrete-dist]
        [(eq? ws #f)
         (define w (if normalize? (/ n) 1))
         (define h (for/fold ([h (hash)]) ([v (in-vector vs)])
                     (hash-set h v (+ w (hash-ref h v 0)))))
         (discrete-dist h (if normalize? 1 n) #f)]
        [log-weight?
         (let ([vs (vector->immutable-vector vs)]
               [lws (vector->immutable-vector ws)])
           (define maxlw
             (for/fold ([maxlw -inf.0]) ([lw (in-vector lws)])
               (if (flonum? lw) (max lw maxlw) (badws))))
           (define h
             (for/fold ([h (hash)]) ([v (in-vector vs)] [lw (in-vector lws)])
               (define w (exp (- lw maxlw)))
               (hash-set h v (+ w (hash-ref h v 0.0)))))
           (-hash->discrete-dist h #f normalize?))]
        [else
         (let ([vs (vector->immutable-vector vs)]
               [ws (vector->immutable-vector ws)])
           (define-values (h ewsum)
             (for/fold ([h (hash)] [ewsum 0])
                       ([v (in-vector vs)] [w (in-vector ws)])
               (unless (and (rational? w) (>= w 0)) (badws))
               (cond [(zero? w) (values h ewsum)]
                     [else (values (hash-set h v (+ w (hash-ref h v 0)))
                                   (and ewsum (exact? w) (+ ewsum w)))])))
           (define dd (-hash->discrete-dist h ewsum normalize?))
           (when (and (eq? (discrete-dist-h dd) h) (= (hash-count h) n))
             (define cws (make-cws ws))
             (set-discrete-dist-ext! dd (ddext vs ws cws)))
           dd)]
        #;
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
  (match-define (discrete-dist h wsum _) dist)
  (define n (hash-count h))
  (when (zero? n) (error 'dist-sample "empty distribution\n  dist: ~e" dist))
  (cond [(< n LINEAR-SAMPLE-LIMIT)
         (-discrete-sample/linear h wsum)]
        [else
         (match-define (ddext vs ws cws) (-discrete-ext dist))
         (define p (* (random) wsum))
         (vector-ref vs (binary-search/least-geq cws p))]))

(define (-discrete-sample/linear h wsum)
  (define p (* (random) wsum))
  (let loop ([p p] [iter (hash-iterate-first h)])
    (cond [iter
           (define w (hash-iterate-value h iter))
           (cond [(> p w) (loop (- p w) (hash-iterate-next h iter))]
                 [else (hash-iterate-key h iter)])]
          [else ;; out of values
           #;(error 'dist-sample "internal error: out of values\n  dist: ~e" dist)
           ;; Probably floating-point error; just return first value.
           (hash-iterate-key h (hash-iterate-first h))])))

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
  (match-define (discrete-dist h wsum _) dist)
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
                           [ewsum 0]
                           #:result (-hash->discrete-dist dh ewsum normalize?))
                          (clause ...)
               (let-values ([(v w) (let () . body)])
                 (unless (and (rational? w) (>= w 0))
                   (for/dd-bad-weight 'for/dd v w))
                 (if (zero? w)
                     (values dh ewsum)
                     (values (hash-set dh v (+ w (hash-ref dh v 0)))
                             (and ewsum (exact? w) (+ ewsum w)))))))]))
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
;; discretize

(define (dist-discretize/quantile dist n)
  (define who 'dist-discretize/quantile)
  (unless (real-dist? dist)
    (raise-argument-error who "real-dist?" dist))
  (unless (exact-positive-integer? n)
    (raise-argument-error who "exact-positive-integer?" n))
  (define delta (/ (fl n)))
  (define vs (for/list ([i (in-range 0.5 n 1.0)])
               (dist-inv-cdf dist (* i delta))))
  (make-discrete-dist (list->vector vs)))

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
(define (discrete-dist-resample dist n #:mode [mode 'systematic])
  (define who 'discrete-dist-resample)
  (when (zero? (discrete-dist-wsum dist))
    (error who "empty dist"))
  (define r (make-vector n #f))
  (case mode
    [(multinomial #f)
     (for ([i (in-range n)]) (vector-set! r i (random)))
     (-resample! who dist r #f)]
    [(stratified)
     (for ([i (in-range n)]) (vector-set! r i (/ (+ i (random)) n)))
     (-resample! who dist r #t)]
    [(systematic)
     (define delta (random))
     (for ([i (in-range n)]) (vector-set! r i (/ (+ i delta) n)))
     (-resample! who dist r #t)]
    [(residual)
     (match-define (discrete-dist h wsum _) dist)
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
     (define dist* (discrete-dist h* wsum* #f))
     (for ([j (in-range next-index n)])
       (vector-set! r j (dist-sample dist*)))]
    [else (error 'discrete-dist-resample "bad resampling mode: ~e" mode)])
  r)

;; -resample! : Symbol DiscreteDist (Vectorof Real[0,1]) -> Void
(define (-resample! who dist us sorted?)
  (match-define (discrete-dist h wsum _) dist)
  (unless sorted? (vector-sort! us <))
  (define n (vector-length us))
  (let loop ([iter (hash-iterate-first h)] [i 0] [ws 0.0] [wc 0.0])
    (cond [iter
           (define v (hash-iterate-key h iter))
           (define w (fl (hash-iterate-value h iter)))
           (define-values (ws* wc*) (compensated+ w ws wc))
           (let uloop ([i i])
             (cond [(< i n)
                    (define u (* wsum (vector-ref us i)))
                    (cond [(<= u ws*)
                           (vector-set! us i v)
                           (uloop (add1 i))]
                          [else
                           (loop (hash-iterate-next h iter) i ws* wc*)])]
                   [else
                    (void)]))]
          [else
           ;; Should not happen!
           ;; In principle, error; but probably just float issues; restart.
           (loop (hash-iterate-first h) i ws wc)])))

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
      [(discrete-dist h _ _)
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
    (check-equal? (dist-density ed 'a) (dnum #f 1/2 #;0))
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
