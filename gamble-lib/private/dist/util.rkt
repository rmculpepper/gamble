;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

;; ------------------------------------------------------------

(module math racket/base
  (require (only-in racket/base [exact->inexact inexact] [inexact->exact exact])
           (submod racket/performance-hint begin-encourage-inline)
           "../util/real.rkt")
  (provide (all-defined-out)
           exact
           inexact
           (all-from-out "../util/real.rkt"))

  (begin-encourage-inline
    (define (probability? v)
      (and (real? v) (<= 0 v 1)))
    (define (nonnegative-rational? v)
      (and (rational? v) (>= v 0)))
    (define (positive-rational? v)
      (and (rational? v) (> v 0)))
    (define (ilog x) ;; avoid error on exact 0
      (log (inexact x)))
    (define (convert-p p log? 1-p?)
      (define p* (if 1-p? (- 1 p) p))
      (if log? (log (inexact p*)) p*))
    (define (unconvert-p p log? 1-p?)
      (define p* (if log? (exp p) p))
      (if 1-p? (- 1 p*) p*)))

  ;; Multiply, but short-circuit if first arg evals to 0.
  ;; FIXME: preserve (in)exactness?
  (define-syntax-rule (lazy* a b ...)
    (let ([av a]) (if (zero? av) 0 (* av b ...))))

  (begin))

;; ------------------------------------------------------------

(module weights racket/base
  (require racket/vector)
  (provide (all-defined-out))

  ;; cumulative-vector : (Vectorof Real) -> (ImmVectorof Real)
  (define (cumulative-vector ws)
    (define cws (make-vector (vector-length ws)))
    (for/fold ([s 0]) ([w (in-vector ws)] [i (in-naturals)])
      (let ([s (+ s w)]) (begin (vector-set! cws i s) s)))
    (vector->immutable-vector cws))

  ;; weights-intern-table : WeakHash[Vector => #t]
  (define weights-intern-table (make-weak-hash))

  ;; normalize-weights : Symbol Vector Bool -> (ImmVectorof Rational), sums to 1
  (define (normalize-weights who in-ws fl?)
    (or (hash-ref-key weights-intern-table in-ws #f)
        (let ([ws (normalize-weights* who in-ws)])
          (begin (hash-set! weights-intern-table ws #t) ws))))
  (define (normalize-weights* who in-ws)
    (define ws (vector->immutable-vector in-ws))
    (for ([w (in-vector ws)])
      (unless (and (rational? w) (>= w 0))
        (raise-argument-error who "(vectorof (>=/c 0))" ws)))
    (define wsum (for/sum ([w (in-vector ws)]) w))
    (unless (> wsum 0)
      (error who "weights sum to zero\n  weights: ~e" ws))
    (cond [(= wsum 1) ws]
          [else (vector->immutable-vector
                 (vector-map (lambda (w) (/ w wsum)) ws))]))

  ;; binary-search/least-geq : (Vectorof Real) Real -> Nat
  ;; PRE: cws is sorted increasing, cws[last] >= x
  ;; POST: returns least index k such that cws[k] >= x
  ;; Note: want this variant for sampling from cumulative vector.
  (define (binary-search/least-geq cws x)
    (let loop ([a -1] [b (sub1 (vector-length cws))]) ;; a < b, a invalid, b valid
      (cond [(= (+ a 1) b)
             b]
            [else
             (define m (quotient (+ a b) 2))
             (if (>= (vector-ref cws m) x)
                 (loop a m)
                 (loop m b))])))

  (begin))

;; ------------------------------------------------------------

(module density racket/base
  (require racket/match
           (submod ".." math))
  (provide (all-defined-out))

  ;; ------------------------------------------------------------
  ;; Density dimension (ddim)

  ;; Consider the following probabilistic "model":

  ;;   X ~ Bernoulli(1/2)
  ;;   Y ~ if X then Uniform(-1, 1) else Bernoulli(1/2)
  ;;   observe Y = 0

  ;; What is the posterior on X given the observation on Y?

  ;; Naive analysis:
  ;; If X = 1, then Y is drawn from Unif(-1,1), and the density at 0 is 1/2.
  ;; If X = 0, then Y is drawn from Bern(1/2), and the density at 0 is 1/2.
  ;; So the observation changes nothing; the posterior is the same as the prior,
  ;; so Bernoulli(1/2).

  ;; But that's absurd. Really, the entire model is absurd.
  ;; ------------------------------------------------------------

  ;; Density = (density NNReal Nat #f) | (density ExtReal Nat #t)

  (struct density (d ddim log?) #:transparent
    #:guard (lambda (d ddim log? _name)
              (unless (or (rational? d) (eqv? d -inf.0))
                (raise-argument-error 'density "(or/c rational? -inf.0)" d))
              (unless (exact-nonnegative-integer? ddim)
                (raise-argument-error 'density "exact-nonnegative-integer?" ddim))
              (cond [log?
                     (values (exact->inexact d) ddim #t)]
                    [else
                     (unless (>= d 0)
                       (error 'density "expected nonnegative rational\n  given: ~e" d))
                     (values d ddim #f)])))

  (define (density->number d [log? #f])
    (match-define (density d1 _ log1?) d)
    (cond [(and log? log1?) d1]
          [log? (log d1)]
          [log1? (exp d1)]
          [else d1]))

  (define (density* d1 d2)
    (match* [d1 d2]
      [[(density d1 ddim1 log1?) (density d2 ddim2 log2?)]
       (density (cond [(and log1? log2?) (+ d1 d2)]
                      [log1? (+ d1 (ilog d2))]
                      [log2? (+ (ilog d1) d2)]
                      [else (* d1 d2)])
                (+ ddim1 ddim2)
                (or log1? log2?))]))

  (define (density+ d1 d2)
    (define (bad-ddim)
      (error 'density+
             "cannot add densities with different dimensions\n  given: ~e, ~e"
             d1 d2))
    (match* [d1 d2]
      [[(density d1 ddim1 log1?) (density d2 ddim2 log2?)]
       (unless (= ddim1 ddim2) (bad-ddim))
       (density (cond [(and log1? log2?) (logspace+ d1 d2)]
                      [log1? (logspace+ d1 (ilog d2))]
                      [log2? (logspace+ (ilog d1) d2)]
                      [else (+ d1 d2)])
                ddim1
                (or log1? log2?))]))

  #|
  (define (density-cmp d1 d2)
    (match-define (density n1 ddim1) d1)
    (match-define (density n2 ddim2) d2)
    (cond [(= ddim1 ddim2)
           (cond [(> n1 n2) '>]
                 [(< n1 n2) '<]
                 [else '=])]
          [else #f]))

  ;; density-logratio : Density Density -> Real
  (define (density-logratio d1 d2)
    (match-define (density _ ll1 ddim1) d1)
    (match-define (density _ ll2 ddim2) d2)
    (cond [(< ddim1 ddim2) +inf.0]
          [(> ddim1 ddim2) -inf.0]
          [else (- ll1 ll2)]))

  |#)

;; ------------------------------------------------------------

(module define racket/base
  (require (for-syntax racket/base
                       syntax/parse
                       racket/syntax)
           racket/match)
  (provide define-dist-struct)

  (begin-for-syntax
    (define-syntax-class name-dist-id
      #:attributes (name)
      (pattern nd:id
               #:do [(define nd-s (symbol->string (syntax-e #'nd)))
                     (define m (regexp-match #rx"^(.*)-dist$" nd-s))]
               #:fail-unless m "expected identifier ending in `-dist`"
               #:with name (format-id #'nd "~a" (cadr m))))
    (define-syntax-class param-spec
      (pattern [param:id pred:expr] #:with conv #'begin)
      (pattern [param:id pred:expr conv:expr]))
    (define-splicing-syntax-class maybe-guard
      #:attributes (guard-fun)
      (pattern (~seq #:guard guard-fun))
      (pattern (~seq) #:attr guard-fun #f)))

  (define-syntax define-dist-struct
    (syntax-parser
      [(_ nd:name-dist-id (p:param-spec ...)
          g:maybe-guard
          more ...)
       #'(struct nd (p.param ...)
           #:transparent
           #:guard (lambda (p.param ... _name)
                     (define (bad who)
                       (maker-error 'nd '(p.param ...) '(p.pred ...) who p.param ...))
                     (unless (p.pred p.param) (bad 'p.param)) ...
                     (let ([p.param (p.conv p.param)] ...)
                       (~? (g.guard-fun p.param ...)
                           (values p.param ...))))
           more ...)]))

  (define (maker-error sname fnames fpreds bad-fname . fvalues)
    (for ([fname (in-list fnames)] [fpred (in-list fpreds)] [index (in-naturals)])
      (when (eq? fname bad-fname)
        (apply raise-argument-error sname (format "~s" fpred) index fvalues))))

  (begin))

;; ------------------------------------------------------------
