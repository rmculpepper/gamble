;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

;; ------------------------------------------------------------

(module math racket/base
  (require (only-in racket/base
                    [real->double-flonum fl]
                    [exact->inexact inexact] [inexact->exact exact])
           (submod racket/performance-hint begin-encourage-inline)
           "../util/real.rkt")
  (provide (all-defined-out)
           fl exact inexact
           (all-from-out "../util/real.rkt"))

  (begin-encourage-inline
    (define (probability? v)
      (and (real? v) (<= 0 v 1)))
    (define (nontrivial-probability? v)
      (and (real? v) (< 0 v 1)))
    (define (nonnegative-rational? v)
      (and (rational? v) (>= v 0)))
    (define (positive-rational? v)
      (and (rational? v) (> v 0)))
    (define (ilog x) ;; avoid error on exact 0
      (log (fl x)))
    (define (convert-p p log? 1-p?)
      (define p* (if 1-p? (- 1 p) p))
      (if log? (log (fl p*)) p*))
    (define (unconvert-p p log? 1-p?)
      (define p* (if log? (exp p) p))
      (if 1-p? (- 1 p*) p*))
    (define (impossible log?)
      (if log? -inf.0 0.0)))

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

  ;; weights-intern-table : WeakHash[ImmVector => #t]
  (define weights-intern-table (make-weak-hash))

  ;; normalize-inexact-weights : Symbol Vector Bool -> (ImmVectorof Flonum), sums to 1
  (define (normalize-inexact-weights who in-ws)
    (or (hash-ref-key weights-intern-table in-ws #f)
        (let ([ws (normalize-inexact-weights* who in-ws)])
          (begin0 ws (hash-set! weights-intern-table ws #t)))))
  (define (normalize-inexact-weights* who in-ws)
    (define ws (vector->immutable-vector in-ws))
    (define-values (wsum any-exact?)
      (for/fold ([s 0] [any-exact? #f]) ([w (in-vector ws)])
        (unless (and (rational? w) (>= w 0))
          (raise-argument-error who "(vectorof (>=/c 0))" ws))
        (values (+ s w) (or any-exact? (exact? w)))))
    (cond [(zero? wsum)
           (error who "weights sum to zero\n  weights: ~e" ws)]
          [(and (= wsum 1.0) (not any-exact?))
           ws]
          [else (vector->immutable-vector
                 (vector-map (lambda (w) (/ (exact->inexact w) wsum)) ws))]))

  (begin))

(module search racket/base
  (provide (all-defined-out))

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

  ;; find-least-natural : Nat (Nat -> Boolean) -> Nat
  ;; PRE: if (ok? m) and n > m, then (ok? n)
  (define (find-least-natural ok?)
    (define (find-ok last-bad)
      (define next (+ last-bad last-bad))
      (cond [(ok? next) (find-least-ok last-bad next)]
            [else (find-ok next)]))
    (define (find-least-ok a b) ;; <a is bad; b is ok
      (cond [(= a b) a]
            [else (let ([m (quotient (+ a b) 2)])
                    (if (ok? m) (find-least-ok a m) (find-least-ok (add1 m) b)))]))
    (if (ok? 0) 0 (find-ok 1)))

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
                     (values (fl d) ddim #t)]
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
    (define-syntax-class param-spec
      (pattern [param:id pred:expr] #:with conv #'begin)
      (pattern [param:id pred:expr conv:expr]))
    (define-splicing-syntax-class maybe-extension
      (pattern (~seq #:extension ~! (f:id ...)))
      (pattern (~seq) #:with (f ...) #'()))
    (define-splicing-syntax-class maybe-guard
      #:attributes (guard-fun)
      (pattern (~seq #:guard guard-fun))
      (pattern (~seq) #:attr guard-fun #f)))

  (define-syntax define-dist-struct
    (syntax-parser
      [(_ nd:id (p:param-spec ...)
          ext:maybe-extension
          g:maybe-guard
          more ...)
       #'(struct nd (p.param ... [ext.f #:auto #:mutable] ...)
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

;; ============================================================

(module+ test
  (require rackunit
           (submod ".." math)
           (submod ".." weights)
           (submod ".." search)
           (submod ".." density))

  ;; --------------------
  ;; math

  (for ([v '(0 1 0.0 1.0 0.1 0.9 1/2)])
    (check-true (probability? v)))
  (for ([v '(-1 2 1.1 3/2 -inf.0 +inf.0 +nan.0 apple "string" (a b c))])
    (check-false (probability? v)))

  (check-equal? (ilog 0) -inf.0)
  (check-equal? (ilog 1) 0.0)

  (check-equal? (convert-p 2/3 #f #f) 2/3)
  (check-equal? (convert-p 2/3 #f #t) 1/3)
  (check-equal? (convert-p 2/3 #t #f) (log 2/3))

  ;; --------------------
  ;; weights

  (check-equal? (cumulative-vector (vector 1 2 3))
                '#(1 3 6))
  (check-equal? (normalize-inexact-weights 'who (vector 1/6 1/3 0.5))
                '#(#i1/6 #i1/3 #i1/2))
  (check-equal? (normalize-inexact-weights 'who (vector 1 2 3))
                '#(#i1/6 #i1/3 #i1/2))

  (let ([ns '#(1 2 2 2 3 4)])
    (check-equal? (binary-search/least-geq ns 0) 0)
    (check-equal? (binary-search/least-geq ns 1) 0)
    (check-equal? (binary-search/least-geq ns 2) 1)
    (check-equal? (binary-search/least-geq ns 3) 4))

  ;; --------------------
  ;; search

  (check-equal? (find-least-natural (lambda (k) (> k 50))) 51)

  ;; --------------------
  ;; density

  (begin))
