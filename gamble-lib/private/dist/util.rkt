;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

;; ------------------------------------------------------------

(module math racket/base
  (require (submod racket/performance-hint begin-encourage-inline)
           "../util/real.rkt")
  (provide (all-from-out "../util/real.rkt")
           (all-defined-out))

  (begin-encourage-inline
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
           (submod ".." search))

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

  (begin))
