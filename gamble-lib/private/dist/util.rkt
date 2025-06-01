;; Copyright 2020-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

;; ------------------------------------------------------------

(module math racket/base
  (require (only-in racket/base [exact->inexact inexact] [inexact->exact exact])
           (submod racket/performance-hint begin-encourage-inline))
  (provide (all-defined-out)
           exact
           inexact)

  (begin-encourage-inline
    (define (probability? v)
      (and (real? v) (<= 0 v 1)))
    (define (nonnegative-rational? v)
      (and (rational? v) (>= v 0)))
    (define (positive-rational? v)
      (and (rational? v) (> v 0)))
    (define (convert-p p log? 1-p?)
      (define p* (if 1-p? (- 1 p) p))
      (if log? (log (exact->inexact p*)) p*))
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
  (require racket/match)
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

  ;; Density = NNReal | (lebesgue-density NNReal Nat)

  (define (density? v)
    (or (and (rational? v) (>= v 0))
        (lebesgue-density? v)))

  (define (density p ddim)
    (unless (and (rational? p) (>= p 0))
      (raise-argument-error 'density "(>=/c 0)" p))
    (unless (exact-positive-integer? ddim)
      (raise-argument-error 'density "exact-positive-integer?" ddim))
    (cond [(zero? ddim) p]
          [(and (zero? p) (= ddim 1)) zero-lebesgue-density]
          [else (lebesgue-density p ddim)]))

  (struct lebesgue-density (p ddim) #:transparent
    #:guard (lambda (d ddim _name)
              (unless (and (rational? d) (>= d 0))
                (raise-argument-error 'lebesgue-density "(>=/c 0)" d))
              (unless (exact-positive-integer? ddim)
                (raise-argument-error 'lebesgue-density "exact-positive-integer?" ddim))
              (values d ddim)))

  (define zero-mass-density 0)
  (define zero-lebesgue-density (lebesgue-density 0 1))

  (define (density* d1 d2)
    (match* [d1 d2]
      [[(? rational? p1) (? rational? p2)]
       (* p1 p2)]
      [[(? rational? p1) (lebesgue-density p2 ddim2)]
       (lebesgue-density (* p1 p2) ddim2)]
      [[(lebesgue-density p1 ddim1) (? rational? p2)]
       (lebesgue-density (* p1 p2) ddim1)]
      [[(lebesgue-density p1 ddim1) (lebesgue-density p2 ddim2)]
       (lebesgue-density (* p1 p2) (+ ddim1 ddim2))]
      [[_ _]
       (unless (density? d1) (raise-argument-error 'density* "density?" d1))
       (unless (density? d2) (raise-argument-error 'density* "density?" d2))]))

  (define (density-product ds)
    (unless (and (list? ds) (andmap density? ds))
      (raise-argument-error 'density-product "(listof density?)" ds))
    (foldl density* 1 ds))

  (define (density+ d1 d2)
    (define (bad)
      (error 'density+ "cannot add incompatible densities\n  given: ~e, ~e" d1 d2))
    (match* [d1 d2]
      [[(? rational? p1) (? rational? p2)]
       (+ p1 p2)]
      [[(lebesgue-density p1 ddim1) (lebesgue-density p2 ddim2)]
       (unless (= ddim1 ddim2) (bad))
       (lebesgue-density (+ p1 p2) ddim1)]
      [[_ _]
       (unless (density? d1) (raise-argument-error 'density* "density?" d1))
       (unless (density? d2) (raise-argument-error 'density* "density?" d2))
       (bad)]))

  (define (density-sum ds)
    (unless (and (pair? ds) (list? ds) (andmap density? ds))
      (raise-argument-error 'density-sum "(nonempty-listof density?)" ds))
    (foldl density+ (car ds) (cdr ds)))

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

  (define (ilog x) (log (exact->inexact x))) ;; avoid error on exact 0
  |#)
