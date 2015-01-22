;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         "../dist.rkt"
         "prob-lazy-tree.rkt"
         "pairingheap.rkt"
         "interfaces.rkt")
(provide enumerate*
         enum-importance-sampler*)

;; == Overview ==
;;
;; Enumeration using lazy tree of probability-labeled possibilities,
;; based on delimited continuations (only for discrete ERPs).
;;
;; For enumeration to work:
;;
;;  - All paths must either terminate or evaluate infinitely many ERPS.
;;    (and every ERP must generate at least two values with nonzero prob.)
;;
;;  - The predicate must accept a nonempty set of paths.
;;
;; Without knowing structure of paths and conditioning predicate, can't
;; make smart distinctions between incomplete paths.
;;
;; Would be nice if we could tell whether a path was viable or not wrt
;; condition. Seems like it would require drastic changes to model of
;; computation (eg, like symbolic execution) to support non-trivial
;; conditions. Would that be a profitable place to spend effort?

;; ============================================================

;; enumerate* : (-> A) etc -> (Listof (List A Prob))
(define (enumerate* thunk limit normalize? exact?)
  (vprintf "Enumerating\n")
  (define tree (reify-tree thunk))
  (define-values (table prob-explored prob-accepted)
    (explore tree limit (not exact?)))
  (vprintf "explored rate = ~s\n" prob-explored)
  (vprintf "accept rate = ~s\n" (/ prob-accepted prob-explored))
  (when (zero? (hash-count table))
    (error 'enumerate "condition accepted no paths"))
  (when (and normalize? (zero? prob-accepted))
    (error 'enumerate "probability of accepted paths underflowed to 0"))
  (tabulate table prob-accepted (not exact?) #:normalize? normalize?))

(define (tabulate table prob-accepted logspace? #:normalize? [normalize? #t])
  (define (p->real p)
    (if normalize?
        (if logspace?
            (exp (- p prob-accepted))
            (/ p prob-accepted))
        (if logspace?
            (exp p)
            p)))
  (define prob-accepted* (if logspace? (exp prob-accepted) prob-accepted))
  (define entries
    (for/list ([(val prob) (in-hash table)])
      (cons val (p->real prob))))
  (make-discrete-dist entries #:normalize? normalize?))

;; ----------------------------------------

;; https://hips.seas.harvard.edu/blog/2013/01/09/computing-log-sum-exp/
(provide logspace+
         logspace-)

;; logspace+ : Real Real -> Real
;; Given (log A) and (log B), return (log (+ A B)).
(define logspace+*
  (case-lambda
    [() -inf.0]
    [(a) a]
    [(a b)
     (define m (max a b))
     (if (= m -inf.0)
         -inf.0
         (+ m (log (+ (exp (- a m)) (exp (- b m))))))]
    [as
     (define m (apply max as))
     (if (= m -inf.0)
         -inf.0
         (+ m (log (for/sum ([a (in-list as)]) (exp (- a m))))))]))

(define (logspace+ a b)
  (define r (logspace+* a b))
  (when (eqv? r +nan.0)
    (error 'logspace+ "~s + ~s => ~s" a b r))
  r)

(define (logspace-* a b)
  (define m (max a b))
  (+ m (log (- (exp (- a m)) (exp (- b m))))))

(define (logspace- a b)
  (define r (logspace-* a b))
  (when (eqv? r +nan.0)
    (error 'logspace- "~s - ~s => ~s" a b r))
  r)

;; explore : (EnumTree A) ...
;;        -> hash[A => Prob] Prob Prob
;; Limit means: explore until potential accepted dist is < limit unaccounted for,
;; ie, unexplored < limit * accepted. Thus, limit is nonlocal; use BFS.
(define (explore tree limit logspace?)
  ;; The representation of probabilities in this code is determined by
  ;; the logspace? argument.  If logspace? is true, then values
  ;; represent log of prob; if false, values represent prob directly.
  (define (->p a) ;; inexact to avoid error on exact 0
    (when (negative? a) (error '->p "negative: ~e" a))
    (if logspace? (log (exact->inexact a)) a))
  (define (p->real a)
    (if logspace? (exp a) a))
  (define (p+ a b)
    (if logspace? (logspace+ a b) (+ a b)))
  (define (p* a b)
    (if logspace? (+ a b) (* a b)))
  (define pzero (->p 0))

  (define (entry->=? x y)
    (>= (car x) (car y)))

  ;; prob-explored -- prob of all explored paths
  ;; prob-accepted -- prob of all accepted paths
  ;; table         -- hash[A => Prob], probs are not normalized
  ;; h             -- heap[(cons Prob (-> (EnumTree A)))]

  ;; add : A Prob table prob-explored prob-accepted
  ;;    -> (values table prob-explored prob-accepted)
  (define (add a p table prob-explored prob-accepted)
    ;; (eprintf "- consider add A=~s\n" a)
    (cond [(> p pzero) ;; ie, possible
           (let* ([prob-explored (p+ prob-explored p)]
                  [prob-accepted (p+ p prob-accepted)]
                  [table (hash-set table a (p+ p (hash-ref table a pzero)))])
             ;; (eprintf "- add B=~s\n" b)
             (values table prob-explored prob-accepted))]
          [else
           (vprintf "WARNING: bad prob ~s for ~s\n" (p->real p) a)
           (values table prob-explored prob-accepted)]))

  ;; traverse-tree : (EnumTree A) Prob ...
  ;;              -> (values h table prob-explored prob-accepted)
  (define (traverse-tree et prob-of-tree h table prob-explored prob-accepted)
    (match et
      [(only a)
       (let-values ([(table prob-explored prob-accepted)
                     (add a prob-of-tree table prob-explored prob-accepted)])
         (values h table prob-explored prob-accepted))]
      [(? split? et)
       (define subs (split->subtrees et logspace?))
       (for/fold ([h h] [table table] [prob-explored prob-explored] [prob-accepted prob-accepted])
           ([sub (in-list subs)])
         (match sub
           [(cons p lt)
            (let ([p* (p* prob-of-tree p)])
              (cond [(> p* pzero) ;; ie, possible
                     (values (heap-insert h (cons p* lt)) table prob-explored prob-accepted)]
                    [else
                     (vprintf "WARNING: probability of a path may have underflowed: ~s\n" p*)
                     (values h table prob-explored prob-accepted)]))]))]
      [(weight dist val scale k)
       (when (and limit (not (or (finite-dist? dist) (integer-dist? dist))))
         ;; limit applies to mass; it can't handle densities
         (error 'enumerate "cannot use both #:limit and observe with continuous distribution"))
       (traverse-tree (k)
                      (p* prob-of-tree (p* (->p scale) (dist-pdf dist val logspace?)))
                      h table prob-explored prob-accepted)]
      [(failed reason)
       (let ([prob-explored (p+ prob-explored prob-of-tree)])
         (values h table prob-explored prob-accepted))]))

  ;; heap-loop : ... -> ...
  (define (heap-loop h table prob-explored prob-accepted)
    (cond [;;     unexplored < limit * accepted
           ;; ie, (1 - explored) < limit * accepted
           ;; ie, 1 < limit * accepted + explored
           ;; ie, limit * accepted + explored > 1
           (and limit (> (p+ prob-explored (p* (->p limit) prob-accepted)) (->p 1)))
           ;; Done!
           (when (positive? (heap-count h))
             (vprintf "stopping with ~s unexplored path(s)\n" (heap-count h)))
           (done h table prob-explored prob-accepted)]
          [(zero? (heap-count h))
           ;; explored all paths
           (done h table prob-explored prob-accepted)]
          [else
           (let* ([sub (heap-find-min/max h)]
                  [h (heap-delete-min/max h)])
             (let-values ([(h table prob-explored prob-accepted)
                           (traverse-tree ((cdr sub)) (car sub)
                                          h table prob-explored prob-accepted)])
               (heap-loop h table prob-explored prob-accepted)))]))

  (define (done h table prob-explored prob-accepted)
    (values table (p->real prob-explored) (p->real prob-accepted)))

  (let ()
    (define prob-explored pzero)
    (define prob-accepted pzero)
    (define table (hash))
    (define h (heap entry->=?)) ;; "min-heap", but want max prob, so >=
    ;; FIXME: prob-explored can rise above 1 because of floating-point
    ;; inaccuracy. (Maybe?) Any remedy?
    (let-values ([(h table prob-explored prob-accepted)
                  (traverse-tree tree (->p 1) h table prob-explored prob-accepted)])
      (heap-loop h table prob-explored prob-accepted))))

;; ============================================================

;; enum-importance-sampler* : (-> A) -> (Cons A Positive-Real)
;; FIXME: can get stuck on infinitely deep path (eg, geometric)
(define (enum-importance-sampler* thunk)
  (new enum-importance-sampler%
       (tree (reify-tree thunk))))

(define enum-importance-sampler%
  (class* object% (weighted-sampler<%>)
    (init-field tree)
    (super-new)

    ;; cache : (listof (Cons A Positive-Real))
    (define cache null)

    (define/public (info)
      (printf "== Enum-importance sampler\n")
      (printf "No information available.\n"))

    ;; get-one-sample : -> (Cons A Prob)
    (define/public (sample/weight)
      (cond [(pair? cache)
             (begin0 (car cache)
               (set! cache (cdr cache)))]
            [else
             (set! cache (get-samples tree 1.0))
             (sample/weight)]))

    ;; get-samples : (EnumTree A) Prob -> (listof (Cons A Positive-Real))
    (define/private (get-samples tree prob)
      (match tree
        [(only a)
         (list (cons a prob))]
        [(split label dist k _)
         (cond [(eq? (dist-enum dist) #f)
                ;; Just sample.
                ;; FIXME: generate multiple samples, "while we're here"???
                (define forced-subs (list (cons 1 (k (dist-sample dist)))))
                (get-samples/paths forced-subs prob)]
               [else
                (define forced-subs (force-subtrees (split->subtrees tree #f)))
                (get-samples/paths forced-subs prob)])]
        [(weight dist val scale k)
         (get-samples/paths (list (cons (* scale (dist-pdf dist val)) (k))) prob)]
        [(failed _)
         null]))

    (define/private (force-subtrees subs)
      (for/list ([sub (in-list subs)])
        (match sub
          [(cons sub-prob sub-thunk)
           (cons sub-prob (sub-thunk))])))

    ;; get-samples/paths : (Listof (Cons Real (EnumTree A))) -> (Listof (Cons A Positive-Real))
    (define/private (get-samples/paths forced-subs prob)
      (define successes (filter (lambda (s) (only? (cdr s))) forced-subs))
      (define failures (filter (lambda (s) (failed? (cdr s))) forced-subs))
      (define unknowns
        (filter (lambda (s) (or (split? (cdr s)) (weight? (cdr s)))) forced-subs))
      (append (for/list ([success (in-list successes)])
                (match success
                  [(cons sub-prob (only value))
                   (cons value (* prob sub-prob))]))
              (if (null? unknowns)
                  null
                  (let* ([unknown-probs (map car unknowns)]
                         [unknown-prob-total (apply + unknown-probs)]
                         [index (discrete-sample unknown-probs unknown-prob-total)]
                         [unknown (list-ref unknowns index)])
                    (match unknown
                      [(cons sub-prob sub-tree)
                       (get-samples sub-tree (* prob (/ sub-prob unknown-prob-total)))])))))

    ;; discrete-sample : (listof Positive-Real) Positive-Real -> Nat
    (define/private (discrete-sample weights weight-total)
      (let loop ([weights weights] [p (* (random) weight-total)] [i 0])
        (cond [(null? weights)
               (error 'importance-sampler "internal error: out of weights")]
              [(< p (car weights))
               i]
              [else (loop (cdr weights) (- p (car weights)) (add1 i))])))
    ))

#|
Maybe new abstraction, alternative to sampler: enumerator. Produces
stream (finite or infinite) of weighted partitions of
distribution. (Note: values may may repeat, just add probs.) Can't
normalize unless stream is finite or we truncate it.
|#
