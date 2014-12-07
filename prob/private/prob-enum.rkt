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
;; Note: pred and project must be pure; applied outside of prompt
(define (enumerate* thunk limit normalize?)
  (vprintf "Enumerating\n")
  (define tree (reify-tree thunk))
  (define-values (table prob-unexplored prob-accepted)
    (explore tree limit))
  (vprintf "unexplored rate = ~s\n" prob-unexplored)
  (vprintf "accept rate = ~s\n" (/ prob-accepted (- 1 prob-unexplored)))
  (when (zero? (hash-count table))
    (error 'enumerate "condition accepted no paths"))
  (when (and normalize? (zero? prob-accepted))
    (error 'enumerate "probability of accepted paths underflowed to 0"))
  (tabulate table prob-accepted #:normalize? normalize?))

(define (tabulate table prob-accepted #:normalize? [normalize? #t])
  (define entries
    (for/list ([(val prob) (in-hash table)])
      (cons val (if normalize? (/ prob prob-accepted) prob))))
  (make-discrete-dist entries #:normalize? normalize?))

;; ----------------------------------------

;; explore : (EnumTree A) ...
;;        -> hash[A => Prob] Prob Prob
;; Limit means: explore until potential accepted dist is < limit unaccounted for,
;; ie, unexplored < limit * accepted. Thus, limit is nonlocal; use BFS.
(define (explore tree limit)
  (define prob-unexplored 1) ;; prob of all unexplored paths
  (define prob-accepted 0) ;; prob of all accepted paths
  (define table (hash)) ;; hash[A => Prob], probs are not normalized

  ;; h: heap[(cons Prob (-> (EnumTree A)))]
  (define (entry->=? x y)
    (>= (car x) (car y)))
  (define h (heap entry->=?)) ;; "min-heap", but want max prob, so >=

  ;; add : A Prob table prob-unexplored prob-accepted
  ;;    -> (values table prob-unexplored prob-accepted)
  (define (add a p table prob-unexplored prob-accepted)
    ;; (eprintf "- consider add A=~s\n" a)
    (cond [(positive? p)
           (let* ([prob-unexplored (- prob-unexplored p)]
                  [prob-accepted (+ p prob-accepted)]
                  [table (hash-set table a (+ p (hash-ref table a 0)))])
             ;; (eprintf "- add B=~s\n" b)
             (values table prob-unexplored prob-accepted))]
          [else
           (vprintf "WARNING: bad prob ~s for ~s\n" p a)
           (values table prob-unexplored prob-accepted)]))

  ;; traverse-tree : (EnumTree A) Prob ...
  ;;              -> (values h table prob-unexplored prob-accepted)
  (define (traverse-tree et prob-of-tree h table prob-unexplored prob-accepted)
    (match et
      [(only a)
       (let-values ([(table prob-unexplored prob-accepted)
                     (add a prob-of-tree table prob-unexplored prob-accepted)])
         (values h table prob-unexplored prob-accepted))]
      [(? split? et)
       (define subs (split->subtrees et))
       (for/fold ([h h] [table table] [prob-unexplored prob-unexplored] [prob-accepted prob-accepted])
           ([sub (in-list subs)])
         (match sub
           [(cons p lt)
            (let ([p* (* prob-of-tree p)])
              (cond [(positive? p*)
                     (values (heap-insert h (cons p* lt)) table prob-unexplored prob-accepted)]
                    [else
                     (vprintf "WARNING: probability of a path may have underflowed\n")
                     (values h table prob-unexplored prob-accepted)]))]))]
      [(weight dist val k)
       (when (and limit (not (or (finite-dist? dist) (integer-dist? dist))))
         ;; limit applies to mass; it can't handle densities
         (error 'enumerate "cannot use both #:limit and observe-at with continuous distribution"))
       (traverse-tree (k) (* prob-of-tree (dist-pdf dist val)) h table prob-unexplored prob-accepted)]
      [(failed reason)
       (let ([prob-unexplored (- prob-unexplored prob-of-tree)])
         (values h table prob-unexplored prob-accepted))]))

  ;; heap-loop : ... -> ...
  (define (heap-loop h table prob-unexplored prob-accepted)
    (cond [(and limit (< prob-unexplored (* limit prob-accepted)))
           ;; Done!
           (when (positive? (heap-count h))
             (vprintf "stopping with ~s unexplored path(s)\n" (heap-count h)))
           (done h table prob-unexplored prob-accepted)]
          [(zero? (heap-count h))
           ;; explored all paths
           (done h table prob-unexplored prob-accepted)]
          [else
           (let* ([sub (heap-find-min/max h)]
                  [h (heap-delete-min/max h)])
             (let-values ([(h table prob-unexplored prob-accepted)
                           (traverse-tree ((cdr sub)) (car sub)
                                          h table prob-unexplored prob-accepted)])
               (heap-loop h table prob-unexplored prob-accepted)))]))

  (define (done h table prob-unexplored prob-accepted)
    (values table prob-unexplored prob-accepted))

  ;; FIXME: prob-unexplored can drop below zero because of floating-point
  ;; inaccuracy. Any remedy?
  (let-values ([(h table prob-unexplored prob-accepted)
                (traverse-tree tree 1 h table prob-unexplored prob-accepted)])
    (heap-loop h table prob-unexplored prob-accepted)))

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
                (define forced-subs (force-subtrees (split->subtrees tree)))
                (get-samples/paths forced-subs prob)])]
        [(weight dist val k)
         (get-samples/paths (list (cons (dist-pdf dist val) (k))) prob)]
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
