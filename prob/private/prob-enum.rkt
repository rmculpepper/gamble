#lang racket/base
(require racket/match
         data/heap
         data/order
         racket/control
         unstable/markparam
         "prob-hooks.rkt"
         "util.rkt")
(provide enumerate*
         enum-ERP
         enum-mem)

#|
Enumeration based on delimited continuations (only for discrete ERPs)

For enumeration to work:

 - All paths must either terminate or evaluate infinitely many ERPS.
   (and every ERP must generate at least two values with nonzero prob.)

 - The predicate must accept a nonempty set of paths.

Without knowing structure of paths and conditioning predicate, can't
make smart distinctions between incomplete paths.

Would be nice if we could tell whether a path was viable or not wrt
condition. Seems like it would require drastic changes to model of
computation (eg, like symbolic execution) to support non-trivial
conditions. Would that be a profitable place to spend effort?
|#

#|
For mem, have a global store that gets rewound after each ERP choice
exploration.

Use markparam to store current global store. (Racket parameters don't
work well with delimited continuations.) Since prompts also delimit
CCM, need a separate prompt tag (sigh... it's complicated).

What should it mean for a memoized function to escape the enumeration
in which it was created? (The current implementation doesn't do
anything reasonable, probably.)
|#


#|

How to make enumeration nest?

(enum ;; outer
 ...
 (enum ;; inner
  ...))

- Straightforward except for mem:

  - An outer-created memoized function that is invoked in the inner
    enum should fork its possibilities to the *outer* prompt.
  - Except... what if the outer-mem-fun calls its argument, which is an
    inner-mem-fun? Then that "should" fork its possibilities to inner
    prompt.
  - Bleh, mem probably only makes sense on first-order functions.
  - Alternatively, in that case we say the inner-mem-fun has escaped
    its context, error. (In general, mem-fun that escapes its context
    is problematical, except for direct-style mem.)
  - What if outer-mem-fun is (lambda (n) (lambda () (flip (/ n))))?
    Then if applied, gets thunk, then applied in inner, inner explores
    branches. That seems reasonable.

  - Anyway... when an outer-mem-fun is invoked, it needs to restore
    the outer ERP (and mem) impls.
    - That means nested enum can't use parameterize ... :/
    - A memoized function must close over the activation support (ctag,
      markparam) for the mem that created it.
  - Each enumeration activation needs a separate prompt tag and
    memo-table key.
  - explore must be rewritten in pure code: find functional priority
    queue (PFDS from planet?), use immutable hash, etc
|#

;; BUG/LIMITATION:
;; - delimited continuations captured by ERP *must not* use include
;;   uses of parameterize, because Racket's parameterize is not correct
;;   wrt delimited control (this is a known Racket WONTFIX bug)
;;   (ok to use parameterize, just not in captured part of continuation)
;;   FIXME: possible to detect and issue error??

;; Use default prompt tag for mark-parameter, ctag for delimited
;; continuations.
(define ctag (make-continuation-prompt-tag))

;; A (EnumTree A) is one of
;; - (only A)
;; - (split (listof (cons Prob (-> (EnumTree A)))))
(struct only (answer))
(struct split (subs))

;; current-global-memo-table : (parameterof (boxof (hash[list => result])))
(define current-global-memo-table (mark-parameter))

;; enumerate* : (-> A) (A -> Boolean) (A -> B) etc -> (Listof (List B Prob))
;; Note: pred and project must be pure; applied outside of prompt
(define (enumerate* thunk pred [project values]
                    #:limit [limit 1e-6]
                    #:normalize? [normalize? #t])
  (define-values (table prob-unexplored prob-accepted)
    (explore (let ([memo-table
                    (cond [(current-global-memo-table)
                           ;; means recursive enumerate ...
                           => unbox]
                          [else (hash)])])
               (call-with-enum-context memo-table (lambda () (only (thunk)))))
             pred project limit))
  (when (verbose?)
    (eprintf "enumerate: unexplored rate: ~s\n" prob-unexplored)
    (eprintf "enumerate: accept rate: ~s\n"
             (/ prob-accepted (- 1 prob-unexplored))))
  (when (zero? (hash-count table))
    (error 'enumerate "condition accepted no paths"))
  (when (and normalize? (zero? prob-accepted))
    (error 'enumerate "probability of accepted paths underflowed to 0"))
  (tabulate table prob-accepted #:normalize? normalize?))

;; explore : (EnumTree A) (A -> Boolean) (A -> B) Prob
;;        -> hash[B => Prob] Prob Prob
;; Limit means: explore until potential accepted dist is < limit unaccounted for,
;; ie, unexplored < limit * accepted. Thus, limit is nonlocal; use BFS.
(define (explore tree pred project limit)
  (define prob-unexplored 1) ;; prob of all unexplored paths
  (define prob-accepted 0) ;; prob of all accepted paths
  (define table (make-hash)) ;; hash[B => Prob], probs are not normalized
  (define seen '()) ;; (listof B), reversed; FIXME currently not used
  ;; add! : A Prob -> void
  (define (add! a p)
    (cond [(positive? p)
           (set! prob-unexplored (- prob-unexplored p))
           (when (pred a)
             (let ([b (project a)])
               (unless (hash-has-key? table b)
                 (set! seen (cons b seen)))
               (set! prob-accepted (+ p prob-accepted))
               (hash-set! table b (+ p (hash-ref table b 0)))))]
          [else
           (when #t ;; (verbose?)
             (eprintf "WARNING: bad prob ~s for ~s\n" p (project a)))]))

  ;; h: heap[(cons Prob (-> (EnumTree A)))]
  (define (entry->=? x y)
    (>= (car x) (car y)))
  (define h (make-heap entry->=?)) ;; Racket has "min-heaps", but want max prob, so >=

  (define (traverse-tree et prob-of-tree)
    (match et
      [(only a)
       (add! a prob-of-tree)]
      [(split subs)
       (for ([sub (in-list subs)])
         (match sub
           [(cons p lt)
            (let ([p* (* prob-of-tree p)])
              (cond [(positive? p)
                     (heap-add! h (cons p* lt))]
                    [else
                     (when (verbose?)
                       (eprintf "WARNING: bad prob ~s in path\n" p*))]))]))]))

  ;; FIXME: detect path prob underflow to 0.
  ;; FIXME: prob-unexplored can drop below zero because of floating-point
  ;; inaccuracy. Any remedy?
  (traverse-tree tree 1)
  (let loop ()
    (cond [(and limit (< prob-unexplored (* limit prob-accepted)))
           ;; Done!
           (when (positive? (heap-count h))
             (when (verbose?)
               (eprintf "stopping with ~s unexplored path(s)\n" (heap-count h))))
           (void)]
          [(zero? (heap-count h))
           ;; explored all paths
           (void)]
          [else
           (define sub (heap-min h)) ;; actually max prob
           (heap-remove-min! h)
           ;; (eprintf "** prob-unexplored = ~s; prob-accepted = ~s\n" prob-unexplored prob-accepted)
           ;; (eprintf "** picked ~s\n" sub)
           (traverse-tree ((cdr sub)) (car sub))
           (loop)]))

  (values table prob-unexplored prob-accepted))

(define (tabulate table prob-accepted #:normalize? [normalize? #t])
  (define entries
    (for/list ([(val prob) (in-hash table)])
      (list val (if normalize? (/ prob prob-accepted) prob))))
  (sort entries (order-<? datum-order)))

;; ----

(define (enum-ERP tag dist)
  (unless (memq (car tag) '(flip discrete binomial geometric poisson continue-special))
    (error 'ERP "cannot enumerate non-discrete distribution: ~s" tag))
  (define memo-table (unbox (current-global-memo-table)))
  (define-values (vals probs tail-prob+tag)
    (dist->vals+probs tag dist))
  (call-with-composable-continuation
   (lambda (k)
     (abort-current-continuation
      ctag
      (lambda ()
        (split
         (cons/f (and tail-prob+tag
                      (let ([tail-prob (car tail-prob+tag)]
                            [tail-tag (cdr tail-prob+tag)])
                        (cons tail-prob
                              (lambda ()
                                (call-with-enum-context memo-table
                                  (lambda () (k (enum-ERP tail-tag dist))))))))
                 (for/list ([val (in-list vals)]
                            [prob (in-list probs)])
                   (cons prob
                         (lambda ()
                           (call-with-enum-context memo-table (lambda () (k val)))))))))))
   ctag))

(define (dist->vals+probs tag dist)
  (let ([enum (dist-enum dist)])
    (cond [(eq? enum 'lazy)
           (lazy-dist->vals+probs tag dist 0)]
          [(eq? enum #f)
           (error 'ERP "cannot enumerate non-integer distribution: ~s" tag)]
          [else ;; positive integer
           (define-values (vals probs)
             (for/lists (vals probs) ([i (in-range enum)])
               (values i (dist-pdf dist i))))
           (values vals probs #f)])))

(define TAKE 10)

;; FIXME: for geometric, can use memoryless feature; just generate
;; same sequence each time (no need to scale/unscale)

(define (lazy-dist->vals+probs tag dist start)
  (match tag
    [(list* 'continue-special _ (and base-tag (list* 'geometric _)))
     ;; Special case: for geometric, no need to resume
     (lazy-dist->vals+probs base-tag dist 0)]
    [(list* 'continue-special start base-tag)
     (lazy-dist->vals+probs base-tag dist start)]
    [_
     (define scale (- 1 (dist-cdf dist (sub1 start))))
     (define-values (vals probs)
       (for/lists (vals probs) ([i (in-range start (+ start TAKE))])
         (values i (/ (dist-pdf dist i) scale))))
     (values vals probs
             (cons (/ (- 1 (dist-cdf dist (+ start TAKE -1))) scale)
                   `(continue-special ,(+ start TAKE) . ,tag)))]))

(define (cons/f x xs) (if x (cons x xs) xs))

;; calls thunk with memo-table in fresh box, parameters set up
(define (call-with-enum-context memo-table thunk)
  (parameterize ((current-ERP enum-ERP)
                 (current-mem enum-mem))
    (mark-parameterize ((current-global-memo-table (box memo-table)))
      (call-with-continuation-prompt
       thunk
       ctag))))

;; ----

;; use global-memo-table to effects can be unwound
(define (enum-mem f)
  (lambda args
    (let ([b (current-global-memo-table)]
          [key (cons f args)])
      (cond [(hash-has-key? (unbox b) key)
             (hash-ref (unbox b) key)]
            [else
             (let ([v (apply f args)])
               ;; NOTE: outer b might be stale, if f called ERP!
               (let ([b (current-global-memo-table)])
                 (set-box! b (hash-set (unbox b) key v))
                 v))]))))

#|
TODO: reify-reflect

For example, flip-based defn of geometric dist is more precise than
flonum-based, but would like to avoid running whole series of flips
whenever geometric is used. Better to have lazy geom tree,
explore (memoized) when needed.

Maybe new abstraction, alternative to sampler: enumerator. Produces
stream (finite or infinite) of weighted partitions of
distribution. (Note: values may may repeat, just add probs.) Can't
normalize unless stream is finite or we truncate it.
|#
