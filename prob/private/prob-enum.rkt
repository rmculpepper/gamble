;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         data/order
         racket/control
         unstable/markparam
         "prob-hooks.rkt"
         "pairingheap.rkt"
         "util.rkt")
(provide enumerate*
         importance-sampler*)

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
For mem, have an activation-specific store that gets rewound after
each ERP choice exploration.

Use markparam to store the memotable. (Racket parameters don't
work well with delimited continuations.) Since prompts also delimit
CCM, need a separate prompt tag (sigh... it's complicated).

What should it mean for a memoized function to escape the enumeration
in which it was created? Should probably be forbidden.
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
      ??? Doesn't work without parameterize ... investigate?
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

;; Each enumeration activation has a prompt-tag and memo-table key.
;; The current prompt-tag is stored by ctag-key.

(define ctag-table (make-weak-hasheq))
(define (ctag-name ctag)
  (hash-ref! ctag-table ctag (lambda () (gensym 'CTAG_))))

;; CCM(activation-key) : (list* PromptTag MarkParameter (Listof Condition))
(define activation-key (mark-parameter))

;; CCM(memo-key) : (Boxof (Hash[ArgList => Result]))

;; A (EnumTree A) is one of
;; - (only A)
;; - (split (listof (cons Prob (-> (EnumTree A)))))
;; - (failed Any)
(struct only (answer))
(struct split (subs))
(struct failed (reason))


;; enumerate* : (-> A) etc -> (Listof (List A Prob))
;; Note: pred and project must be pure; applied outside of prompt
(define (enumerate* thunk spconds
                    #:limit [limit 1e-6]
                    #:normalize? [normalize? #t])
  (define tree (reify-tree thunk spconds))
  (define-values (table prob-unexplored prob-accepted)
    (explore tree limit))
  (when (verbose?)
    (eprintf "enumerate: unexplored rate: ~s\n" prob-unexplored)
    (eprintf "enumerate: accept rate: ~s\n"
             (/ prob-accepted (- 1 prob-unexplored))))
  (when (zero? (hash-count table))
    (error 'enumerate "condition accepted no paths"))
  (when (and normalize? (zero? prob-accepted))
    (error 'enumerate "probability of accepted paths underflowed to 0"))
  (tabulate table prob-accepted #:normalize? normalize?))


;; importance-sampler* : (-> A) -> (List A Positive-Real)
;; FIXME: can get stuck on infinitely deep path (eg, geometric)
(define (importance-sampler* thunk spconds)
  (define tree (reify-tree thunk spconds))
  ;; cache : (listof (List A Positive-Real))
  (define cache null)
  ;; get-samples : (EnumTree A) Prob -> (listof (List A Positive-Real))
  (define (get-samples tree prob)
    (match tree
      [(only a)
       (list (list a prob))]
      [(split subs)
       (define forced-subs 
         (for/list ([sub (in-list subs)])
           (match sub
             [(cons sub-prob sub-thunk)
              (cons sub-prob (sub-thunk))])))
       (define successes (filter (lambda (s) (only? (cdr s))) forced-subs))
       (define failures (filter (lambda (s) (failed? (cdr s))) forced-subs))
       (define unknowns (filter (lambda (s) (split? (cdr s))) forced-subs))
       (append (for/list ([success (in-list successes)])
                 (match success
                   [(cons sub-prob (only value))
                    (list value (* prob sub-prob))]))
               (if (null? unknowns)
                   null
                   (let* ([unknown-probs (map car unknowns)]
                          [unknown-prob-total (apply + unknown-probs)]
                          [index (discrete-sample unknown-probs unknown-prob-total)]
                          [unknown (list-ref unknowns index)])
                     (match unknown
                       [(cons sub-prob sub-tree)
                        (get-samples sub-tree (* prob (/ sub-prob unknown-prob-total)))]))))]
      [(failed _)
       null]))
  ;; discrete-sample : (listof Positive-Real) Positive-Real -> Nat
  (define (discrete-sample weights weight-total)
    (let loop ([weights weights] [p (* (random) weight-total)] [i 0])
      (cond [(null? weights)
             (error 'importance-sampler "internal error: out of weights")]
            [(< p (car weights))
             i]
            [else (loop (cdr weights) (- p (car weights)) (add1 i))])))
  ;; get-one-sample : -> (List A Prob)
  (define (get-one-sample)
    (cond [(pair? cache)
           (begin0 (car cache)
             (set! cache (cdr cache)))]
          [else
           (set! cache (get-samples tree 1.0))
           (get-one-sample)]))
  get-one-sample)

;; ----------------------------------------

;; reify-tree : (-> A) (Listof Condition) -> (EnumTree A)
(define (reify-tree thunk spconds)
  (define memo-key (mark-parameter))
  (define memo-table (hash))
  (define ctag (make-continuation-prompt-tag))
  (call-with-enum-context ctag memo-key memo-table spconds (lambda () (only (thunk)))))


;; calls thunk with memo-table in fresh box, parameters set up
(define (call-with-enum-context ctag memo-key memo-table spconds thunk)
  (parameterize ((current-ERP enum-ERP)
                 (current-mem enum-mem)
                 (current-fail enum-fail))
    (mark-parameterize ((memo-key (box memo-table))
                        (activation-key (list* ctag memo-key spconds)))
      (call-with-continuation-prompt thunk ctag))))

;; ----

;; explore : (EnumTree A)
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
           (when #t ;; (verbose?)
             (eprintf "WARNING: bad prob ~s for ~s\n" p a))
           (values table prob-unexplored prob-accepted)]))

  ;; traverse-tree : (EnumTree A) Prob ... -> (values h table prob-unexplored prob-accepted)
  (define (traverse-tree et prob-of-tree h table prob-unexplored prob-accepted)
    (match et
      [(only a)
       (let-values ([(table prob-unexplored prob-accepted)
                     (add a prob-of-tree table prob-unexplored prob-accepted)])
         (values h table prob-unexplored prob-accepted))]
      [(split subs)
       (for/fold ([h h] [table table] [prob-unexplored prob-unexplored] [prob-accepted prob-accepted])
           ([sub (in-list subs)])
         (match sub
           [(cons p lt)
            (let ([p* (* prob-of-tree p)])
              (cond [(positive? p)
                     (values (heap-insert h (cons p* lt)) table prob-unexplored prob-accepted)]
                    [else
                     (when (verbose?)
                       (eprintf "WARNING: bad prob ~s in path\n" p*))
                     (values h table prob-unexplored prob-accepted)]))]))]
      [(failed reason)
       (let ([prob-unexplored (- prob-unexplored prob-of-tree)])
         (values h table prob-unexplored prob-accepted))]))

  ;; heap-loop : ... -> ...
  (define (heap-loop h table prob-unexplored prob-accepted)
    (cond [(and limit (< prob-unexplored (* limit prob-accepted)))
           ;; Done!
           (when (positive? (heap-count h))
             (when (verbose?)
               (eprintf "stopping with ~s unexplored path(s)\n" (heap-count h))))
           (done h table prob-unexplored prob-accepted)]
          [(zero? (heap-count h))
           ;; explored all paths
           (done h table prob-unexplored prob-accepted)]
          [else
           (let* ([sub (heap-find-min/max h)]
                  [h (heap-delete-min/max h)])
             ;; (eprintf "- picked ~s\n" sub)
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

(define (tabulate table prob-accepted #:normalize? [normalize? #t])
  (define entries
    (for/list ([(val prob) (in-hash table)])
      (list val (if normalize? (/ prob prob-accepted) prob))))
  (sort entries (order-<? datum-order)))

;; ----

(define (enum-fail reason)
  (define activation (activation-key))
  (define ctag (car activation))
  (abort-current-continuation ctag
   (lambda () (failed reason))))

(define (enum-ERP tag dist)
  (define activation (activation-key))
  (define ctag (car activation))
  (define memo-key (cadr activation))
  (define spconds (cddr activation))
  (define memo-table (unbox (memo-key)))
  (cond [(assq (current-label) spconds)
         => (lambda (e)
              (match (cdr e)
                [(spcond:equal value)
                 (define l (dist-pdf dist value))
                 (cond [(positive? l)
                        (call-with-composable-continuation
                         (lambda (k)
                           (abort-current-continuation
                            ctag
                            (lambda ()
                              ;; Note: create split w/ prob sum 1 so that explore
                              ;; gets prob-explored right.
                              (split
                               (list (cons l (lambda ()
                                               (call-with-enum-context ctag memo-key memo-table spconds
                                                 (lambda () (k value)))))
                                     (cons (- 1 l) (lambda () (failed #f))))))))
                         ctag)]
                       [else
                        (abort-current-continuation ctag
                          (lambda () (failed 'condition)))])]))]
        [else
         (unless (memq (car tag)
                       '(bernoulli flip discrete binomial geometric poisson continue-special))
           (error 'ERP "cannot enumerate non-discrete distribution: ~s" tag))
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
                                       (call-with-enum-context ctag memo-key memo-table spconds
                                         (lambda () (k (enum-ERP tail-tag dist))))))))
                        (for/list ([val (in-list vals)]
                                   [prob (in-list probs)])
                          (cons prob
                                (lambda ()
                                  (call-with-enum-context ctag memo-key memo-table spconds
                                    (lambda () (k val)))))))))))
          ctag)]))

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

;; ----

(define (enum-mem f)
  (define activation (activation-key))
  (define ctag (car activation))
  (define memo-key (cadr activation))
  (define f-key (gensym))
  (define (memoized-function . args)
    (let ([b (memo-key)]
          [key (cons f-key args)])
      (unless (continuation-prompt-available? ctag)
        (error 'mem
               "memoized function escaped its creating context\n  function: ~e\n  arguments: ~e\n"
               f args))
      (cond [(hash-has-key? (unbox b) key)
             (hash-ref (unbox b) key)]
            [else
             ;; Call with saved activation; may be outer enumeration!
             (define v
               (mark-parameterize ((activation-key activation))
                 (apply f args)))
             ;; NOTE: outer b might be stale, if f called ERP!
             (define b (memo-key))
             (set-box! b (hash-set (unbox b) key v))
             v])))
  memoized-function)

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
