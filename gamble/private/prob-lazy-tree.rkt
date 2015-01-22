;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (rename-in racket/match [match-define defmatch])
         racket/class
         unstable/markparam
         "interfaces.rkt"
         (only-in "context.rkt" OBS-mark)
         "dist.rkt")
(provide (struct-out only)
         (struct-out split)
         (struct-out failed)
         (struct-out weight)
         reify-tree
         split->subtrees)

;; == mem ==
;;
;; For mem, have an activation-specific store that gets rewound after
;; each ERP choice exploration. (cf logic continuations, or store
;; continuations?)
;;
;; Use mark-parameter to store the memotable. (See notes on parameters
;; below.)  Since prompts also delimit CCM, need a separate prompt tag
;; (sigh... it's complicated).
;;
;; What should it mean for a memoized function to escape the enumeration
;; in which it was created? Should probably be forbidden.

;; == Nesting enumerations ==
;;
;; How to make enumeration nest?
;;
;; (enum ;; outer
;;  ...
;;  (enum ;; inner
;;   ...))
;;
;; - Straightforward except for mem:
;;
;;   - An outer-created memoized function that is invoked in the inner
;;     enum should fork its possibilities to the *outer* prompt.
;;   - Except... what if the outer-mem-fun calls its argument, which is an
;;     inner-mem-fun? Then that "should" fork its possibilities to inner
;;     prompt.
;;   - Bleh, mem probably only makes sense on first-order functions.
;;   - Alternatively, in that case we say the inner-mem-fun has escaped
;;     its context, error. (In general, mem-fun that escapes its context
;;     is problematical, except for direct-style mem.)
;;   - What if outer-mem-fun is (lambda (n) (lambda () (flip (/ n))))?
;;     Then if applied, gets thunk, then applied in inner, inner explores
;;     branches. That seems reasonable.
;;
;;   - Anyway... when an outer-mem-fun is invoked, it needs to restore
;;     the outer ERP (and mem) impls.
;;     - That means nested enum can't use parameterize ... :/
;;       ??? Doesn't work without parameterize ... investigate?
;;     - A memoized function must close over the activation support (ctag,
;;       markparam) for the mem that created it.
;;   - Each enumeration activation needs a separate prompt tag and
;;     memo-table key.
;;   - explore must be rewritten in pure code: find functional priority
;;     queue (PFDS from planet?), use immutable hash, etc


;; == Notes on Parameters and Delimited Continuations ==

;; In the general case, Racket's parameters do not work interact
;; "correctly" with delimited continuations, in the sense that a
;; parameter P's value is not determined by the nearest (parameterize
;; ((P _)) []) in the context. (Parameters are grouped together into a
;; parameterization, and the nearest parameterization is fetched. This
;; is a known Racket WONTFIX.)
;;
;; However, the way 'enumerate' uses parameters is safe, since
;; captured continuations are invoked in dynamic contexts that are
;; mostly "compatible" with the ones they were captured in. But note:
;;
;;  - The invocation context needs a different memo-table, so the
;;    memo-table must be stored using a mark-parameter rather than an
;;    ordinary parameter.
;;  - The 'explore' function cannot use parameterize to affect the execution
;;    of the code that produces the lazy tree. The parameterization is
;;    essentially captured by the call to 'reify-tree'.

;; ============================================================

;; Each enumeration activation has a prompt-tag and memo-table key.
;; The current prompt-tag is stored by ctag-key.

;; An Activation is (activation PromptTag MarkParam)
;; where (memo-key) : (Boxof (Hashof ArgList => Result))
(struct activation (prompt memo-table-key))

;; (current-activation) : Activation
(define current-activation (make-parameter #f))

;; A (EnumTree A) is one of
;; - (only A)
;; - (split Any (Dist B) (-> B (EnumTree A)) Nat/#f)
;;     where Nat represents continuing enumeration of infinite int-dist
;; - (failed Any)
;; - (weight PositiveReal (-> (EnumTree A)))
(struct only (answer))
(struct split (label dist k start))
(struct failed (reason))
(struct weight (dist val scale k))

;; ----------------------------------------

;; reify-tree : (-> A) (Listof Condition) -> (EnumTree A)
(define (reify-tree thunk)
  (define memo-key (mark-parameter))
  (define memo-table (hash))
  (define ctag (make-continuation-prompt-tag))
  (define act (activation ctag memo-key))
  (call-with-enum-context act memo-table (lambda () (only (thunk)))))

;; calls thunk with memo-table in fresh box, parameters set up
(define (call-with-enum-context act memo-table thunk)
  (define ctag (activation-prompt act))
  (define memo-key (activation-memo-table-key act))
  (parameterize ((current-stochastic-ctx
                  (new lazy-tree-stochastic-ctx%))
                 (current-activation (activation ctag memo-key)))
    (mark-parameterize ((memo-key (box memo-table)))
      (call-with-continuation-prompt thunk (activation-prompt act)))))

;; ----

(define lazy-tree-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (super-new)

    (define/public (sample dist)
      (define act (current-activation))
      (define ctag (activation-prompt act))
      (define memo-key (activation-memo-table-key act))
      (define memo-table (unbox (memo-key)))
      (define label (current-label))
      (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation
          ctag
          (lambda ()
            (split label dist
                   (lambda (v)
                     (call-with-enum-context act memo-table
                       (lambda () (k v))))
                   #f))))
       ctag))

    (define/public (observe-sample dist val scale)
      (define act (current-activation))
      (define ctag (activation-prompt act))
      (define memo-key (activation-memo-table-key act))
      (define memo-table (unbox (memo-key)))
      (define label (current-label))
      (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation
          ctag
          (lambda ()
            (weight dist val scale
                    (lambda ()
                      (call-with-enum-context act memo-table
                        (lambda () (k val))))))))
       ctag))

    (define/public (fail reason)
      (abort-current-continuation (activation-prompt (current-activation))
        (lambda () (failed reason))))

    (define/public (mem f)
      (define act (current-activation))
      (define ctag (activation-prompt act))
      (define memo-key (activation-memo-table-key act))
      (define f-key (gensym))
      (define (memoized-function . args)
        (unless (continuation-prompt-available? ctag)
          (error 'mem
            "memoized function escaped its creating context\n  function: ~e\n  arguments: ~e\n"
            f args))
        (call-with-immediate-continuation-mark OBS-mark
          (lambda (obs)
            (let ([b (memo-key)]
                  [key (cons f-key args)])
              (cond [(hash-has-key? (unbox b) key)
                     (hash-ref (unbox b) key)]
                    [else
                     ;; Call with saved activation; may be outer enumeration!
                     (define v
                       (mark-parameterize ((current-activation act))
                         (with-continuation-mark OBS-mark obs
                           (apply f args))))
                     ;; NOTE: outer b might be stale, if f called ERP!
                     (define b (memo-key))
                     (set-box! b (hash-set (unbox b) key v))
                     v])))))
      memoized-function)
    ))

;; ----

;; split->subtrees : split Boolean -> (Listof (List Prob (-> (EnumTree A))))
(define (split->subtrees s logspace?)
  (defmatch (split _label dist k start) s)
  (define enum (dist-enum dist))
  (define result
  (cond [(eq? enum #f)
         (error 'enumerate "cannot enumerate distribution\n  distribution: ~e" dist)]
        [(eq? enum 'lazy)
         (split->subtrees*/lazy dist k (or start 0) logspace?)]
        [(integer? enum)
         (for/list ([i (in-range enum)])
           (cons (dist-pdf dist i logspace?) (lambda () (k i))))]
        [(vector? enum)
         (for/list ([v (in-vector enum)])
           (cons (dist-pdf dist v logspace?) (lambda () (k v))))]
        [else (error 'enumerate "internal error: bad enum value: ~e" enum)]))
  result)

;; split->subtrees*/lazy : Dist (-> Value (EnumTree A))
;;                      -> (Listof (Cons Prob (-> (EnumTree A))))
(define (split->subtrees*/lazy dist k start logspace?)
  (define-values (vals probs tail-prob next)
    (lazy-dist->vals+probs dist start logspace?))
  (cons (cons tail-prob (lambda () (split dist k next)))
        (for/list ([val (in-list vals)]
                   [prob (in-list probs)])
          (cons prob (lambda () (k val))))))

(define TAKE 10)

;; lazy-dist->vals+probs : Dist Nat Boolean -> (values (Listof A) (Listof Prob) Prob Nat)
(define (lazy-dist->vals+probs dist start logspace?)
  (define the-scale (dist-cdf dist (sub1 start) logspace? #t))
  (define (scale a) (if logspace? (- a the-scale) (/ a the-scale)))
  (define-values (vals probs)
    (for/lists (vals probs) ([i (in-range start (+ start TAKE))])
      (values i (scale (dist-pdf dist i logspace?)))))
  (values vals
          probs
          (scale (dist-cdf dist (+ start TAKE -1) logspace? #t))
          (+ start TAKE)))
