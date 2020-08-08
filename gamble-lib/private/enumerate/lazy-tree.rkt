;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         markparam
         "../interfaces.rkt"
         "../dist/base.rkt"
         "../dist/density.rkt")
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
;; - (split (Dist B) (-> B (EnumTree A)) Nat/#f)
;;     where Nat represents continuing enumeration of infinite int-dist
;; - (failed Any)
;; - (weight Density (-> (EnumTree A)))
(struct only (answer))
(struct split (dist k start))
(struct failed (reason))
(struct weight (dn k))

;; ----------------------------------------

;; reify-tree : (-> A) -> (EnumTree A)
(define (reify-tree thunk limit?)
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

    (define/private (call/restore proc)
      (define act (current-activation))
      (define ctag (activation-prompt act))
      (define memo-key (activation-memo-table-key act))
      (define memo-table (unbox (memo-key)))
      (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation
          ctag
          (lambda ()
            (proc k (lambda (continue) (call-with-enum-context act memo-table continue))))))
       ctag))

    (define/public (sample dist _id)
      (call/restore
       (lambda (k restore)
         (split dist (lambda (v) (restore (lambda () (k v)))) #f))))

    (define/public (lscore ll) (dn-score (make-density #f ll 0) (void)))
    (define/public (nscore l) (dn-score (make-density l #f 0) (void)))
    (define/public (observe dist val _scale) (dn-score (dist-density dist val) val))
    (define/private (dn-score dn val)
      (call/restore
       (lambda (k restore)
         (weight dn (lambda () (restore (lambda () (k val))))))))

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
        (let ([b (memo-key)]
              [key (cons f-key args)])
          (cond [(hash-has-key? (unbox b) key)
                 (hash-ref (unbox b) key)]
                [else
                 ;; Call with saved activation; may be outer enumeration!
                 (define v (mark-parameterize ((current-activation act)) (apply f args)))
                 ;; NOTE: outer b might be stale, if f called ERP!
                 (define b (memo-key))
                 (set-box! b (hash-set (unbox b) key v))
                 v])))
      memoized-function)

    (define/public (trycatch p1 p2)
      (error 'trycatch "not implemented"))
    ))

;; ----

;; split->subtrees : split Boolean -> (Listof (List Prob (-> (EnumTree A))))
(define (split->subtrees s)
  (match-define (split dist k start) s)
  (define enum (dist-enum dist))
  (define result
  (cond [(integer? enum)
         (for/list ([i (in-range enum)])
           (cons (dist-density dist i) (lambda () (k i))))]
        [(vector? enum)
         (for/list ([v (in-vector enum)])
           (cons (dist-density dist v) (lambda () (k v))))]
        ;; [(eq? enum 'lazy)
        ;;  (split->subtrees*/lazy dist k (or start 0))]
        [else ;;(eq? enum #f)
         (error 'enumerate "cannot enumerate distribution\n  distribution: ~e" dist)]
        #;[else (error 'enumerate "internal error: bad enum value: ~e" enum)]))
  result)
