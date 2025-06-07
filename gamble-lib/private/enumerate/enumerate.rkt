;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         "../dist/base.rkt"
         "../dist/discrete.rkt"
         "../interfaces.rkt"
         (submod "../dist/util.rkt" density)
         (submod "../dist/util.rkt" math)
         #;"pairingheap.rkt")
(provide enumerate)

;; ============================================================

(define (enumerate thunk)
  (define ctx (new enumerate-stochastic-ctx%))
  (hash->discrete-dist
   (let loop ([h (hash)] [dn one-density] [thunk (lambda () (send ctx run thunk))])
     (match (thunk)
       [(done v)
        (hash-set h v (density+ dn (hash-ref h v #f)))]
       [(? list? wdn+continue-list)
        (for/fold ([h h]) ([wdn+continue (in-list wdn+continue-list)])
          (match-define (cons wdn continue) wdn+continue)
          (loop h (density* wdn dn) continue))]))))

;; ------------------------------------------------------------
;; Nesting enumerations
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

;; ------------------------------------------------------------
;; Notes on Parameters and Delimited Continuations
;;
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

;; A (EnumTree A) is one of
;; - (done A)
;; - (listof (cons Density (-> (EnumTree A))))
(struct done (answer))

;; ============================================================

(define enumerate-stochastic-ctx%
  (class plain-stochastic-ctx%
    (super-new)

    (define memo-key (gensym))
    (define ctag (make-continuation-prompt-tag))

    (define/override (sample dist _id)
      (call/restore
       (lambda (k restore)
         (for/list ([(v w) (in-dist dist)])
           (cons (density w 0 #f) (lambda () (restore (lambda () (k v)))))))))

    (define/override (dscore dn)
      (call/restore
       (lambda (k restore)
         (list (cons dn (lambda () (restore (lambda () (k (void))))))))))

    (define/override (fail reason)
      (call/restore
       (lambda (k restore)
         null)))

    (define/override (run thunk)
      (call (hash) (lambda () (done (thunk)))))

    (define/private (call memo-table thunk)
      (parameterize ((current-stochastic-ctx this))
        (with-continuation-mark memo-key (box memo-table)
          (call-with-continuation-prompt thunk ctag))))

    (define/private (call/restore proc)
      (define memo-table (unbox (continuation-mark-set-first #f memo-key)))
      (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation ctag
          (lambda () (proc k (lambda (continue) (call memo-table continue))))))
       ctag))

    (define/override (mem f)
      (define f-key (gensym))
      (define (memoized-function . args)
        (unless (continuation-prompt-available? ctag)
          (error 'mem
                 (string-append "memoized function escaped its creating context"
                                "\n  function: ~e\n  arguments: ~e\n")
                 f args))
        (define b (continuation-mark-set-first #f memo-key))
        (define key (cons f-key args))
        (cond [(hash-has-key? (unbox b) key)
               (hash-ref (unbox b) key)]
              [else
               ;; Call with creating context; may be outer enumeration!
               (define v
                 (parameterize ((current-stochastic-ctx this))
                   (apply f args)))
               ;; NOTE: outer b might be stale, if f called ERP!
               (define b (continuation-mark-set-first #f memo-key))
               (set-box! b (hash-set (unbox b) key v))
               v]))
      memoized-function)
    ))
