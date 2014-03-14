;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         data/order
         racket/control
         unstable/markparam
         "prob-hooks.rkt"
         "prob-enum-common.rkt"
         "pairingheap.rkt"
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
;; The current prompt-tag is stored by ctag-key.f

(define ctag-table (make-weak-hasheq))
(define (ctag-name ctag)
  (hash-ref! ctag-table ctag (lambda () (gensym 'CTAG_))))

;; CCM(activation-key) : (cons PromptTag MarkParameter)
(define activation-key (mark-parameter))

;; CCM(memo-key) : (Boxof (Hash[ArgList => Result]))

;; enumerate* : (-> A) (A -> Boolean) (A -> B) etc -> (Listof (List B Prob))
;; Note: pred and project must be pure; applied outside of prompt
(define (enumerate* thunk pred [project values]
                    #:limit [limit 1e-6]
                    #:normalize? [normalize? #t])
  (define memo-key (mark-parameter))
  (define memo-table (hash))
  (define ctag (make-continuation-prompt-tag))
  (define-values (table prob-unexplored prob-accepted)
    (explore (call-with-enum-context ctag memo-key memo-table (lambda () (only (thunk))))
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

;; calls thunk with memo-table in fresh box, parameters set up
(define (call-with-enum-context ctag memo-key memo-table thunk)
  (parameterize ((current-ERP enum-ERP)
                 (current-mem enum-mem))
    (mark-parameterize ((memo-key (box memo-table))
                        (activation-key (cons ctag memo-key)))
      (call-with-continuation-prompt thunk ctag))))

;; ----

(define (enum-ERP tag dist)
  (unless (memq (car tag) '(bernoulli flip discrete binomial geometric poisson continue-special))
    (error 'ERP "cannot enumerate non-discrete distribution: ~s" tag))
  (define activation (activation-key))
  (define ctag (car activation))
  (define memo-key (cdr activation))
  (define memo-table (unbox (memo-key)))
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
                                (call-with-enum-context ctag memo-key memo-table
                                  (lambda () (k (enum-ERP tail-tag dist))))))))
                 (for/list ([val (in-list vals)]
                            [prob (in-list probs)])
                   (cons prob
                         (lambda ()
                           (call-with-enum-context ctag memo-key memo-table
                             (lambda () (k val)))))))))))
   ctag))

(define (cons/f x xs) (if x (cons x xs) xs))

;; ----

(define (enum-mem f)
  (define activation (activation-key))
  (define ctag (car activation))
  (define memo-key (cdr activation))
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
               (mark-parameterize ((activation-key (cons ctag memo-key)))
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
