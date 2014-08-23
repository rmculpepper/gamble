;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(provide (all-defined-out))

;; This module describes (in comments) the approaches used for
;; address-tracking (a la Bher) and conditioning. It also defines
;; parameters and functions used to contain that information.

;; See instrument.rkt for the implementation (through rewriting of
;; expanded modules) of the approaches described here.

;; ------------------------------------------------------------
;; Address-tracking

#|
How to represent an Address (ie, a point in evaluation, reasonably stable 
across runs with different random choices)?

Version 0:
An Address0 is a (listof CallSite).
-- the list of call sites in the context (ie, continuation), most recent first

Note: call sites, not functions. Consider (define (f) (g (h) (h)))---need to
distinguish separate calls to h. But what if function changes? Well, what if
argument changes? We're ignoring the latter, why not ignore the former too?

Store address in context using with-continuation-mark (WCM) and retrieve using
current-continuation-marks (CCM).

Problem: tail calls. If f tail-calls itself, then second WCM overwrites first;
calls within the two activations of f will have colliding addresses. Also if 
f tail-calls g then g tail-calls f, collisions in two f activations. And so on. 
(See sum-n-flips* in test-pl1.rkt.)

Version 1 (old):

An Address is a (listof CallSequence)
--- a list of tail-call sequences, most recent first
A CallSequence is an improper list of CallSite.
--- the list of tail calls (most recent first) together with 
    the non-tail call they start from at the end

Issue: May change space complexity of program---but it needs to make finer
distinctions than original program, so somewhat justified. Maybe devise ad-hoc
representation optimizations: eg, RLE for self-tail-calling functions.

Version 2 (current):

Use parameter instead of continuation marks. Parameter access seems to
be specially optimized in Racket runtime, so it is considerably faster
than current-continuation-marks.

Potential issue: will parameters cause problems for enumerate?

Another possibility is call-with-immediate-continuation-mark, which in
my microbenchmarks is slightly faster than parameters. It would also
allow for safety checking. The downside is all reads to the context
have to be in tail position wrt a call from instrumented code. So eg
flip impl would have to either grab context or call (current-ERP) in
tail position. Or flip could be defined in instrumented module. But
would contracts interfere? (Probably not, if both definition and use
site are instrumented.)

|#

;; the-context : (Parameterof (Listof Nat))
(define the-context (make-parameter null))

(define (get-context)
  (the-context))

;; Delimit call-site tracking.
;; Can't test using normal (f arg ...) syntax, because testing call-sites 
;; would be part of context! Use (apply/delimit f arg ...) instead.
(define (apply/delimit f . args)
  (parameterize ((the-context null))
    (apply f args)))


;; ------------------------------------------------------------
;; Observations

#|
Goal: implement (observe CC[(sample d)] v)
             as CC[(observe-at CC^-1[v])]

Conditioning contexts (CC) are defined thus:
CC ::=                   -- CCFrame rep:
       []
    |  (+ v CC)          -- v
    |  (- CC)            -- '-
    |  (cons CC e)       -- 'car
    |  (cons v CC)       -- 'cdr
    |  (reverse CC)      -- 'reverse
    |  ... more invertible functions that don't affect density
           (eg, * is bad, I think)

ConditioningContext = (list CCFrame ...)

The CC is implicitly stored in the continuation mark stack under the
obs-mark key. The value of the observation is stored in the observing?
parameter.

Note: in order to cooperate with unannotated code---ie, to detect
uninstrumented non-conditionable frames in context---store the CC one
frame back in the context. So in general:

    at CC[e], cms(CC) = (cons 'unknown 〚CC〛)

So if CC[(f)] -> CC[B[(g)]] --- B for bad frame, non-conditionable ---
then -> CC[B[CC'[e]]], cms(CC[B[CC'[]]]) will have an internal
'unknown frame indicating that we cannot invert the context to adjust 
the observed value.

(Need to explain this better. TODO: reread Jay's paper on serializable
continuations for servlets, which also wants to detect bad frames.)

So sample function will just overwrite top frame using WCM. Better to
overwrite than ignore first 'unknown, since overwriting also catches
CC[B[(observe ...)]].

More challenges:
- (let ([t conditionable-expr]) (+ v t))
   cf laziness w/ 2 kinds of forcing contexts
- conditions on accumulated lists (eg, for/list expansion)
- (begin (define X conditionable-expr) (observe X v))
- conditioning through lists/vectors/tables (indexed RVs)
- smarter +: pick conditionable expr even if not last

Optimizations:
- limit instrumentation to functions that could be called in CC
|#

#|
Major problem: How to check that condition is applied?

Idea 1: (observe e v) also checks e evaluates to v (equal?).
Problem: floating-point arithmetic may mean equality doesn't hold.

Idea 2: check approximate numeric equality
Problem: .... eh, maybe, but what tolerance?

Idea 3: Mutable flag checked when condition used by sample.
Problem: mutation bad for enum; given cons, can have many subconditions

Idea 4: Check structure of v up to numeric leaves, then use
  4a: approximate equality on reals
  4b: mutable counter for reals

What constitutes an unused condition?
eg (observe (cons 1 (bernoulli)) '(1 . 1))
The first part of the condition is "unused" but still satisfied.

|#

(struct observation (value))

(define obs-mark 'observe)
(define obs-prompt (make-continuation-prompt-tag))

(define observing? (make-parameter #f))

(define (call/observe-context thunk expected)
  (define obs (observation expected))
  (define actual
    (call-with-continuation-prompt
     (lambda ()
       (parameterize ((observing? obs))
         (with-continuation-mark obs-mark 'unknown (thunk))))
     obs-prompt))
  ;; Check that result "equals" value.
  ;; FIXME: make equality approximation parameter or optional arg to observe?
  (check-observe-result expected actual)
  actual)

(define (check-observe-result expected0 actual0)
  (define (bad)
    ;; This is an error rather than a (fail) because it indicates that
    ;; a condition is being applied to a non-conditionable expr.
    ;; FIXME: statically detect conditionable exprs
    (error 'observe "observation failed\n  expected: ~e\n  got: ~e"
           expected0 actual0))
  (let loop ([expected expected0] [actual actual0])
    (cond [(pair? expected)
           (unless (pair? actual) (bad))
           (loop (car expected) (car actual))
           (loop (cdr expected) (cdr actual))]
          [(rational? expected)
           ;; FIXME: ??? better general way of checking "close-enough" for reals ???
           (unless (rational? expected) (bad))
           (unless (or (= actual expected)
                       (<= (abs (- expected actual))
                           (* 0.001 (max (abs expected) (abs actual)))))
             (bad))]
          [else (unless (equal? actual expected) (bad))])))

(define (get-observe-context)
  (and (observing?)
       (continuation-mark-set->list
        (current-continuation-marks obs-prompt)
        obs-mark
        obs-prompt)))

(define (get-adjusted-observation)
  (let ([obs (observing?)])
    (and (observation? obs)
         (interpret-context (get-observe-context) (observation-value obs)))))

(define (observe-context-adjust-value ctx obs)
  (interpret-context ctx (observation-value obs)))

(define (interpret-context ctx val)
  (foldr interpret-frame val ctx))

(define (interpret-frame frame val)
  (cond [(real? frame)  ;; (+ frame [])
         (- val frame)]
        [(eq? frame 'car) ;; (cons [] e)
         (car val)]
        [(eq? frame 'cdr) ;; (cons v [])
         (cdr val)]
        [(eq? frame 'reverse) ;; (reverse [])
         (reverse val)]
        [(eq? frame 'ok)
         val]
        [(eq? frame 'unknown)
         (error 'observe "expression is not conditionable;\n ~a"
                "`sample' context contains unknown unconditionable frame")]
        [else
         (error 'observe "expression is not conditionable;\n ~a\n  bad frame: ~e"
                "`sample' context contains unconditionable frame"
                frame)]))

#|
;; Test script:

(require prob/private/context)
(begin
  ;; f must be uninstrumented to avoid 'unknown frame!
  (define (f x)
    (with-continuation-mark obs-mark 'ok
      (begin (eprintf "ctx = ~s\n" (get-observe-context))
             (eprintf "adj = ~s\n" (get-adjusted-observation))
             (or (get-adjusted-observation)
                 (random 10)))))
  (require prob))
(begin
  (define (g x) (let ([t (f (* 2 x))]) (+ t 23 (f x))))
  (define (h x) (+ 17 (g x))))
(begin
  (define (gg x) (cons (f x) (cons (+ 1 (f x)) null)))
  (define (hh x) (reverse (gg x))))


(list (h 12))
(call/observe-context (lambda () (h 12)) 0)

(list (hh 1))
(call/observe-context (lambda () (hh 1)) '(2 1))
|#
