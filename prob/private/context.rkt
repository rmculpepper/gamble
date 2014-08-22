;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base))
(provide the-context
         get-context
         apply/delimit)

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

---- See instrument.rkt for instrumentation ----

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
