#lang racket/base
(require (for-syntax racket/base))
(provide CM-KEY
         app/call-site
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

Current:

An Address is a (listof CallSequence)
 -- a list of tail-call sequences, most recent first
A CallSequence is an improper list of CallSite.
 -- the list of tail calls (most recent first) together with 
    the non-tail call they start from at the end

Issue: May change space complexity of program---but it needs to make finer
distinctions than original program, so somewhat justified. Maybe devise ad-hoc
representation optimizations: eg, RLE for self-tail-calling functions.
|#

(define CM-KEY 'call-stack)

(define-syntax (app/call-site stx)
  (syntax-case stx ()
    [(app/call-site call-site f arg ...)
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #`(let ([c call-site] [tmp-f f] [tmp-arg arg] ...)
           (call-with-immediate-continuation-mark CM-KEY
             (lambda (v)
               (with-continuation-mark CM-KEY (if v (cons c v) c)
                 (#%app tmp-f tmp-arg ...))))))]))

(define (get-context)
  (continuation-mark-set->list (current-continuation-marks) CM-KEY))

;; Delimit call-site tracking.
;; Can't test using normal (f arg ...) syntax, because testing call-sites 
;; would be part of context! Use (apply/delimit f arg ...) instead.
(define (apply/delimit f . args)
  (call-with-continuation-prompt (lambda () (apply f args))))
