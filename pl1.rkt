;; Experiment to adapt technique from "Lightweight Implementations of 
;; Probabilistic Programming Languages Via Transformational Compilation"
;; by al. et Goodman to use continuation marks to localize rewriting and
;; avoid changing function signatures.

#lang racket/base
(require (for-syntax racket/base)
         racket/list)
(provide (except-out (all-from-out racket/base) #%app)
         (rename-out [instrumenting-app #%app]))

#|
How to represent an Address (ie, a point in evaluation, reasonably stable 
across runs with different random choices)?

Version 0:
An Address0 is a (listof CallSite).
-- the list of call sites in the context (ie, continuation).

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
 -- a list of tail-call sequences (most recent first)
A CallSequence is an improper list of CallSite.
 -- the list of tail calls (most recent first) together with 
    the non-tail call they start from at the end

Issue: May change space complexity of program---but it needs to make finer
distinctions than original program, so somewhat justified. Maybe devise ad-hoc
representation optimizations: eg, RLE for self-tail-calling functions.
|#

(define CM-KEY 'call-stack)

#|
A CallSite is a Nat.
--- counter of occurrences of #%app syntax in program

Note: the counter resets for each new module, repl compilation.
Can fix with gensym or pairing with module-path, etc, but no need for now.
Will need to fix for real (multi-module) programs.
|#
(begin-for-syntax
  (define counter 0)
  (define (next-counter)
    (begin0 counter
      (set! counter (add1 counter)))))

(define-syntax (instrumenting-app stx)
  (syntax-case stx ()
    [(app f arg ...)
     (let ([c (next-counter)])
       (eprintf "app ~s = ~a:~a ~.s\n" c 
                (syntax-line stx) (syntax-column stx)
                (cdr (syntax->datum stx)))
       (with-syntax ([(tmp-arg ...)
                      (generate-temporaries #'(arg ...))])
         #`(let ([tmp-f f] [tmp-arg arg] ...)
             (call-with-immediate-continuation-mark
              CM-KEY
              (lambda (v)
                (with-continuation-mark CM-KEY (if v (cons '#,c v) '#,c)
                  (#%app tmp-f tmp-arg ...)))))))]))

(define (get-context)
  (continuation-mark-set->list (current-continuation-marks) CM-KEY))

;; ----

(define current-log (make-hash))
(define last-log (make-hash))

(define (print-log)
  (for ([(k v) (in-hash current-log)])
    (printf "~s => ~s\n" k v)))

;; Delimit call-site tracking.
;; Can't test log using normal (f arg ...) syntax, because testing call-sites 
;; would be part of context! Use (apply/delimit f arg ...) instead.
(define (apply/delimit f . args)
  (call-with-continuation-prompt (lambda () (apply f args))))

(define (reset-log)
  (eprintf "Resetting log\n")
  (set! last-log current-log)
  (set! current-log (make-hash)))

(define (apply/reset f . args)
  (reset-log)
  (apply apply/delimit f args))

(provide current-log
         last-log
         print-log
         apply/delimit
         reset-log
         apply/reset)

;; ----

(define (flip)
  (define context (get-context))
  (define result
    (cond [(hash-ref current-log context #f)
           => (lambda (v)
                (if (and (pair? context)
                         (let ([frame (last context)])
                           (and (list? frame) (memq 'mem frame))))
                    (eprintf "- reusing flip -- mem: ~s\n" context)
                    (eprintf "- reusing flip -- context collision: ~s\n" context))
                v)]
          [(hash-ref last-log context #f)
           => (lambda (v)
                (eprintf "- reusing flip -- history: ~s\n" context)
                v)]
          [else 
           (eprintf "- flipping -- context = ~s\n" context)
           (random 2)]))
  (hash-set! current-log context result)
  result)

(define (mem f)
  (define context (get-context))
  (lambda args
    (call-with-continuation-prompt
     (lambda ()
       (with-continuation-mark CM-KEY (list 'mem args)
         (apply f args))))))

(provide flip
         mem)

#|

Issue: tail recursion
- Need to distinguish different calls in tail-call sequence.
- What exactly are we using as a proxy for "eval node", anyway?
  - Don't want too much info (eg args, env contents), because it changes
    from run to run.
  - Need some context info to distinguish calls -- eg, (begin (flip) (flip))

Issue: useless WCMs
- Is this even a problem? Probably WCM is cheap, CCM is expensive.
- Maybe avoid via static analysis.

Issue: map, for/*, etc not annotated
- annotation instead of replace #%app would solve for for/*
  - naive annotator might introduce *many* more WCMs, though
- replacements for common HO functions, like map
- what about interop story?

|#
