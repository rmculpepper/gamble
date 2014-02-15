;; Experiment to adapt technique from "Lightweight Implementations of 
;; Probabilistic Programming Languages Via Transformational Compilation"
;; by al. et Goodman to use continuation marks to localize rewriting and
;; avoid changing function signatures.

#lang racket/base
(require (for-syntax racket/base)
         racket/list
         "private/prob.rkt"
         "private/context.rkt"
         "private/prob-db.rkt")
(provide (except-out (all-from-out racket/base) #%app)
         (rename-out [instrumenting-app #%app])
         (all-from-out "private/prob.rkt")
         (all-from-out "private/context.rkt")
         (all-from-out "private/prob-db.rkt"))

(current-ERP db-ERP)
(current-mem db-mem)

#|
See private/context.rkt for discussion of Address representation.
|#

#|
A CallSite is a Nat.
--- counter of occurrences of #%app syntax in program

Note: the counter resets for each new module, repl compilation.
Can fix with gensym or pairing with module-path, etc, but no need for
now.  Will need to fix for real (multi-module) programs.
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
       #`(app/call-site '#,c f arg ...))]))

;; ----

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
