;; Experiment to adapt technique from "Lightweight Implementations of 
;; Probabilistic Programming Languages Via Transformational Compilation"
;; by al. et Goodman to use continuation marks to localize rewriting and
;; avoid changing function signatures.

;; Like pl1, but uses post-expansion instrumentation instead of
;; redefining #%app.

;; Like pl2, but uses trampoline macro instead of monolithic
;; instrumentor function.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         racket/list
         "private/prob.rkt"
         "private/pl1-context.rkt"
         "private/pl1-prob.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin #%top-interaction)
         (rename-out [instrumenting-module-begin #%module-begin]
                     [instrumenting-top-interaction #%top-interaction])
         (all-from-out "private/prob.rkt")
         (all-from-out "private/pl1-context.rkt")
         (all-from-out "private/pl1-prob.rkt"))

(current-ERP pl1-ERP)

#|
See private/pl1-context.rkt for discussion of Address representation.
|#

#|
A CallSite is a Nat.
--- counter of occurrences of #%plain-app syntax in expanded program

Note: the counter resets for each new module, repl compilation.
Can fix with gensym or pairing with module-path, etc, but no need for now.
Will need to fix for real (multi-module) programs.
Note: instrumenting-module? hack separates module from repl numbers.
|#
(begin-for-syntax
  (define instrumenting-module? (make-parameter #t))
  (define counter 0)
  (define (next-counter)
    (set! counter (add1 counter))
    counter))

;; FIXME: instead of monolithic instrumentor, maybe use instrument macro?

(define-syntax (instrumenting-module-begin stx)
  (syntax-case stx ()
    [(instrumenting-module-begin form ...)
     (with-syntax ([e-module-body
                    (local-expand #'(#%module-begin form ...)
                                  'module-begin
                                  null)])
       #'(instrument e-module-body))]))

(define-syntax (instrumenting-top-interaction stx)
  (syntax-case stx ()
    [(instrumenting-top-interaction . e)
     (parameterize ((instrumenting-module? #f))
       (let ([estx (local-expand #'(#%top-interaction . e) 'top-level #f)])
         (syntax-case estx (begin)
           [(begin form ...)
            #'(begin (instrumenting-top-interaction . form) ...)]
           [form
            (with-syntax ([e-form (local-expand #'form 'top-level null)])
              #'(instrument e-form))])))]))

(begin-for-syntax
 (define stx-insp
   (variable-reference->module-declaration-inspector
    (#%variable-reference))))

(define-syntax (instrument stx0)
  (syntax-parse stx0
    [(instrument form-to-instrument)
     (define stx (syntax-disarm #'form-to-instrument stx-insp))
     (define instrumented
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         ;; Fully-Expanded Programs
         ;; Rewrite applications
         [(#%plain-app e ...)
          (with-syntax ([c (if (instrumenting-module?)
                               (next-counter)
                               (- (next-counter)))])
            (when #f
              (eprintf "app ~s = ~a:~a ~.s\n\n"
                       (syntax-e #'c)
                       (or (syntax-line stx) '?)
                       (or (syntax-column stx) '?)
                       (syntax->datum stx)))
            #'(app/call-site 'c (instrument e) ...))]
         ;; Just recur through all other forms
         ;; -- module body
         [(#%plain-module-begin form ...)
          #'(#%plain-module-begin (instrument form) ...)]
         ;; -- module-level form
         [(#%provide . _) stx]
         [(begin-for-syntax . _) stx]
         [(module . _) stx]
         ;; [(module* . _) stx]
         ;; [(#%declare . _) stx]
         ;; -- general top-level form
         [(define-values ids e)
          #'(define-values ids (instrument e))]
         [(define-syntaxes . _) stx]
         [(#%require . _) stx]
         ;; -- expr
         [var:id #'var]
         [(#%plain-lambda formals e ...)
          #'(#%plain-lambda formals (instrument e) ...)]
         [(case-lambda [formals e ...] ...)
          #'(case-lambda [formals (instrument e) ...] ...)]
         [(if e1 e2 e3)
          #'(if (instrument e1) (instrument e2) (instrument e3))]
         [(begin e ...)
          #'(begin (instrument e) ...)]
         [(begin0 e ...)
          #'(begin0 (instrument e) ...)]
         [(let-values ([vars rhs] ...) body ...)
          #'(let-values ([vars (instrument rhs)] ...) (instrument body) ...)]
         [(letrec-values ([vars rhs] ...) body ...)
          #'(letrec-values ([vars (instrument rhs)] ...) (instrument body) ...)]
         [(set! var e)
          (eprintf "** set! in expanded code: ~e" (syntax->datum stx))
          #'(set! var (instrument e))]
         [(quote d) stx]
         [(quote-syntax s) stx]
         [(with-continuation-mark e1 e2 e3)
          #'(with-continuation-mark (instrument e1) (instrument e2) (instrument e3))]
         ;; #%plain-app -- see above
         [(#%top . _) stx]
         [(#%variable-reference . _) stx]
         [(#%expression e)
          #'(#%expression (instrument e))]
         [_
          (raise-syntax-error #f "unhandled syntax" stx)]
         ))
     ;; Rearm and track result
     (syntax-rearm (if (eq? stx instrumented)
                       stx
                       (syntax-track-origin instrumented stx #'instrument))
                   #'form-to-instrument)]))

;; ----

#|

TODO: avoid instrumenting *safe* applications?
  - eg known first-order functions like list, +, etc
  - potential benefits: speed, code size, size of address reps

* See also pl1.rkt

Issue: interop w/ Racket, etc
- solved issue with for/*, other macros
- HO functions still not ok
- Idea: develop mem-like context-replacement/augmentation discipline?
  eg, (lift map) = (lambda (f xs) (map (lift* f) xs))
      where (lift* f) = (lambda (x) (app/call-site (list 'map f x) f x)) ????

|#
