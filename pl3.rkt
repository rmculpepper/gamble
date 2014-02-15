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
         "private/prob-enum.rkt"
         "private/context.rkt"
         "private/prob-db.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin #%top-interaction)
         (rename-out [instrumenting-module-begin #%module-begin]
                     [instrumenting-top-interaction #%top-interaction])
         (all-from-out "private/prob.rkt")
         (all-from-out "private/prob-enum.rkt")
         (all-from-out "private/context.rkt")
         (all-from-out "private/prob-db.rkt"))

#|
See private/context.rkt for discussion of Address representation.
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
    (if (instrumenting-module?)
        counter
        (- counter))))

;; FIXME: instead of monolithic instrumentor, maybe use instrument macro?

(define-syntax (instrumenting-module-begin stx)
  (syntax-case stx ()
    [(instrumenting-module-begin form ...)
     (with-syntax ([e-module-body
                    (local-expand #'(#%module-begin form ...)
                                  'module-begin
                                  null)])
       #'(instrument e-module-body #:un))]))

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
              #'(instrument e-form #:un))])))]))

(begin-for-syntax
 (define stx-insp
   (variable-reference->module-declaration-inspector
    (#%variable-reference))))

#|
To get list of '#%kernel exports:
(define (simplify e) (match e [`(just-meta ,n (rename '#%kernel ,x ,_)) x] [_ #f]))
(define knames
  (filter symbol?
          (map simplify
               (cdr (syntax->datum (expand '(require (rename-in '#%kernel))))))))
|#

;; (instrument expanded-form Mode)
;; where Mode is one of:
;;    #:nt   - non-tail: definitely not in tail position wrt any WCM with CM-MARK
;;    #:tail - tail: definitely in tail position wrt a WCM with CM-MARK
;;    #:un   - unknown: use expansion that's safe and correct either way
;; Non-tail mode has more efficient impl than #:tail/#:un; use when possible.
(define-syntax (instrument stx0)
  (syntax-parse stx0
    [(instrument form-to-instrument m)
     (define stx (syntax-disarm #'form-to-instrument stx-insp))
     (define instrumented
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         ;; Fully-Expanded Programs
         ;; Rewrite applications
         [(#%plain-app) stx]
         [(#%plain-app f:id e ...)
          (define classification (classify-function #'f))
          (cond [(eq? classification 'safe)
                 ;; "safe" function: doesn't need app/call-site*
                 ;; Benefit: for tail calls, avoid call-with-immediate-continuation-mark
                 #'(#%plain-app f (instrument e #:nt) ...)]
                [else
                 ;; unsafe or unknown; use app/call-site*
                 (when #f
                   (when (and (eq? classification 'unknown)
                              (not (eq? (syntax->datum #'m) '#:nt)))
                     (eprintf "## ~s\n" (syntax-e #'f))))
                 (with-syntax ([c (next-counter)])
                   #'(app/call-site* m 'c f (instrument e #:nt) ...))])]
         [(#%plain-app f e ...)
          (with-syntax ([c (next-counter)])
            (when #f
              (when (not (eq? (syntax->datum #'m) '#:nt))
                (eprintf "## ~s\n" (syntax->datum #'f))))
            (when #f
              (eprintf "app ~s = ~a:~a ~.s\n\n"
                       (syntax-e #'c)
                       (or (syntax-line stx) '?)
                       (or (syntax-column stx) '?)
                       (syntax->datum stx)))
            #'(app/call-site* m 'c (instrument f #:nt) (instrument e #:nt) ...))]
         ;; Just recur through all other forms
         ;; -- module body
         [(#%plain-module-begin form ...)
          #'(#%plain-module-begin (instrument form #:nt) ...)]
         ;; -- module-level form
         [(#%provide . _) stx]
         [(begin-for-syntax . _) stx]
         [(module . _) stx]
         ;; [(module* . _) stx]
         ;; [(#%declare . _) stx]
         ;; -- general top-level form
         [(define-values ids e)
          #'(define-values ids (instrument e #:nt))]
         [(define-syntaxes . _) stx]
         [(#%require . _) stx]
         ;; -- expr
         [var:id #'var]
         [(#%plain-lambda formals e ... e*)
          #'(#%plain-lambda formals (instrument e #:nt) ... (instrument e* #:un))]
         [(case-lambda [formals e ... e*] ...)
          #'(case-lambda [formals (instrument e #:nt) ... (instrument e* #:un)] ...)]
         [(if e1 e2 e3)
          #'(if (instrument e1 #:nt) (instrument e2 m) (instrument e3 m))]
         [(begin e ... e*)
          #'(begin (instrument e #:nt) ... (instrument e* m))]
         [(begin0 e ...)
          #'(begin0 (instrument e #:nt) ...)]
         [(let-values ([vars rhs] ...) body ... body*)
          #'(let-values ([vars (instrument rhs #:nt)] ...)
              (instrument body #:nt) ... (instrument body* m))]
         [(letrec-values ([vars rhs] ...) body ... body*)
          #'(letrec-values ([vars (instrument rhs #:nt)] ...)
              (instrument body #:nt) ... (instrument body* m))]
         [(set! var e)
          (eprintf "** set! in expanded code: ~e" (syntax->datum stx))
          #'(set! var (instrument e #:nt))]
         [(quote d) stx]
         [(quote-syntax s) stx]
         [(with-continuation-mark e1 e2 e3)
          #'(with-continuation-mark (instrument e1 #:nt) (instrument e2 #:nt)
              (instrument e3 m))]
         ;; #%plain-app -- see above
         [(#%top . _) stx]
         [(#%variable-reference . _) stx]
         [(#%expression e)
          #'(#%expression (instrument e m))]
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
