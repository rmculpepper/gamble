;; Experiment to adapt technique from "Lightweight Implementations of 
;; Probabilistic Programming Languages Via Transformational Compilation"
;; by al. et Goodman to use continuation marks to localize rewriting and
;; avoid changing function signatures.

;; Like pl1, but uses post-expansion instrumentor instead of redefining #%app.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         racket/list
         "private/pl1-context.rkt"
         "private/pl1-prob.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin #%top-interaction)
         (rename-out [instrumenting-module-begin #%module-begin]
                     [instrumenting-top-interaction #%top-interaction])
         (all-from-out "private/pl1-context.rkt")
         (all-from-out "private/pl1-prob.rkt"))

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
     (instrument
      (local-expand #'(#%module-begin form ...) 'module-begin null))]))

(define-syntax (instrumenting-top-interaction stx)
  (syntax-case stx ()
    [(instrumenting-top-interaction . e)
     (parameterize ((instrumenting-module? #f))
       (let ([estx (local-expand #'(#%top-interaction . e) 'top-level #f)])
         (syntax-case estx (begin)
           [(begin form ...)
            #'(begin (instrumenting-top-interaction . form) ...)]
           [form
            (instrument
             (local-expand #'form 'top-level null))])))]))

(begin-for-syntax

 (define stx-insp
   (variable-reference->module-declaration-inspector
    (#%variable-reference)))

 (define (instrument* stxs)
   (map instrument (stx->list stxs)))

 (define (instrument stx)
   (let ([disarmed-stx (syntax-disarm stx stx-insp)])
     (syntax-rearm (instrument/track disarmed-stx) stx)))

 (define (instrument/track stx)
   (let ([stx* (instrument/inner stx)])
     (if (eq? stx stx*)
         stx*
         (syntax-track-origin stx* stx #'instrument))))

 (define (instrument/inner stx)
   ;; (eprintf ">> ~.s\n\n" (syntax->datum stx))
   (syntax-parse stx
     #:literal-sets (kernel-literals)
     ;; Fully-Expanded Programs

     ;; Rewrite applications
     [(#%plain-app e ...)
      (with-syntax ([(e* ...) (instrument* #'(e ...))]
                    [c (if (instrumenting-module?)
                           (next-counter)
                           (- (next-counter)))])
        (when #f
          (eprintf "app ~s = ~a:~a ~.s\n\n"
                   (syntax-e #'c)
                   (or (syntax-line stx) '?)
                   (or (syntax-column stx) '?)
                   (syntax->datum stx)))
        #'(app/call-site 'c e* ...))]

     ;; Just recur through all other forms
     ;; -- module body
     [(#%plain-module-begin form ...)
      (with-syntax ([(form* ...) (instrument* #'(form ...))])
        #'(#%plain-module-begin form* ...))]
     ;; -- module-level form
     [(#%provide . _) stx]
     [(begin-for-syntax . _) stx]
     ;; [(module . _) stx]
     ;; [(module* . _) stx]
     ;; [(#%declare . _) stx]
     ;; -- general top-level form
     [(define-values ids e)
      (with-syntax ([e* (instrument #'e)])
        #'(define-values ids e*))]
     [(define-syntaxes . _) stx]
     [(#%require . _) stx]
     ;; -- expr
     [var:id #'var]
     [(#%plain-lambda formals e ...)
      (with-syntax ([(e* ...) (instrument* #'(e ...))])
        #'(#%plain-lambda formals e* ...))]
     [(case-lambda [formals e] ...)
      (with-syntax ([(e* ...) (instrument* #'(e ...))])
        #'(case-lambda [formals e*] ...))]
     [(if e ...)
      (with-syntax ([(e* ...) (instrument* #'(e ...))])
        #'(if e* ...))]
     [(begin e ...)
      (with-syntax ([(e* ...) (instrument* #'(e ...))])
        #'(begin e* ...))]
     [(begin0 e ...)
      (with-syntax ([(e* ...) (instrument* #'(e ...))])
        #'(begin0 e ...))]
     [(let-values ([vars rhs] ...) body ...)
      (with-syntax ([(rhs* ...) (instrument* #'(rhs ...))]
                    [(body* ...) (instrument* #'(body ...))])
        #'(let-values ([vars rhs*] ...) body* ...))]
     [(letrec-values ([vars rhs] ...) body ...)
      (with-syntax ([(rhs* ...) (instrument* #'(rhs ...))]
                    [(body* ...) (instrument* #'(body ...))])
        #'(letrec-values ([vars rhs*] ...) body* ...))]
     [(set! var e)
      (begin
        (eprintf "** set! in expanded code: ~e" (syntax->datum stx))
        (with-syntax ([e* (instrument #'e)])
          #'(set! var e*)))]
     [(quote d) stx]
     [(quote-syntax s) stx]
     [(with-continuation-mark e ...)
      (with-syntax ([(e* ...) (instrument* #'(e ...))])
        #'(with-continuation-mark e* ...))]
     ;; #%plain-app -- see above
     [(#%top . _) stx]
     [(#%variable-reference . _) stx]
     [(#%expression e)
      (with-syntax ([e* (instrument #'e)])
        #'(#%expression e*))]
     [_
      (raise-syntax-error #f "unhandled syntax" stx)]
     ))
 )

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
