;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         "context.rkt")
(provide describe-all-call-sites
         describe-call-site
         instrumenting-module-begin
         instrumenting-top-interaction
         instrument)

#|
A CallSite is a Nat.

Call-site index determined dynamically, so distinct modules get
distinct call-site indexes.
|#

;; call-site-counter : Nat
(define call-site-counter 0)

;; call-site-table : hash[nat => sexpr describing source]
(define call-site-table (make-hash))

;; next-counter : (List VariableReference any any any) : -> Nat
(define (next-counter src-info)
  (set! call-site-counter (add1 call-site-counter))
  (hash-set! call-site-table call-site-counter src-info)
  call-site-counter)

(define-syntax (fresh-call-site stx)
  (syntax-case stx ()
    [(fresh-call-site call)
     (with-syntax ([stx-file (syntax-source #'call)]
                   [line (syntax-line #'call)]
                   [col (syntax-column #'call)])
       #'(next-counter
          (cons (variable-reference->module-source (#%variable-reference))
                '(stx-file line col call))))]))

(define (describe-all-call-sites)
  (for ([i (in-range 1 (add1 call-site-counter))])
    (describe-call-site i)))

(define (describe-call-site n)
  (cond [(hash-ref call-site-table n #f)
         => (lambda (info)
              (match info
                [(list mod src line col stx)
                 (let ([mod (if (path? mod) (path->string mod) mod)]
                       [src (if (path? src) (path->string src) src)])
                   (printf "call site ~s: ~a:~a:~a\n"
                           n
                           (or src (and (not line) (not col) mod) "?")
                           (or line "?")
                           (or col "?")))]))]
        [else (printf "call site ~s: no info available" n)]))

;; ----

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
     (let ([estx (local-expand #'(#%top-interaction . e) 'top-level #f)])
       (syntax-case estx (begin)
         [(begin form ...)
          #'(begin (instrumenting-top-interaction . form) ...)]
         [form
          (with-syntax ([e-form (local-expand #'form 'top-level null)])
            #'(instrument e-form #:un))]))]))

(begin-for-syntax
 ;; Need privileged inspector to rewrite expanded code.
 (define stx-insp
   (variable-reference->module-declaration-inspector
    (#%variable-reference))))

;; (instrument expanded-form Mode)
;; where Mode is one of:
;;    #:nt   - non-tail wrt enclosing lambda
;;    #:tail - tail wrt enclosing lambda
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
                   (when (eq? classification 'unknown)
                     (eprintf "## ~s\n" (syntax-e #'f))))
                 (with-syntax ([c (syntax-local-lift-expression
                                   #`(fresh-call-site #,stx))])
                   #'(app/call-site* m c f (instrument e #:nt) ...))])]
         [(#%plain-app f e ...)
          (with-syntax ([c (syntax-local-lift-expression
                            #`(fresh-call-site #,stx))])
            (when #f
              (eprintf "## ~s\n" (syntax->datum #'f)))
            #'(app/call-site* m c (instrument f #:nt) (instrument e #:nt) ...))]
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
          #'(#%plain-lambda formals (instrument e #:nt) ... (instrument e* #:tail))]
         [(case-lambda [formals e ... e*] ...)
          #'(case-lambda [formals (instrument e #:nt) ... (instrument e* #:tail)] ...)]
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
         [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ... body*)
          #'(letrec-syntaxes+values ([svars srhs] ...) ([vvars (instrument vrhs #:nt)] ...)
              (instrument body #:nt) ... (instrument body* m))]
         [(set! var e)
          ;; (eprintf "** set! in expanded code: ~e" (syntax->datum stx))
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

(define-syntax (app/call-site* stx)
  (syntax-case stx ()
    [(app/call-site* mode call-site f arg ...)
     (with-syntax ([(tmp-f)
                    (generate-temporaries #'(f))]
                   [(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #`(let ([c call-site] [tmp-f f] [tmp-arg arg] ...)
           (app/call-site** mode c tmp-f tmp-arg ...)))]))

(define-syntax (app/call-site** stx)
  (syntax-case stx ()
    [(app/call-site** m c f arg ...)
     #'(parameterize ((the-context (cons c (the-context))))
         (#%app f arg ...))]))

;; ----

;; A function is "safe" if no ERP is ever executed in the context of a call
;; to that function. A "safe" function does not need an WCM around it for
;; address tracking.

;; TODO: add common non-kernel Racket functions
;; TODO: static analysis for locally-defined functions
(begin-for-syntax
 ;; classify-function : id -> (U 'safe 'unsafe 'unknown)
 (define (classify-function f-id)
   (let ([b (identifier-binding f-id)])
     (if (list? b)
         (let ([def-mpi (car b)]
               [def-name (cadr b)])
           (let-values ([(def-mod def-relto) (module-path-index-split def-mpi)])
             (if (equal? def-mod ''#%kernel)
                 (if (memq def-name HO-kernel-procedures)
                     'unsafe
                     'safe)
                 'unknown)))
         'unknown)))
 ;; functions defined in kernel, known to be unsafe
 (define HO-kernel-procedures
   '(;; omit indirect HO functions, like make-struct-type, chaperone-*, impersonate-*
     apply
     map
     for-each
     andmap
     ormap
     call-with-values
     call-with-escape-continuation
     call/ec
     call-with-current-continuation
     call/cc
     call-with-continuation-barrier
     call-with-continuation-prompt
     call-with-composable-continuation
     abort-current-continuation
     call-with-semaphore
     call-with-semaphore/enable-break
     call-with-immediate-continuation-mark
     time-apply
     dynamic-wind
     hash-map
     hash-for-each
     call-with-input-file
     call-with-output-file
     with-input-from-file
     with-output-to-file
     eval
     eval-syntax
     call-in-nested-thread
     ))
 )

#|
To get list of '#%kernel exports:
(define (simplify e) (match e [`(just-meta ,n (rename '#%kernel ,x ,_)) x] [_ #f]))
(define knames
  (filter symbol?
          (map simplify
               (cdr (syntax->datum (expand '(require (rename-in '#%kernel))))))))
|#
