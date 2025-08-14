;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/id-table
                     "analysis.rkt"
                     "known-functions.rkt")
         racket/match
         "../addr.rkt")
(provide describe-all-call-sites
         describe-call-site
         instrumenting-module-begin
         instrumenting-top-interaction
         begin-instrumented
         instrument/local-expand
         (for-syntax analyze)
         instrument
         next-counter)

(begin-for-syntax
  (define-logger instr)

  ;; analyze : Syntax -> Syntax
  (define (analyze stx)
    (define tagged-stx (transform-TAG stx))
    (analyze-FUN-EXP tagged-stx)
    (analyze-CALLS-ERP tagged-stx)
    tagged-stx))

(define-syntax (fresh-call-site stx)
  (syntax-case stx ()
    [(fresh-call-site info)
     #'(#%plain-app next-counter
         (#%plain-app variable-reference->module-source (#%variable-reference))
         info)]))

(begin-for-syntax
  (define (lift-call-site stx)
    (with-syntax ([stx-file (syntax-source stx)]
                  [line (syntax-line stx)]
                  [col (syntax-column stx)]
                  [fun (syntax-case stx (#%plain-app)
                         [(#%plain-app f arg ...) (identifier? #'f) #'f]
                         [_ #f])])
      (syntax-local-lift-expression
       #`(fresh-call-site '(stx-file line col #,stx f))))))

;; ============================================================
;; Instrumenter

(define-syntax (begin-instrumented stx)
  (syntax-case stx ()
    [(_ form)
     (case (syntax-local-context)
       [(expression)
        (with-syntax ([e-form (analyze (local-expand #'form 'expression null))])
          #'(let ([ADDR #f]) (instrument e-form)))]
       [else ;; module, top-level
        (let ([e-form (local-expand #'form (syntax-local-context) #f)])
          (syntax-parse e-form
            #:literal-sets (kernel-literals)
            [(define-values ids rhs)
             #'(define-values ids (begin-instrumented rhs))]
            [(define-syntaxes . _) e-form]
            [(#%require . _) e-form]
            [(#%provide . _) e-form]
            [(#%declare . _) e-form]
            [(module . _) e-form]
            [(module* . _) e-form]
            [(begin form ...)
             #'(begin (begin-instrumented form) ...)]
            [expr
             #'(#%expression (begin-instrumented expr))]))])]
    [(_ form ...)
     #'(begin (begin-instrumented form) ...)]))

;; (instrument expanded-expr) : expr
;; PRE: argument is fully-expanded expression
;; PRE: the ADDR variable is bound to base/start address
(define-syntax (instrument istx)
  (syntax-parse istx
    [(instrument form-to-instrument)
     (define stx #'form-to-instrument)
     (define instrumented
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         ;; Fully-Expanded Programs
         ;; Rewrite applications
         [(#%plain-app) stx]
         [(#%plain-app f e ...)
          #`(instrument-app #,stx)]
         ;; -- module body
         [(#%plain-module-begin form ...)
          #'(#%plain-module-begin (instrument form) ...)]
         ;; -- module-level form
         [(#%provide . _) stx]
         [(begin-for-syntax . _) stx]
         [(module . _) stx]
         [(module* . _)
          (raise-syntax-error #f "cannot instrument submodule" stx)]
         [(#%declare . _) stx]
         ;; -- general top-level form
         [(define-values ids e)
          #`(instrument-definition #,stx)]
         [(define-syntaxes . _) stx]
         [(#%require . _) stx]
         ;; -- expr
         [var:id #'var]
         [(#%plain-lambda formals e ...)
          #'(#%plain-lambda formals
              (with-let-ADDR ADDR
                (instrument e) ...))]
         [(case-lambda [formals e ...] ...)
          #'(case-lambda
              [formals
               (with-let-ADDR ADDR
                 (instrument e) ...)]
              ...)]
         [(if e1 e2 e3)
          #'(if (instrument e1)
                (instrument e2)
                (instrument e3))]
         [(begin e ...)
          #'(begin (instrument e) ...)]
         [(begin0 e0 e ...)
          #'(begin0 (instrument e0) (instrument e) ...)]
         [(let-values ([vars rhs] ...) body ...)
          ;; HACK: okay to turn let-values into intdef (letrec) because
          ;; already expanded, thus "alpha-renamed", so no risk of capture
          #'(let ()
              (instrument (define-values vars rhs)) ...
              (#%expression (instrument body)) ...)]
         [(letrec-values ([vars rhs] ...) body ...)
          #'(let ()
              (instrument (define-values vars rhs)) ...
              (#%expression (instrument body)) ...)]
         [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ...)
          #'(let ()
              (define-syntaxes svars srhs) ...
              (instrument (define-values vvars vrhs)) ...
              (#%expression (instrument body)) ...)]
         [(set! var e)
          #'(set! var (instrument e))]
         [(quote d) stx]
         [(quote-syntax . _) stx]
         [(with-continuation-mark e1 e2 e3)
          #'(with-continuation-mark (instrument e1) (instrument e2)
              (instrument e3))]
         ;; #%plain-app -- see above
         [(#%top . _) stx]
         [(#%variable-reference . _) stx]
         [(#%expression e)
          #'(#%expression (instrument e))]
         [_ (raise-syntax-error #f "unhandled syntax in instrument" stx)]
         ))
     ;; Rearm and track result
     (let ([instrumented (relocate instrumented #'form-to-instrument)])
       (if (eq? stx instrumented)
           stx
           (syntax-track-origin instrumented stx #'instrument)))]))

(begin-for-syntax
  (define (relocate stx loc-stx)
    (if (identifier? stx)
        stx
        (datum->syntax stx (syntax-e stx) loc-stx stx))))

;; ------------------------------------------------------------

(define-syntax (instrument-definition idstx)
  (syntax-parse idstx
    #:literals (define-values #%plain-lambda case-lambda)
    [(_ (define-values (f:id) (#%plain-lambda (arg:id ...) body ...)))
     (with-syntax ([(fimpl) (generate-temporaries #'(f))]
                   [arity (map length (syntax->datum #'((arg ...))))])
       #'(begin (define-values (fimpl)
                  (#%plain-lambda (addr arg ...)
                    (with-ADDR addr (instrument body) ...)))
                (define-values (f)
                  (#%plain-lambda (arg ...)
                    (with-let-ADDR addr (fimpl addr arg ...))))
                (begin-for-syntax*
                  (register-instrumented-fun! (quote-syntax f) (quote-syntax fimpl)
                                              (quote arity)))))]
    [(_ (define-values (var:id) (case-lambda [(arg ...) body ...] ...)))
     (with-syntax ([(fimpl) (generate-temporaries #'(f))]
                   [arity (map length (syntax->datum #'((arg ...) ...)))])
       #'(begin (define-values (fimpl)
                  (case-lambda
                    [(addr arg ...)
                     (with-ADDR addr
                       (instrument body) ...)]
                    ...))
                (define-values (f)
                  (case-lambda
                    [(arg ...)
                     (with-let-ADDR addr
                       (fimpl addr arg ...))]
                    ...))
                (begin-for-syntax*
                  (register-instrumented-fun! (quote-syntax f) (quote-syntax fimpl)
                                              (quote arity)))))]
    [(_ (define-values vars e))
     #'(define-values vars (instrument e))]))

(define-syntax (begin-for-syntax* stx)
  (syntax-case stx ()
    [(_ expr ...)
     #'(define-syntaxes () (begin expr ... (values)))]))

(begin-for-syntax

  ;; instr-fun-table : (free-id-table Id => (cons Id (Listof Nat)))
  (define instr-fun-table
    (make-free-id-table))

  (define (register-instrumented-fun! id id* arity)
    (free-id-table-set! instr-fun-table id (cons id* arity)))

  (define-syntax-class instr-fun
    #:attributes (instr arity)
    (pattern f:id
             #:do [(define p (free-id-table-ref instr-fun-table #'f #f))]
             #:when p
             #:with instr (car p)
             #:attr arity (cdr p))))

;; ------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class nrfo-fun
    (pattern f:id
             #:when (or (eq? (classify-function #'f) 'non-random-first-order)
                        (free-id-table-ref non-random-first-order-funs #'f #f))))
  (define (add-app-tooltip! ttb stx msg)
    (when stx
      (define pos (syntax-position stx))
      (define span (syntax-span stx))
      (define tt
        (and pos span
             ;; offset positions by -1 to work around DrRacket bug (?)
             (vector stx (+ pos -1) (+ pos span -1) (string-append "* " msg))))
      ;; (log-instr-info "tooltip(~s) for ~s" (if (syntax-original? stx) 'Y 'N) stx)
      (when tt (set-box! ttb (cons tt (unbox ttb)))))))

(define-syntax (instrument-app stx)
  (define f-stx (syntax-case stx (#%plain-app) [(#%plain-app f . _) #'f]))
  (define tooltips (box null))
  (define (log-app-type msg)
    (log-instr-info (format "~a for ~s" msg f-stx)))
  (define (tt-fun-type! msg)
    (add-app-tooltip! tooltips f-stx msg))
  (define result
    (syntax-parse stx
      #:literals (#%plain-app)
      ;; non-random first-order non-instrumented => doesn't need address
      [(#%plain-app f:nrfo-fun e ...)
       (log-app-type "STATIC app (NRFO)")
       (tt-fun-type! "non-random first-order function")
       #'(#%plain-app f (instrument e) ...)]
      ;; analysis says doesn't call ERP (superset of prev case) => doesn't need address
      [(#%plain-app f:id e ...)
       #:when (not (app-calls-erp? stx))
       (log-app-type "STATIC app (!APP-CALLS-ERP)")
       (tt-fun-type! "analyzed non-random function (not passing address)")
       #'(#%plain-app f (instrument e) ...)]
      ;; instrumented function with right arity => use static protocol
      [(#%plain-app f:instr-fun e ...)
       #:when (member (length (syntax->list #'(e ...))) (attribute f.arity))
       (log-app-type "STATIC app (instrumented)")
       (tt-fun-type! "instrumented function")
       (with-syntax ([c (lift-call-site stx)]
                     [f-instr (syntax-property #'f.instr 'disappeared-use #'f)])
         #'(#%plain-app f-instr (cons c ADDR) (instrument e) ...))]
      ;; unknown, function is varref => use dynamic protocol
      [(#%plain-app f:id e ...)
       (log-app-type "DYNAMIC app")
       (tt-fun-type! "uninstrumented function (passing address dynamically)")
       (with-syntax ([c (lift-call-site stx)]
                     [(tmp ...) (generate-temporaries #'(e ...))])
         #'(let-values ([(tmp) (instrument e)] ...)
             (with-ADDR (cons c ADDR)
               (#%plain-app f tmp ...))))]
      ;; unknown, function is expr => use dynamic protocol
      [(#%plain-app e ...)
       (with-syntax ([c (lift-call-site stx)]
                     [(tmp ...) (generate-temporaries #'(e ...))])
         #'(let-values ([(tmp) (instrument e)] ...)
             (with-ADDR (cons c ADDR)
               (#%plain-app tmp ...))))]))
  (syntax-property result 'mouse-over-tooltips (unbox tooltips)))
