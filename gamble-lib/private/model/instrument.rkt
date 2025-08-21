;; Copyright (c) 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/id-table
                     syntax/stx
                     syntax/parse/experimental/template
                     "analysis.rkt"
                     "known-functions.rkt")
         racket/match
         racket/stxparam
         "addr.rkt"
         "../base.rkt")
(provide (all-defined-out))

(begin-for-syntax
  (define-logger instr)

  (define-template-metafunction ~track
    (syntax-parser
      [(_ new-term (~and old-term (old-kw:id . _)))
       (syntax-track-origin #'new-term #'old-term #'old-kw)]
      [(_ new-term old-term old-kw)
       (syntax-track-origin #'new-term #'old-term #'old-kw)])))

(define-syntax (lift stx)
  (syntax-parse stx
    [(_ e:expr) (syntax-local-lift-expression #'e)]))

(define-syntax-rule (begin-for-syntax* expr ...)
  (define-syntaxes () (begin expr ... (values))))


;; ============================================================
;; Call site indexing

(define next-global-call-site 1)

(define (allocate-call-sites n)
  (begin0 next-global-call-site
    (set! next-global-call-site (+ next-global-call-site n))))

(define-syntax-parameter CSBASE
  (lambda (stx) (wrong-syntax stx "used out of context")))
(define-syntax-parameter ADDR
  (lambda (stx) (wrong-syntax stx "used out of context")))

(begin-for-syntax
  ;; call-site-counter : (Parameterof Nat)
  (define call-site-counter (make-parameter 'uninitialized))

  ;; next-call-site : -> Nat
  (define (next-call-site)
    (let ([cs (call-site-counter)])
      (begin (call-site-counter (add1 cs)) cs))))


;; ============================================================
;; Instrumenter

;; (instrument-expr Expr[X]) : Expr[Addr -> X]
(define-syntax (instrument-expr stx)
  (case (syntax-local-context)
    [(expression)
     (syntax-parse stx
       [(_ e:expr)
        (define ee (local-expand #'e 'expression null))
        (define tagged-ee (transform-TAG ee))
        (analyze-FUN-EXP tagged-ee)
        (analyze-CALLS-ERP tagged-ee)
        (define-values (instr-code call-site-count)
          (parameterize ((call-site-counter 0))
            (define istx #`(#%plain-lambda (csbase)
                             (syntax-parameterize ((CSBASE (make-rename-transformer
                                                            (quote-syntax csbase))))
                               (#%plain-lambda (addr)
                                 (syntax-parameterize ((ADDR (make-rename-transformer
                                                              (quote-syntax addr))))
                                   (instrument #,tagged-ee))))))
            (define-values (_instr-ee instr-code)
              (syntax-local-expand-expression istx #t))
            (values instr-code (call-site-counter))))
        #`(#,instr-code (lift (allocate-call-sites (quote #,call-site-count))))])]
    [else #`(#%expression #,stx)]))

;; (instrument ExpandedExpr) : Expr
;; PRE: argument is fully-expanded expression, tagged, and analyzed
;; PRE: result is used in context of binding of ADDR and CALL-SITE-BASE
(define-syntax (instrument istx)
  (syntax-parse istx
    [(instrument-id ee)
     (define stx #'ee)
     (define result
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         ;; Fully-Expanded Programs
         ;; Rewrite applications
         [(#%plain-app) #'ee]
         [(#%plain-app f e ...)
          #'(instrument-app ee)]
         ;; -- general top-level form
         [(define-values ids e)
          #'(instrument-definition ee)]
         [(define-syntaxes . _) stx]
         ;; -- expr
         [var:id #'var]
         [(#%plain-lambda formals e ...)
          ;; Receive address using dynamic protocol
          #'(#%plain-lambda formals
              (with-get-ADDR addr
                (syntax-parameterize ((ADDR (make-rename-transformer (quote-syntax addr))))
                  (instrument e) ...)))]
         [(case-lambda [formals e ...] ...)
          ;; Receive address using dynamic protocol
          #'(case-lambda
              [formals
               (with-get-ADDR addr
                 (syntax-parameterize ((ADDR (make-rename-transformer (quote-syntax addr))))
                   (instrument e) ...))]
              ...)]
         [(if e1 e2 e3)
          #'(if (instrument e1) (instrument e2) (instrument e3))]
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
     (let ([result (relocate result stx)])
       (if (eq? result stx) result (syntax-track-origin result stx (stx-car istx))))]))

(begin-for-syntax
  (define (relocate stx loc-stx)
    (if (identifier? stx)
        stx
        (datum->syntax stx (syntax-e stx) loc-stx stx))))

;; ------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class define-values-form
    #:literals (define-values)
    (pattern (kw:define-values (var:id ...) rhs:expr)))
  (define-syntax-class define-1values-form
    #:literals (define-values)
    (pattern (kw:define-values (var:id) rhs:expr)))
  (define-syntax-class plain-lambda-expr
    #:literals (#%plain-lambda)
    (pattern (kw:#%plain-lambda (var:id ...) body:expr ...)))
  (define-syntax-class case-lambda-expr
    #:literals (case-lambda)
    (pattern (kw:case-lambda [(var:id ...) body:expr ...] ...))))

(define-syntax (instrument-definition idstx)
  (syntax-parse idstx
    [(_ d:define-1values-form)
     #:with rhs:plain-lambda-expr #'d.rhs
     (with-syntax ([(fimpl) (generate-temporaries #'(d.var))]
                   [arity (map length (syntax->datum #'((rhs.var ...))))])
       #'(begin (define-values (fimpl)
                  (~track (#%plain-lambda (addr rhs.var ...)
                            (syntax-parameterize ((ADDR (make-rename-transformer
                                                         (quote-syntax addr))))
                              (instrument rhs.body) ...))
                          rhs rhs.kw))
                (define-values (d.var)
                  (#%plain-lambda (rhs.var ...)
                    (with-get-ADDR addr (fimpl addr rhs.var ...))))
                (declare-instrumented d.var fimpl arity)))]
    [(_ d:define-1values-form)
     #:with rhs:case-lambda-expr #'d.rhs
     (with-syntax ([(fimpl) (generate-temporaries #'(d.var))]
                   [arity (map length (syntax->datum #'((rhs.var ...) ...)))])
       #'(begin (define-values (fimpl)
                  (~track (case-lambda
                            [(addr rhs.var ...)
                             (syntax-parameterize ((ADDR (make-rename-transformer
                                                          (quote-syntax addr))))
                               (instrument rhs.body) ...)]
                            ...)
                          rhs rhs.kw))
                (define-values (d.var)
                  (case-lambda
                    [(rhs.var ...) (with-get-ADDR addr (fimpl addr rhs.var ...))]
                    ...))
                (declare-instrumented d.var fimpl arity)))]
    [(_ d:define-values-form)
     #'(~track (define-values (d.var ...) (instrument d.rhs)) d d.kw)]))

(define-syntax-rule (declare-instrumented f fimpl arity)
  (begin-for-syntax*
    (register-instrumented! (quote-syntax f) (quote-syntax fimpl) (quote arity))))

(begin-for-syntax

  ;; instr-fun-table : (free-id-table Id => (cons Id (Listof Nat)))
  (define instr-fun-table
    (make-free-id-table))

  (define (register-instrumented! id id* arity)
    (free-id-table-set! instr-fun-table id (cons id* arity)))

  (define (instrumented-impl f-id argn)
    (cond [(free-id-table-ref instr-fun-table f-id #f)
           => (lambda (fimpl+arity)
                (and (member argn (cdr fimpl+arity)) (car fimpl+arity)))]
          [else #f])))

;; ------------------------------------------------------------

(begin-for-syntax
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

(define-syntax (instrument-app istx)
  (define stx (syntax-case istx () [(_ app) #'app]))
  (syntax-parse stx
    #:literals (#%plain-app)
    [(#%plain-app fun:id arg ...)
     (define tooltips (box null))
     (define (log-app-type msg)
       (log-instr-info "~a for ~s" msg #'fun))
     (define (tt-fun-type! msg)
       (add-app-tooltip! tooltips #'fun msg))
     ;(log-instr-info "- class ~s for ~s" (function-may-call-erp? #'fun) #'fun)
     ;(log-instr-info "- CALLS-ERP is ~s for ~s" (app-calls-erp? stx) stx)
     (define result
       (cond [(not (function-may-call-erp? #'fun))
              ;; non-random first-order non-instrumented => doesn't need address
              (log-app-type "STATIC app (NRFO)")
              (tt-fun-type! "non-random first-order function")
              #'(#%plain-app fun (instrument arg) ...)]
             [(not (app-calls-erp? stx))
              ;; analysis says doesn't call ERP (superset of prev case) => doesn't need address
              (log-app-type "STATIC app (!APP-CALLS-ERP)")
              (tt-fun-type! "analyzed non-random function (not passing address)")
              #'(#%plain-app fun (instrument arg) ...)]
             [(instrumented-impl #'fun (length (syntax->list #'(arg ...))))
              => (lambda (fimpl)
                   ;; instrumented function with right arity => use static protocol
                   (log-app-type "STATIC app (instrumented)")
                   (tt-fun-type! "instrumented function")
                   (with-syntax ([cs (next-call-site)]
                                 [fimpl (syntax-property fimpl 'disappeared-use #'fun)])
                     #'(#%plain-app fimpl (addr-add-call ADDR (+ CSBASE cs))
                                    (instrument arg) ...)))]
             [else
              ;; unknown, function is varref => use dynamic protocol
              (log-app-type "DYNAMIC app")
              (tt-fun-type! "uninstrumented function (passing address dynamically)")
              (with-syntax ([cs (next-call-site)]
                            [(tmp ...) (generate-temporaries #'(arg ...))])
                #'(let-values ([(tmp) (instrument arg)] ...)
                    (with-put-ADDR (addr-add-call ADDR (+ CSBASE cs))
                      (#%plain-app fun tmp ...))))]))
     (let ([result (syntax-track-origin result stx (stx-car stx))])
       (syntax-property result 'mouse-over-tooltips (unbox tooltips)))]))
