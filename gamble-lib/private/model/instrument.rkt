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
         "../addr.rkt"
         "../base.rkt")
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

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

(define-syntax-parameter CSBASE
  (lambda (stx) (wrong-syntax stx "used out of context")))
(define-syntax-parameter ADDR
  (lambda (stx) (wrong-syntax stx "used out of context")))

(define-syntax-rule (declare-instrumented f fimpl arity)
  (begin-for-syntax*
    (register-instrumented! (quote-syntax f) (quote-syntax fimpl) (quote arity))))

;; ============================================================
;; Static protocol

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

;; ============================================================
;; Instrumenter

(define-syntax (instrument-top stx)
  ;; Note: must be kept in sync with `with-ctx` and `model` expressions.
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ (#%plain-lambda (ctx)
          (let-values ctx-bindings body:expr))
        ((f fi) ...))
     #:with ([(ctx-sample _ ... ctx-sample/addr) _]) #'ctx-bindings
     (with-syntax ([(ft ...) (generate-temporaries #'(f ...))])
       #'(#%plain-lambda (ctx addr)
           (let-values ([(ft) (fi ctx addr)] ...)
             (let-values ctx-bindings
               (syntax-parameterize ((ADDR (make-rename-transformer
                                            (quote-syntax addr))))
                 (letrec-syntaxes ([(instrument)
                                    (make-instrument (quote-syntax instrument)
                                                     (quote-syntax ctx-sample)
                                                     (quote-syntax ctx-sample/addr)
                                                     (syntax->list (quote-syntax (f ...)))
                                                     (syntax->list (quote-syntax (ft ...))))])
                   (instrument body)))))))]))

(begin-for-syntax
  ;; Definitions
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
    (pattern (kw:case-lambda [(var:id ...) body:expr ...] ...)))

  ;; Tooltip
  (define (add-app-tooltip! ttb stx msg)
    (when stx
      (define pos (syntax-position stx))
      (define span (syntax-span stx))
      (define tt
        (and pos span
             ;; offset positions by -1 to work around DrRacket bug (?)
             (vector stx (+ pos -1) (+ pos span -1) (string-append "* " msg))))
      ;; (log-instr-info "tooltip(~s) for ~s" (if (syntax-original? stx) 'Y 'N) stx)
      (when tt (set-box! ttb (cons tt (unbox ttb))))))

  (define (relocate stx loc-stx)
    (if (identifier? stx) stx (datum->syntax stx (syntax-e stx) loc-stx stx)))

  ;; ----------------------------------------
  ;; Instrumenter

  ;; make-instrument : Id Id Id -> (Syntax[ExpandedForm] -> Syntax[Form])
  ;; PRE: syntax is fully-expanded expression, tagged, and analyzed
  ;; PRE: result is used in context of binding of ADDR and CALL-SITE-BASE
  (define (make-instrument instrument-id sample-id sample/addr-id fs fts)
    (define replace (make-free-id-table))
    (for ([f-id (in-list fs)] [ft-id (in-list fts)])
      (free-id-table-set! replace f-id ft-id))
    (lambda (istx)
      (define stx (syntax-case istx () [(_ ee) #'ee]))
      (define/with-syntax instrument instrument-id)
      (define/with-syntax ee stx)
      (define result
        (syntax-parse stx
          #:literal-sets (kernel-literals)
          ;; ----------------------------------------
          ;; Expressions
          [var:id (or (free-id-table-ref replace #'var #f) #'var)]
          [(#%plain-lambda ~! formals e ...)
           ;; Receive address using dynamic protocol
           #'(#%plain-lambda formals
               (with-get-ADDR addr
                 (syntax-parameterize ((ADDR (make-rename-transformer (quote-syntax addr))))
                   (instrument e) ...)))]
          [(case-lambda ~! [formals e ...] ...)
           ;; Receive address using dynamic protocol
           #'(case-lambda
               [formals
                (with-get-ADDR addr
                  (syntax-parameterize ((ADDR (make-rename-transformer (quote-syntax addr))))
                    (instrument e) ...))]
               ...)]
          [(if ~! e1 e2 e3)
           #'(if (instrument e1) (instrument e2) (instrument e3))]
          [(begin ~! e ...)
           #'(begin (instrument e) ...)]
          [(begin0 ~! e0 e ...)
           #'(begin0 (instrument e0) (instrument e) ...)]
          [(let-values ~! ([vars rhs] ...) body ...)
           ;; HACK: okay to turn let-values into intdef (letrec) because
           ;; already expanded, thus "alpha-renamed", so no risk of capture
           #'(let-values ()
               (instrument (define-values vars rhs)) ...
               (#%expression (instrument body)) ...)]
          [(letrec-values ~! ([vars rhs] ...) body ...)
           #'(let-values ()
               (instrument (define-values vars rhs)) ...
               (#%expression (instrument body)) ...)]
          [(quote ~! d) stx]
          [(quote-syntax . _) stx]
          [(with-continuation-mark ~! e1 e2 e3)
           #'(with-continuation-mark (instrument e1) (instrument e2)
               (instrument e3))]
          [(#%top . _) stx]
          [(#%variable-reference . _) stx]
          [(#%expression ~! e)
           #'(#%expression (instrument e))]
          ;; --------------------
          ;; Applications
          [(#%plain-app) #'ee]
          [(#%plain-app sample:id dist:expr)
           #:when (free-identifier=? #'sample sample-id)
           (with-syntax ([cs (CALL-SITE stx)])
             (with-syntax ([addr-expr #'(addr-add-call ADDR (+ CSBASE cs))])
               #`(#%plain-app #,sample/addr-id (instrument dist) '#f addr-expr)))]
          [(#%plain-app sample:id dist:expr tag:expr)
           #:when (free-identifier=? #'sample sample-id)
           (with-syntax ([cs (CALL-SITE stx)])
             (with-syntax ([addr-expr #'(addr-add-call ADDR (+ CSBASE cs))])
               #`(#%plain-app #,sample/addr-id (instrument dist) (instrument tag) addr-expr)))]
          [(#%plain-app fun0:id arg ...)
           (define/with-syntax fun (or (free-id-table-ref replace #'fun0 #f) #'fun0))
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
                    (tt-fun-type! "function without stochastic effects")
                    #'(#%plain-app fun (instrument arg) ...)]
                   [(not (app-calls-erp? stx))
                    ;; analysis says doesn't call ERP (superset of prev case) => doesn't need address
                    (log-app-type "STATIC app (!APP-CALLS-ERP)")
                    (tt-fun-type! "function without stochastic effects (analyzed)")
                    #'(#%plain-app fun (instrument arg) ...)]
                   [(instrumented-impl #'fun (length (syntax->list #'(arg ...))))
                    => (lambda (fimpl)
                         ;; instrumented function with right arity => use static protocol
                         (log-app-type "STATIC app (instrumented)")
                         (tt-fun-type! "instrumented function (static protocol)")
                         (with-syntax ([cs (CALL-SITE stx)]
                                       [fimpl (syntax-property fimpl 'disappeared-use #'fun)])
                           #'(#%plain-app fimpl (addr-add-call ADDR (+ CSBASE cs))
                                          (instrument arg) ...)))]
                   [else
                    ;; unknown, function is varref => use dynamic protocol
                    (log-app-type "DYNAMIC app")
                    (tt-fun-type! "uninstrumented function (dynamic protocol)")
                    (with-syntax ([cs (CALL-SITE stx)]
                                  [(tmp ...) (generate-temporaries #'(arg ...))])
                      #'(let-values ([(tmp) (instrument arg)] ...)
                          (with-put-ADDR (addr-add-call ADDR (+ CSBASE cs))
                            (#%plain-app fun tmp ...))))]))
           (syntax-property result 'mouse-over-tooltips (unbox tooltips))]
          ;; ----------------------------------------
          ;; Definitions
          [d:define-1values-form
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
          [d:define-1values-form
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
          [d:define-values-form
           #'(define-values (d.var ...) (instrument d.rhs))]
          ;; ----------------------------------------
          [_ (raise-syntax-error #f "unhandled syntax in instrument" stx)]))
      (let ([result (relocate result stx)])
        (cond [(eq? result stx) result]
              [(stx-pair? stx) (syntax-track-origin result stx (stx-car stx))]
              [else result])))))
