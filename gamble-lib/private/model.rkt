;; Copyright (c) 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/transformer
                     "model/analysis.rkt")
         racket/stxparam
         "base.rkt"
         "model/instrument.rkt"
         "model/graph-trace.rkt")
(provide model*
         begin-model-definitions)

(define-syntax (model* stx)
  (case (syntax-local-context)
    [(expression)
     (syntax-parse stx
       [(_ e:expr ...)
        (with-syntax ([(aproc gproc)
                       (instrument-model #'(let-values () e ...))])
          #'(model aproc gproc))])]
    [else #`(#%expression #,stx)]))

(begin-for-syntax
  ;; (instrument-model Expr[X]) : Expr[Ctx Addr -> X]
  (define (instrument-model body-expr [check void])
    (define ctx-proc-expr
      #`(#%plain-lambda (ctx) (with-ctx ctx #,body-expr)))
    (define ee (local-expand ctx-proc-expr 'expression null))
    (check ee)
    (define-values (fs fis fgs fvs)
      (vars->replacements (free-variables ee model-definition-id?)))
    (define (lift e fvs)
      (with-syntax ([(fv ...) fvs] [(tmp ...) (generate-temporaries fvs)] [e e])
        (define lifted
          (syntax-local-lift-expression
           #'(#%plain-lambda (tmp ...)
               (letrec-syntax ([fv (make-variable-like-transformer (quote-syntax tmp))] ...)
                 e))))
        #`(#%plain-app #,lifted fv ...)))
    (define-values (tagged-ee call-site-count) (transform-TAG+CS ee))
    (analyze-FUN-EXP tagged-ee)
    (analyze-CALLS-ERP tagged-ee)
    (define csbase-id
      (syntax-local-lift-expression
       #`(allocate-call-sites (quote #,call-site-count))))
    (define aproc-expr
      (lift (with-syntax ([(f ...) fs] [(fi ...) fis])
              #`(syntax-parameterize ((CSBASE (make-rename-transformer
                                               (quote-syntax #,csbase-id))))
                  (instrument-top #,tagged-ee ((f fi) ...))))
            (append fis fvs)))
    (define gproc-expr
      (lift (with-syntax ([(f ...) fs] [(fg ...) fgs])
              #`(syntax-parameterize ((CSBASE (make-rename-transformer
                                               (quote-syntax #,csbase-id))))
                  (instrument/graph-top #,tagged-ee ((f fg) ...))))
            (append fgs fvs)))
    (list aproc-expr gproc-expr)))

(define next-global-call-site 1)

(define (allocate-call-sites n)
  (begin0 next-global-call-site
    (set! next-global-call-site (+ next-global-call-site n))))

;; ------------------------------------------------------------

(define-syntax (begin-model-definitions stx)
  (when (eq? (syntax-local-context) 'expression)
    (raise-syntax-error #f "cannot be used in expression context" stx))
  (syntax-parse stx
    [(_ def:expr)
     (define edef (local-expand #'def (syntax-local-context) #f))
     (syntax-parse edef
       #:literal-sets (kernel-literals)
       [(define-values ~! (f:id) rhs:expr)
        (with-syntax ([(f* fi fg) (generate-temporaries #'(f f f))])
          #'(begin
              (define-values (fi fg)
                (instrument-model-function rhs def))
              (define f* 'unreplaced-model-function)
              (define-syntax f (model-function-transformer (quote-syntax f*)))
              (begin-for-syntax*
                (register-model-definition! (quote-syntax f*)
                                            (quote-syntax fi) (quote-syntax fg)))))])]
    [(_ def:expr ...)
     #'(begin (begin-model-definitions def) ...)]))

(define-syntax (instrument-model-function stx)
  (syntax-parse stx
    [(_ rhs:expr orig-def)
     (define (check e)
       (syntax-parse e
         #:literal-sets (kernel-literals)
         [(#%lambda ~! . _) (void)]
         [(case-lambda ~! . _) (void)]
         [(let-values ~! bindings body) (check #'body)]
         [(letrec-values ~! bindings body) (check #'body)]
         [_ (raise-syntax-error #f "ill-formed function definition" #'orig-def)]))
     (with-syntax ([(aproc gproc) (instrument-model #'rhs check)])
       #'(values aproc gproc))]))

(begin-for-syntax
  (define (model-function-transformer f*-id)
    (make-variable-like-transformer
     (lambda (ref-id)
       (unless (syntax-parameter-value #'within-model?)
         (raise-syntax-error #f "used out of model context" ref-id))
       f*-id))))
