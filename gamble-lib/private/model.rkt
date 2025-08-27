;; Copyright (c) 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/stxparam
         "base.rkt"
         "model/addr.rkt"
         (for-syntax "model/analysis.rkt")
         "model/instrument.rkt")
(provide model*)

(define-syntax (model* stx)
  (case (syntax-local-context)
    [(expression)
     (syntax-parse stx
       [(_ e:expr ...)
        (with-syntax ([(proc ast (ast-fv ...) csbase)
                       (instrument-model
                        #'(#%plain-lambda (ctx)
                            (with-ctx ctx
                              (let-values () e ...))))])
          #'(model/ast proc (quote ast)
                       (vector ast-fv ...)
                       (list->vector (syntax->list #'(ast-fv ...)))
                       csbase))])]
    [else #`(#%expression #,stx)]))

(begin-for-syntax
  ;; (instrument-model Expr[Ctx -> X]) : Expr[Ctx -> X]
  (define (instrument-model stx)
    (define ee (local-expand stx 'expression null))
    (define-values (tagged-ee call-site-count) (transform-TAG+CS ee))
    (analyze-FUN-EXP tagged-ee)
    (analyze-CALLS-ERP tagged-ee)
    (define csbase-id
      (syntax-local-lift-expression
       #`(allocate-call-sites (quote #,call-site-count))))
    (define proc-expr
      #`(syntax-parameterize ((CSBASE (make-rename-transformer
                                       (quote-syntax #,csbase-id))))
          (instrument #,tagged-ee)))
    (define-values (ast ast-fvs) (parse-ast tagged-ee))
    (list proc-expr ast ast-fvs csbase-id)))
