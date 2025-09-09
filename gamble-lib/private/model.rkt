;; Copyright (c) 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/stxparam
         "base.rkt"
         (for-syntax "model/analysis.rkt")
         "model/instrument.rkt")
(provide model*)

(define-syntax (model* stx)
  (case (syntax-local-context)
    [(expression)
     (syntax-parse stx
       [(_ e:expr ...)
        (with-syntax ([(proc ast (ast-fv ...) csbase)
                       (instrument-model #'(let-values () e ...))])
          #'(model/ast proc (quote ast) (vector ast-fv ...) csbase))])]
    [else #`(#%expression #,stx)]))

(begin-for-syntax
  ;; (instrument-model Expr[X]) : Expr[Ctx Addr -> X]
  (define (instrument-model body-expr)
    (define ctx-proc-expr
      #`(#%plain-lambda (ctx) (with-ctx ctx #,body-expr)))
    (define ee (local-expand ctx-proc-expr 'expression null))
    (define-values (tagged-ee call-site-count) (transform-TAG+CS ee))
    (analyze-FUN-EXP tagged-ee)
    (analyze-CALLS-ERP tagged-ee)
    (define csbase-id
      (syntax-local-lift-expression
       #`(allocate-call-sites (quote #,call-site-count))))
    (define proc-expr
      (syntax-parse tagged-ee
        #:literal-sets (kernel-literals)
        [(#%plain-lambda (ctx) body:expr)
         #`(#%plain-lambda (ctx addr)
             (syntax-parameterize ((CSBASE (make-rename-transformer
                                            (quote-syntax #,csbase-id)))
                                   (ADDR (make-rename-transformer
                                          (quote-syntax addr))))
               (instrument body)))]))
    (define-values (ast ast-fvs) (parse-ast tagged-ee))
    (list proc-expr ast ast-fvs csbase-id)))
