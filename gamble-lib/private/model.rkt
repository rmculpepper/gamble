;; Copyright (c) 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse "model/analysis.rkt")
         racket/stxparam
         "base.rkt"
         "model/instrument.rkt"
         "model/graph-trace.rkt")
(provide model*)

(define-syntax (model* stx)
  (case (syntax-local-context)
    [(expression)
     (syntax-parse stx
       [(_ e:expr ...)
        (with-syntax ([(aproc gproc csbase)
                       (instrument-model #'(let-values () e ...))])
          #'(model aproc gproc csbase))])]
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
    (define aproc-expr
      #`(syntax-parameterize ((CSBASE (make-rename-transformer
                                       (quote-syntax #,csbase-id))))
          (instrument-top #,tagged-ee)))
    (define gproc-expr
      #`(syntax-parameterize ((CSBASE (make-rename-transformer
                                       (quote-syntax #,csbase-id))))
          (instrument/graph-top #,tagged-ee)))
    (list aproc-expr gproc-expr csbase-id)))

(define next-global-call-site 1)

(define (allocate-call-sites n)
  (begin0 next-global-call-site
    (set! next-global-call-site (+ next-global-call-site n))))
