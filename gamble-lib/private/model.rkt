;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/transformer
                     syntax/id-table
                     "model/traverse.rkt"
                     "model/analysis.rkt")
         racket/stxparam
         racket/class
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
        (define body #'(#%plain-lambda (ctx) (with-ctx ctx e ...)))
        (define ebody (local-expand body 'expression null))
        (with-syntax ([(bindings body) (lift-no-instrument ebody)])
          #'(let bindings (model** body)))])]
    [else #`(#%expression #,stx)]))

(define-syntax (model** stx)
  (syntax-parse stx
    [(_ ee)
     (with-syntax ([(aproc gproc) (instrument-model #'ee)])
       #'(begin-no-instrument (model aproc gproc)))]))

(define-syntax (begin-no-instrument stx)
  (syntax-parse stx
    [(_ e:expr ...)
     (syntax-property #'(begin e ...) no-instrument-property #t)]))

(begin-for-syntax
  ;; lift-no-instrument : Syntax[EE] -> (list (Listof (list Id Syntax[EE])) Syntax[EE])
  (define (lift-no-instrument estx)
    (define bound (make-free-id-table))
    (define (bound? id) (free-id-table-ref bound id #f))
    (define (bound! xs) (for ([x (in-list xs)]) (free-id-table-set! bound x #t)))
    (define bindings null) ;; mutated, (Listof (list Identifier Syntax[Expr]))
    (define (replace stx recur)
      (cond [(syntax-property stx no-instrument-property)
             (define fvs (free-variables stx #:add-lexical? #f #:add bound?))
             (with-syntax ([body stx]
                           [(fv ...) fvs]
                           [(name) (generate-temporaries (list 'noinstr_))]
                           [(tmp ...) (generate-temporaries fvs)])
               (define proc-expr
                 #'(#%plain-lambda (tmp ...)
                     (letrec-syntax ([(fv) (make-variable-like-transformer (quote-syntax tmp))] ...)
                       body)))
               (set! bindings (cons (list #'name proc-expr) bindings))
               #'(#%plain-app name fv ...))]
            [else #f]))
    (define traverse (make-expression-traverser
                      #:bind bound!
                      #:replace replace))
    (define body (traverse estx))
    (list (reverse bindings) body)))

(begin-for-syntax
  ;; (instrument-model (Syntax[EE[(Ctx -> X)]]) : Syntax[Expr[Ctx Addr -> X]]
  (define (instrument-model ee)
    (define fvs (free-variables ee #:add model-definition-id?))
    (define-values (fs fis fgs) (vars->replacements fvs))
    (define-values (tagged-ee call-site-count) (transform+analyze ee))
    (define csbase-id
      (syntax-local-lift-expression
       #`(allocate-call-sites (quote #,call-site-count))))
    (define aproc-expr
      (with-syntax ([(f ...) fs] [(fi ...) fis])
        #`(syntax-parameterize ((CSBASE (make-rename-transformer
                                         (quote-syntax #,csbase-id))))
            (instrument-top #,tagged-ee ((f fi) ...)))))
    (define gproc-expr
      (with-syntax ([(f ...) fs] [(fg ...) fgs])
        #`(syntax-parameterize ((CSBASE (make-rename-transformer
                                         (quote-syntax #,csbase-id))))
            (instrument/graph-top #,tagged-ee ((f fg) ...)))))
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
       [(begin ~! form:expr ...)
        #'(begin (begin-model-definitions form) ...)]
       [(define-values ~! (f:id) rhs:expr)
        (with-syntax ([(f* fi fg) (generate-temporaries #'(f f f))])
          #'(begin
              (define-values (fi fg)
                (instrument-model-function rhs def fi fg))
              (define f* 'unreplaced-model-function)
              (define-syntax f (model-function-transformer (quote-syntax f*)))
              (begin-for-syntax*
                (register-model-definition! (quote-syntax f*)
                                            (quote-syntax fi) (quote-syntax fg)))))])]
    [(_ def:expr ...)
     #'(begin (begin-model-definitions def) ...)]))

(define-syntax (instrument-model-function stx)
  (syntax-parse stx
    [(_ rhs:expr orig-def fi fg)
     (define body #'(#%plain-lambda (ctx) (with-ctx ctx rhs)))
     (define ebody (local-expand body 'expression null))
     (with-syntax ([(bindings body) (lift-no-instrument ebody)])
       #'(let bindings (instrument-model-function* body orig-def fi fg)))]))

(define-syntax (instrument-model-function* stx)
  (syntax-parse stx
    [(_ erhs:expr orig-def fi fg)
     (begin
       (define (check e)
         (syntax-parse e
           #:literal-sets (kernel-literals)
           [(#%lambda ~! . _) (void)]
           [(case-lambda ~! . _) (void)]
           [(let-values ~! bindings body) (check #'body)]
           [(letrec-values ~! bindings body) (check #'body)]
           [_ (raise-syntax-error #f "ill-formed function definition" #'orig-def)]))
       (syntax-parse #'erhs
         #:literal-sets (kernel-literals)
         [(#%plain-lambda (ctx) body) (check #'body)]))
     (with-syntax ([(aproc gproc) (instrument-model #'erhs)])
       #'(values (wrap-linker aproc (lambda () fi))
                 (wrap-linker gproc (lambda () fg))))]))

(define ((wrap-linker proc get-fi) ctx addr)
  (define fi (get-fi))
  (define fbox (box #f))
  (hash-set! (send ctx get-linker) fi fbox)
  (set-box! fbox (proc ctx addr))
  fbox)

(begin-for-syntax
  (define (model-function-transformer f*-id)
    (make-variable-like-transformer
     (lambda (ref-id)
       (unless (syntax-parameter-value #'within-model?)
         (raise-syntax-error #f "used out of model context" ref-id))
       f*-id))))
