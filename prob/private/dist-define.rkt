;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     syntax/parse/experimental/eh
                     racket/syntax)
         racket/contract
         racket/flonum
         "dist.rkt")
(provide define-dist-type
         define-fl-dist-type)

(begin-for-syntax

  (define-syntax-class param-spec
    (pattern [param:id ctc:expr]))

  (define-splicing-syntax-class prob-functions
    (pattern (~seq (~or (~once (~seq #:pdf pdf-fun:expr))
                        (~once (~seq #:sample sample-fun:expr))
                        (~optional (~seq #:cdf cdf-fun:expr))
                        (~optional (~seq #:inv-cdf inv-cdf-fun:expr)))
                   ...)))

  (define-eh-alternative-set stat-options
    (pattern
     (~or (~optional (~seq #:support support:expr))
          (~optional (~seq #:enum enum:expr))
          (~optional (~seq #:mean mean:expr))
          (~optional (~seq #:median median:expr))
          (~optional (~seq #:modes modes:expr))
          (~optional (~seq #:variance variance:expr))
          (~optional (~seq #:conjugate conj-fun:expr))
          (~optional (~seq #:Denergy Denergy-fun:expr))
          (~optional (~seq #:drift drift-fun:expr)))))
  )

(define-syntax (define-dist-type stx)
  (syntax-parse stx
    [(define-dist-type name-dist:id (p:param-spec ...)
       f:prob-functions
       (~or (~eh-var o stat-options)
            (~optional (~seq #:guard guard-fun:expr))
            (~optional (~and no-provide #:no-provide))
            (~optional (~seq #:extra [extra-clause ...])
                       #:defaults ([(extra-clause 1) '()])))
       ...)
     (unless (regexp-match? #rx"-dist$" (symbol->string (syntax-e #'name-dist)))
       (raise-syntax-error #f "name does not end with `-dist'" stx #'name-dist))
     (with-syntax ([make-name-dist (format-id #'name-dist "make-~a" #'name-dist)]
                   [(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name-dist "~a-~a" #'name-dist param))])
       (quasitemplate
        (begin
          (struct name-dist (p.param ...)
                  #:extra-constructor-name make-name-dist
                  (?? (?@ #:guard guard-fun))
                  #:methods gen:dist
                  [(define (*pdf d x log?)
                     (f.pdf-fun (get-param d) ... x log?))
                   (define (*sample d)
                     (f.sample-fun (get-param d) ...))
                   (define (*cdf d x log? 1-p?)
                     (?? (f.cdf-fun (get-param d) ... x log? 1-p?)
                         (error 'dist-cdf "not defined for distribution\n  given: ~e" d)))
                   (define (*inv-cdf d x log? 1-p?)
                     (?? (f.inv-cdf-fun (get-param d) ... x log? 1-p?)
                         (error 'dist-inv-cdf "not defined for distribution\n  given: ~e" d)))
                   (define (*type d) 'name-dist)
                   (define (*params d) (vector (get-param d) ...))
                   (?? (define (*enum d) (let ([p.param (get-param d)] ...) o.enum)))
                   (?? (define (*support d) (let ([p.param (get-param d)] ...) o.support)))
                   (?? (define (*mean d) (let ([p.param (get-param d)] ...) o.mean)))
                   (?? (define (*median d) (let ([p.param (get-param d)] ...) o.median)))
                   (?? (define (*modes d) (let ([p.param (get-param d)] ...) o.modes)))
                   (?? (define (*variance d) (let ([p.param (get-param d)] ...) o.variance)))
                   (?? (define (*Denergy d x . d/dts)
                         (apply (let ([p.param (get-param d)] ...) o.Denergy-fun) x d/dts)))
                   (?? (define (*conj d data-d data)
                         (let ([p.param (get-param d)] ...)
                           (o.conj-fun data-d data))))
                   (?? (define (*drift d value scale-factor)
                         (let ([p.param (get-param d)] ...)
                           (o.drift-fun value scale-factor))))]
                  extra-clause ...
                  #:transparent)
          #,(if (attribute no-provide)
                #'(begin)
                #'(provide (contract-out [struct name-dist ([p.param p.ctc] ...)]))))))]))

;; ----

(begin-for-syntax
  (define-syntax-class name-dist-id
    #:attributes (name)
    (pattern nd:id
             #:do [(define m (regexp-match #rx"^(.*)-dist$" (symbol->string (syntax-e #'nd))))]
             #:fail-unless m "expected identifier matching `<name>-dist'"
             #:with name (format-id #'nd "~a" (cadr m))))
  (define-syntax-class kind-kw
    (pattern #:real)
    (pattern #:nat))
  )

(define-syntax (define-fl-dist-type stx)
  (syntax-parse stx
    [(define-fl-dist-type name-dist:name-dist-id (p:param-spec ...)
       kind:kind-kw
       (~optional (~seq #:guard guard-fun:expr))
       (~and (~seq (~or (~eh-var o stat-options)) ...)
             (~seq more-options ...)))
     (define prefix "m:fl")
     (define name #'name-dist.name)
     (with-syntax ([fl-pdf (format-id name "~a~a-pdf" prefix name)]
                   [fl-cdf (format-id name "~a~a-cdf" prefix name)]
                   [fl-inv-cdf (format-id name "~a~a-inv-cdf" prefix name)]
                   [fl-sample (format-id name "~a~a-sample" prefix name)]
                   [(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name-dist "~a-~a" #'name-dist param))]
                   [convert-in #'exact->inexact]
                   [convert-out
                    (syntax-case #'kind ()
                      [#:real #'values]
                      [#:nat  #'inexact->exact])]
                   [make-name-dist (format-id #'name-dist "make-~a" #'name-dist)])
       (quasitemplate
        (begin
          (define (pdf p.param ... x log?)
            (fl-pdf p.param ... (convert-in x) log?))
          (define (cdf p.param ... x log? 1-p?)
            (fl-cdf p.param ... (convert-in x) log? 1-p?))
          (define (inv-cdf p.param ... x log? 1-p?)
            (convert-out (fl-inv-cdf p.param ... x log? 1-p?)))
          (define (sample p.param ...)
            (convert-out (flvector-ref (fl-sample p.param ... 1) 0)))
          (define-dist-type name-dist (p ...)
            #:pdf pdf #:cdf cdf #:inv-cdf inv-cdf #:sample sample
            #:guard (?? guard-fun (lambda (p.param ... _name) (values (convert-in p.param) ...)))
            more-options ...))))]))
