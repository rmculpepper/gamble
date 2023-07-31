;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/eh
                     racket/syntax)
         racket/match
         racket/flonum
         "base.rkt"
         "density.rkt")
(provide define-dist-struct
         define-real-dist-struct)

(define-syntax struct-fields (syntax-rules ()))
(define-syntax struct-options (syntax-rules ()))

(define-syntax define-struct/e
  (syntax-parser
    [(_ name:id e:expr ...)
     _]))





(begin-for-syntax

  (define-syntax-class name-dist-id
    #:attributes (name)
    (pattern nd:id
             #:do [(define nd-s (symbol->string (syntax-e #'nd)))
                   (define m (regexp-match #rx"^(.*)-dist$" nd-s))]
             #:fail-unless m "expected identifier ending in `-dist`"
             #:with name (format-id #'nd "~a" (cadr m))))

  (define-syntax-class param-spec
    (pattern [param:id pred:expr] #:with conv #'begin)
    (pattern [param:id pred:expr conv:expr]))

  (define-splicing-syntax-class maybe-guard
    #:attributes (guard-fun)
    (pattern (~seq #:guard guard-fun))
    (pattern (~seq) #:attr guard-fun #f))

  #;
  (define-syntax-class base-measure
    (pattern #:counting #:with ddim #''0 #:with ndensity #''#t #:with ldensity #''#f)
    (pattern #:lebesgue #:with ddim #''1 #:with ndensity #''#f #:with ldensity #''#t)
    (pattern #:mixture  #:attr ddim #f   #:attr ndensity #f    #:attr ldensity #f))

  #;
  (define-eh-alternative-set prob-options
    (pattern
     (~or (~once (~seq #:sample sample-fun:expr))
          (~optional (~seq #:density density-fun:expr))
          (~optional (~seq #:pdf pdf-fun:expr))
          (~optional (~seq #:cdf cdf-fun:expr))
          (~optional (~seq #:inv-cdf inv-cdf-fun:expr)))))

  #;
  (define-eh-alternative-set options
    (pattern
     (~or (~optional (~seq #:support support:expr))
          (~optional (~seq #:total-mass total-mass:expr))
          (~optional (~seq #:enum enum:expr))
          (~optional (~seq #:mean mean:expr))
          (~optional (~seq #:median median:expr))
          (~optional (~seq #:modes modes:expr))
          (~optional (~seq #:variance variance:expr))
          (~optional (~seq #:conjugate conj-fun:expr))
          (~optional (~seq #:Denergy Denergy-fun:expr))
          (~optional (~seq #:drift1 drift1-fun:expr))
          (~optional (~seq #:drift-dist drift-dist-fun:expr))
          (~optional (~seq #:provide provide-clause:expr))))))

(define-syntax define-dist-struct
  (syntax-parser
    [(_ nd:name-dist-id (p:param-spec ...)
        g:maybe-guard
        more ...)
     #'(begin
         (struct nd (p.param ...)
           #:transparent
           #:guard (lambda (p.param ... _name)
                     (define (bad who)
                       (maker-error 'nd '(p.param ...) '(p.pred ...) who p.param ...))
                     (unless (p.pred p.param) (bad 'p.param)) ...
                     (~? (g.guard-fun p.param ... _name)
                         (values p.param ...)))
           more ...))]))




(define-syntax (define-real-dist-struct stx)
  (define-syntax-class flparam
    (pattern [param:id pred:expr]
             #:with mkconv #'exact->inexact
             #:with ref #'param)
    (pattern [param:id pred:expr #:exact]
             #:with mkconv #'begin
             #:with ref #'(exact->inexact param)))
  (define-syntax-class kind-kw
    (pattern #:real #:with convert #'begin #:with measure #'#:lebesgue)
    (pattern #:nat  #:with convert #'inexact->exact #:with measure #'#:counting))
  (define-splicing-syntax-class prefix-clause
    (pattern (~seq #:prefix pfx:id))
    (pattern (~seq) #:attr pfx #f))
  (syntax-parse stx
    [(define-fl-dist-type nd:name-dist-id (p:flparam ...)
       kind:kind-kw
       pc:prefix-clause
       (~optional (~seq #:guard guard-expr:expr))
       (~optional (~seq #:dist-methods [dist-method:expr ...]))
       (~optional (~seq #:real-methods [real-method:expr ...]))
       more ...)
     (with-syntax ([(sample-def auto-real-defs)
                    (cond [(attribute pc.pfx)
                           (define prefix #'pc.pfx)
                           (with-syntax ([fl-pdf
                                          (format-id prefix "~a-pdf" prefix)]
                                         [fl-cdf
                                          (format-id prefix "~a-cdf" prefix)]
                                         [fl-invcdf
                                          (format-id prefix "~a-inv-cdf" prefix)]
                                         [fl-sample
                                          (format-id prefix "~a-sample" prefix)])
                             #'((define (-sample self)
                                  (match-define (nd p.param ...) self)
                                  (kind.convert (flvector-ref (fl-sample p.ref ... 1) 0)))
                                (begin
                                  (define (-pdf self x log?)
                                    (match-define (nd p.param ...) self)
                                    (fl-pdf p.ref ... (exact->inexact x) log?))
                                  (define (-cdf self x log? 1-p?)
                                    (match-define (nd p.param ...) self)
                                    (fl-cdf p.ref ... (exact->inexact x) log? 1-p?))
                                  (define (-invcdf self x log? 1-p?)
                                    (match-define (nd p.param ...) self)
                                    (kind.convert
                                     (fl-invcdf p.ref ... (exact->inexact x) log? 1-p?))))))]
                          [else
                           #'((begin) (begin))])]
                   [(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name-dist "~a-~a" #'name-dist param))])
       #'(begin
           (struct nd (p.param ...)
             #:transparent
             #:guard (lambda (p.param ... _name)
                       (define (bad who)
                         (maker-error 'nd '(p.param ...) '(p.pred ...) who p.param ...))
                       (unless (p.pred p.param) (bad 'p.param)) ...
                       (let-values ([(p.param ...)
                                     (~? (guard-expr p.param ... _name)
                                         (values p.param ...))])
                         (values (p.mkconv p.param) ...)))
             #:methods gen:dist
             [(define (-density self x)
                (cond [(not (rational? x)) 0]
                      [else (-pdf self (exact->inexact x) #f)]))
              (define (-total-measure self) 1)
              sample-def
              #;
              (define (-measure self ms)
                (match-define (nd p.param ...) self)
                _)
              (~? (begin dist-method ...))]
             #:methods gen:real-dist
             [auto-real-defs
              (~? (begin real-method ...))]
             more ...)))]))

#;
(define-syntax (define-dist-type stx)
  (syntax-parse stx
    [(define-dist-type name-dist:id (p:param-spec ...)
       (~or (~eh-var o options)
            (~eh-var f prob-options)
            (~once base:base-measure)
            (~optional (~seq #:guard guard-fun:expr))
            (~optional (~seq #:extra [extra-clause ...])))
       ...)
     (unless (regexp-match? #rx"-distx?$" (symbol->string (syntax-e #'name-dist)))
       (raise-syntax-error #f "name does not end with `-dist'" stx #'name-dist))
     (with-syntax ([(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name-dist "~a-~a" #'name-dist param))])
       #'(begin
           (~? o.provide-clause (provide (struct-out name-dist)))
           (struct name-dist (p.param ...)
             #:guard (lambda (p.param ... _name)
                       (define (bad fname)
                         (maker-error 'name-dist '(p.param ...) '(p.pred ...) fname p.param ...))
                       (unless (p.pred p.param) (bad 'p.param)) ...
                       (~? (guard-fun p.param ... _name)
                           (values p.param ...)))
             #:methods gen:dist/sample
             [(define (*sample d)
                (f.sample-fun (get-param d) ...))]
             #:methods gen:dist
             [(define (*type d) 'name-dist)
              (define (*params d) (vector (get-param d) ...))
              (define (*density d x full?)
                (~? (f.density-fun (get-param d) ... x full?)
                    (make-density (and (or full? base.ndensity) (*pdf d x #f))
                                  (and (or full? base.ldensity) (*pdf d x #t))
                                  base.ddim)))
              (~? (define (*pdf d x log?)
                    (f.pdf-fun (get-param d) ... x log?)))
              (~? (define (*cdf d x log? 1-p?)
                    (f.cdf-fun (get-param d) ... x log? 1-p?)))
              (~? (define (*inv-cdf d x log? 1-p?)
                    (f.inv-cdf-fun (get-param d) ... x log? 1-p?)))
              (~? (define (*total-mass d) (let ([p.param (get-param d)] ...) o.total-mass)))
              (~? (define (*Denergy d x . d/dts)
                    (apply (let ([p.param (get-param d)] ...) o.Denergy-fun) x d/dts)))
              (~? (define (*support d) (let ([p.param (get-param d)] ...) o.support)))
              (~? (define (*enum d) (let ([p.param (get-param d)] ...) o.enum)))
              (~? (define (*conjugate d data-d data)
                    (let ([p.param (get-param d)] ...)
                      (o.conj-fun data-d data))))
              (~? (define (*mean d) (let ([p.param (get-param d)] ...) o.mean)))
              (~? (define (*median d) (let ([p.param (get-param d)] ...) o.median)))
              (~? (define (*modes d) (let ([p.param (get-param d)] ...) o.modes)))
              (~? (define (*variance d) (let ([p.param (get-param d)] ...) o.variance)))]
             (~? (~@ extra-clause ...))
             #:transparent)))]))

(define (maker-error sname fnames fpreds bad-fname . fvalues)
  (for ([fname (in-list fnames)] [fpred (in-list fpreds)] [index (in-naturals)])
    (when (eq? fname bad-fname)
      (apply raise-argument-error sname (format "~s" fpred) index fvalues))))

;; ------------------------------------------------------------

#;
(define-syntax (define-fl-dist-type stx)
  (define-syntax-class flparam
    (pattern [param:id pred:expr]
             #:with mkconv #'exact->inexact
             #:with ref #'param)
    (pattern [param:id pred:expr #:exact]
             #:with mkconv #'begin
             #:with ref #'(exact->inexact param)))
  (define-syntax-class kind-kw
    (pattern #:real #:with convert #'begin #:with measure #'#:lebesgue)
    (pattern #:nat  #:with convert #'inexact->exact #:with measure #'#:counting))
  (syntax-parse stx
    [(define-fl-dist-type name-dist:name-dist-id (p:flparam ...)
       kind:kind-kw
       (~optional (~seq #:guard ~! guard-fun:expr))
       (~and (~seq (~or (~eh-var o options)) ...)
             (~seq more-options ...)))
     (define prefix "m:fl")
     (define name #'name-dist.name)
     (with-syntax ([fl-pdf (format-id name "~a~a-pdf" prefix name)]
                   [fl-cdf (format-id name "~a~a-cdf" prefix name)]
                   [fl-inv-cdf (format-id name "~a~a-inv-cdf" prefix name)]
                   [fl-sample (format-id name "~a~a-sample" prefix name)]
                   [(get-param ...)
                    (for/list ([param (in-list (syntax->list #'(p.param ...)))])
                      (format-id #'name-dist "~a-~a" #'name-dist param))])
       #'(begin
           (define (pdf p.param ... x log?)
             (cond [(not (real? x)) (if log? -inf.0 0)]
                   [else (fl-pdf p.ref ... (exact->inexact x) log?)]))
           (define (cdf p.param ... x log? 1-p?)
             (fl-cdf p.ref ... (exact->inexact x) log? 1-p?))
           (define (inv-cdf p.param ... x log? 1-p?)
             (kind.convert (fl-inv-cdf p.ref ... (exact->inexact x) log? 1-p?)))
           (define (sample p.param ...)
             (kind.convert (flvector-ref (fl-sample p.ref ... 1) 0)))
           (define-dist-type name-dist (p ...)
             kind.measure #:pdf pdf #:cdf cdf #:inv-cdf inv-cdf #:sample sample
             #:total-mass (~? o.total-mass 1)
             #:guard (lambda (p.param ... _name)
                       (let-values ((~? [(p.param ...) (guard-fun p.param ... _name)]))
                         (values (p.mkconv p.param) ...)))
             more-options ...)))]))
