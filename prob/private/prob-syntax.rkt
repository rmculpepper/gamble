;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     syntax/name)
         racket/contract/base
         racket/class
         "dist.rkt"
         "prob-util.rkt"
         "prob-hmc.rkt"
         "prob-mh.rkt"
         "prob-enum.rkt"
         "interfaces.rkt")
(provide rejection-sampler
         importance-sampler
         mh-sampler
         hmc-sampler
         enumerate
         enum-importance-sampler
         label
         derivative
         ppromise?
         pdelay
         (contract-out
          [pforce (-> ppromise? any)])
         (rename-out [table* table])
         table?)

;; ----

(begin-for-syntax
 (define-syntax-class special-condition
   (pattern ((~datum =) label value:expr)
            #:with e #'(cons `label (spcond:equal value)))))

;; ----

(define-syntax (rejection-sampler stx)
  (syntax-parse stx
    [(rejection-query def:expr ... result:expr
                      (~optional (~seq #:when condition:expr)))
     (template
      (rejection-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))))]))

(define (rejection-sampler* thunk)
  (new rejection-sampler% (thunk thunk)))

(define rejection-sampler%
  (class sampler-base%
    (init-field thunk)
    (super-new)
    (define/override (sample)
      (let ([v (let/ec escape
                 (parameterize ((current-stochastic-ctx
                                 (new rejection-stochastic-ctx%
                                      (escape escape))))
                   (cons 'succeed (thunk))))])
        (case (car v)
          [(succeed) (cdr v)]
          [(fail) (sample)])))
    ))

(define rejection-stochastic-ctx%
  (class plain-stochastic-ctx%
    (init-field escape)
    (super-new)
    (define/override (fail reason)
      (escape (cons 'fail reason)))
    (define/override (observe-at dist val)
      (cond [(dist-pdf-max dist)
             => (lambda (maxpdf)
                  (unless (< (* (random) maxpdf) (dist-pdf dist val))
                    (fail 'observation)))]
            [(finite-dist? dist)
             (unless (< (random) (dist-pdf dist val))
               (fail 'observation))]
            [else
             (error 'observe-at
                    (string-append 
                     "observation on distribution not supported by rejection sampler"
                     ";\n cannot determine multiplier for distribution"
                     "\n  distribution: ~e")
                    dist)]))
    ))

;; ----

(define-syntax (importance-sampler stx)
  (syntax-parse stx
    [(importance-sampler def:expr ... result:expr
                         (~optional (~seq #:when condition:expr))
                         (~seq #:cond sp:special-condition)
                         ...)
     (template
      (importance-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (list sp.e ...)))]))

(define (importance-sampler* thunk spconds)
  (new importance-sampler% (thunk thunk) (spconds spconds)))

(define importance-sampler%
  (class* object% (weighted-sampler<%>)
    (init-field thunk
                spconds)
    (super-new)

    (define/public (sample/weight)
      (let ([v (let/ec escape
                 (define ctx (new importance-stochastic-ctx% (escape escape)))
                 (parameterize ((current-stochastic-ctx ctx))
                   (define result (thunk))
                   (define weight (get-field weight ctx))
                   (cons 'succeed (cons (thunk) weight))))])
        (case (car v)
          [(succeed) (cdr v)]
          [(fail) (sample/weight)])))
    ))

(define importance-stochastic-ctx%
  (class rejection-stochastic-ctx%
    (field [weight 1])
    (inherit fail)
    (super-new)
    (define/override (observe-at dist val)
      (set! weight (* weight (dist-pdf dist val))))
    ))

;; ----

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sampler def:expr ... result:expr
                 (~optional (~seq #:when condition:expr))
                 (~seq #:cond sp:special-condition)
                 ...)
     (template
      (mh-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (list sp.e ...)))]))

;; ----

(define-syntax (hmc-sampler stx)
  (syntax-parse stx
    [(hmc-sampler def:expr ... result:expr
                  (~or (~optional (~seq #:epsilon epsilon:expr))
                       (~optional (~seq #:L L:expr))
                       (~optional (~seq #:when condition:expr))
                       (~seq #:cond sp:special-condition))
                  ...)
     (template
      (hmc-sampler*
       (λ () def ... (begin0 result (unless (?? condition #t) (fail))))
       (?? epsilon 0.01)
       (?? L 10)
       (list sp.e ...)))]))

;; ----

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr
                (~or (~optional (~seq #:when condition:expr))
                     (~seq #:cond sp:special-condition)
                     (~optional (~seq #:limit limit:expr))
                     (~optional (~seq #:normalize? normalize?)))
                ...)
     (template
      (enumerate*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (list sp.e ...)
       (?? (?@ #:limit limit))
       (?? (?@ #:normalize? normalize?))))]))

(define-syntax (enum-importance-sampler stx)
  (syntax-parse stx
    [(importance-sample def:expr ... result:expr
                        (~optional (~seq #:when condition:expr))
                        (~seq #:cond sp:special-condition) ...)
     (template
      (enum-importance-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (list sp.e ...)))]))

;; ----

(struct ppromise (thunk))

(define-syntax-rule (pdelay e ...)
  (ppromise (mem (lambda () e ...))))

(define (pforce pp)
  ((ppromise-thunk pp)))

;; ----

(define-syntax-rule (label l e)
  (parameterize ((current-label l)) e))

;; ----

(begin-for-syntax
  (define-syntax-class derivative-spec
    (pattern [(ids:id ...) grad-e:expr]
             #:with grad #'(cons (vector (quote ids) ...)
                                 grad-e))
    (pattern #f
             #:with grad #'#f)))

(define-syntax (derivative stx)
  (syntax-parse stx
    [(derivative dist:expr pderivs:derivative-spec ...)
     (template
      (parameterize ([current-derivatives (vector pderivs.grad ...)])
        dist))]))

;; ----

(begin-for-syntax
 (define-splicing-syntax-class maybe-lazy
   (pattern (~seq #:lazy)
            #:with wrap-body #'pdelay
            #:with lazy? #'#t)
   (pattern (~seq)
            #:with wrap-body #'begin
            #:with lazy? #'#f)))

(define table-none (gensym 'none))

(define-syntax (table* stx)
  (syntax-parse stx
    [(table ([x:id seq:expr] ...) l:maybe-lazy body:expr ...+)
     (with-syntax ([inferred-name (syntax-local-infer-name stx)])
       #'(let* ([h (for*/hash ([x seq] ...)
                     (values (vector x ...)
                             (l.wrap-body (let () body ...))))])
           (make-table h 'inferred-name (length '(x ...)) l.lazy?)))]
    [(table (x:id ...) body:expr ...+)
     (with-syntax ([inferred-name (syntax-local-infer-name stx)])
       #'(let ([inferred-name (lambda (x ...) body ...)])
           (make-memo-table (mem inferred-name) 'inferred-name)))]))

(define-struct table (h name arity lazy?)
  #:property prop:procedure
  (lambda (t . args)
    (let ([key (list->vector args)]
          [h (table-h t)])
      (let ([v (hash-ref h key table-none)])
        (cond [(eq? v table-none)
               (table-error t key)]
              [(table-lazy? t)
               (pforce v)]
              [else v]))))
  #:property prop:custom-write
  (lambda (t port mode)
    (write-string (format "#<table:~s>" (table-name t)) port)))

(define (table-error t key)
  (cond [(= (vector-length key) (table-arity t))
         (error (table-name t)
                "table has no value for given arguments\n  arguments: ~e"
                (vector->list key))]
        [else
         (apply raise-arity-error (table-name t) (table-arity t) (vector->list key))]))

;; FIXME: add operations on table, eg enumerate keys?

;; FIXME: recognize array cases, use more compact representation?

(define-struct memo-table (f name)
  #:property prop:procedure (struct-field-index f)
  #:property prop:custom-write
  (lambda (t port mode)
    (write-string (format "#<table:~s>" (memo-table-name t)) port)))
