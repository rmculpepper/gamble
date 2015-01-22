;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     unstable/syntax
                     syntax/name)
         racket/class
         "dist.rkt"
         "context.rkt"
         "prob-util.rkt"
         "prob-mh.rkt"
         "prob-enum.rkt"
         "interfaces.rkt")
(provide (except-out (all-defined-out) table table*)
         (rename-out [table* table]))

;; ----

(define-syntax (observe stx)
  (syntax-case stx ()
    [(observe e v)
     ;; Note: instrumenter uses 'observed-expr property to report error
     (with-syntax ([thunk (syntax-property #'(lambda () e) 'observe-form stx)])
       #'(observe* thunk v))]))

(define-syntax (check-observe stx)
  (syntax-case stx ()
    [(check-observe e)
     #'(check-observe* (lambda () e))]))

(define observe/fail
  ;; FIXME: turn 2nd case into observe with fail instead of error?
  ;; FIXME: then convert 1st case w/ =, equal?, etc into 2nd case
  (case-lambda
    [(v) (unless v (fail 'observation))]
    [(v1 v2) (unless (equal? v1 v2) (fail 'observation))]))

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
    (field [successes 0]
           [rejections 0])
    (super-new)

    (define/override (info)
      (printf "== Rejection sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections))

    (define/override (sample)
      (define ctx (new rejection-stochastic-ctx%))
      (define v (send ctx run thunk))
      (case (car v)
        [(okay)
         (set! successes (add1 successes))
         (cdr v)]
        [(fail)
         (set! rejections (add1 rejections))
         (sample)]))
    ))

(define rejection-stochastic-ctx%
  (class plain-stochastic-ctx/run%
    (inherit fail)
    (super-new)
    (define/override (observe-sample dist val scale)
      (cond [(or (finite-dist? dist) (integer-dist? dist))
             ;; ie, actually have pmf
             (unless (< (random) (dist-pdf dist val))
               (fail 'observation))]
            [else
             (error 'observe-sample
                    (string-append 
                     "observation on distribution not supported by rejection sampler"
                     "\n  distribution: ~e")
                    dist)]))
    ))

;; ----

(define-syntax (importance-sampler stx)
  (syntax-parse stx
    [(importance-sampler def:expr ... result:expr
                         (~optional (~seq #:when condition:expr)))
     (template
      (importance-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))))]))

(define (importance-sampler* thunk)
  (new importance-sampler% (thunk thunk)))

(define importance-sampler%
  (class* object% (weighted-sampler<%>)
    (init-field thunk)
    (field [successes 0]
           [rejections 0])
    (super-new)

    (define/public (info)
      (printf "== Importance sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections))

    (define/public (sample/weight)
      (define ctx (new importance-stochastic-ctx%))
      (define v (send ctx run thunk))
      (case (car v)
        [(okay)
         (set! successes (add1 successes))
         (cons (cdr v) (get-field weight ctx))]
        [(fail)
         (set! rejections (add1 rejections))
         (sample/weight)]))
    ))

(define importance-stochastic-ctx%
  (class rejection-stochastic-ctx%
    (field [weight 1])
    (inherit fail)
    (super-new)
    (define/override (observe-sample dist val scale)
      (define l (dist-pdf dist val))
      (if (positive? l)
          (set! weight (* weight l scale))
          (fail 'observation)))
    ))

;; ----

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sampler def:expr ... result:expr
                 (~or (~optional (~seq #:when condition:expr))
                      (~optional (~seq #:transition tx)))
                 ...)
     #:declare tx (expr/c #'mh-transition?)
     (template
      (mh-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (?? tx.c)))]))

;; ----

(define-syntax (hmc-sampler stx)
  (syntax-parse stx
    [(hmc-sampler def:expr ... result:expr
                  (~or (~optional (~seq #:epsilon epsilon:expr))
                       (~optional (~seq #:L L:expr))
                       (~optional (~seq #:when condition:expr)))
                  ...)
     (template
      (mh-sampler*
       (λ () def ... (begin0 result (unless (?? condition #t) (fail))))
       (hmc (?? epsilon 0.01) (?? L 10))))]))

;; ----

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr
                (~or (~optional (~seq #:when condition:expr))
                     (~optional (~seq #:limit limit:expr))
                     (~optional (~seq #:normalize? normalize?))
                     (~optional (~seq #:exact? exact?)))
                ...)
     (template
      (enumerate*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (?? limit #f)
       (?? normalize? #t)
       (?? exact? #t)))]))

(define-syntax (enum-importance-sampler stx)
  (syntax-parse stx
    [(importance-sample def:expr ... result:expr
                        (~optional (~seq #:when condition:expr)))
     (template
      (enum-importance-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))))]))

;; ----

(struct ppromise (thunk))

(define-syntax-rule (pdelay e ...)
  (ppromise (mem (lambda () e ...))))

(define (pforce pp)
  ((ppromise-thunk pp)))

(define-syntax (deflazy stx)
  (syntax-parse stx
    [(deflazy x:id e:expr)
     (with-syntax ([(xtmp) (generate-temporaries #'(x))])
       #'(begin (define xtmp (pdelay e))
                (define-syntax x
                  (make-variable-like-transformer
                   #'(pforce xtmp)))))]))

(define-syntax (defmem stx)
  (define-syntax-class formals
    (pattern (_:id ...))
    (pattern (_:id ... . _:id)))
  (syntax-parse stx
    [(defmem (f:id . frm:formals) body:expr ...+)
     #'(define f (mem (let ([f (lambda frm body ...)]) f)))]))

;; ----

(define-syntax-rule (label l e)
  (parameterize ((current-label l)) e))

;; ----

(define-syntax-rule (with-zone z e ...)
  (parameterize ((current-zones (cons z (current-zones)))) e ...))

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
