#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/name)
         "../interfaces.rkt")
(provide (except-out (all-defined-out) table table*)
         (rename-out [table* table]))

(struct ppromise (thunk))

(define-syntax-rule (pdelay e ...)
  (ppromise (mem (lambda () e ...))))

(define (pforce pp)
  ((ppromise-thunk pp)))

(define-syntax (deflazy stx)
  (syntax-parse stx
    [(deflazy x:id e:expr)
     (with-syntax ([(xtmp) (generate-temporaries #'(x))]
                   [x (syntax-property #'x 'gamble:model:export-mode 'lazy)])
       #'(begin (define xtmp (mem (lambda () e)))
                (define-syntaxes (x)
                  (make-variable-like-transformer
                   #'(xtmp)))))]))

(define-syntax (defmem stx)
  (define-syntax-class formals
    (pattern (_:id ...))
    (pattern (_:id ... . _:id)))
  (syntax-parse stx
    [(defmem (f:id . frm:formals) body:expr ...+)
     #'(define f (mem (let ([f (lambda frm body ...)]) f)))]))

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
