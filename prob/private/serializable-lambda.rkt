;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/define
                     syntax/name
                     syntax/free-vars)
         racket/serialize)
(provide lambda/s
         define/s)

(serializable-struct serializable-fun (name mod sym env)
  #:property prop:procedure
  (lambda (sl . args) (apply (get-sf-proc sl) args))
  #:property prop:custom-write
  (lambda (sl out mode)
    (if (serializable-fun-name sl)
        (fprintf out "#<serializable-procedure:~a>" (serializable-fun-name sl))
        (fprintf out "#<serializable-procedure>"))))

;; ----------------------------------------

;; sl-fn-table : WeakHashEq[ serializable-fun => procedure ]
(define sf-fn-table (make-weak-hasheq))

;; get-sl-proc : serializable-lambda -> procedure
(define (get-sf-proc sf)
  (hash-ref! sf-fn-table sf (lambda () (get-sf-proc* sf))))
(define (get-sf-proc* sf)
  (apply (dynamic-require (serializable-fun-mod sf) (serializable-fun-sym sf))
         (serializable-fun-env sf)))

;; ----------------------------------------

(begin-for-syntax
  (define counter 0)
  (define (genname)
    (set! counter (add1 counter))
    (syntax-local-introduce (format-id #f "serializable-lambda-~a" counter)))
  (define insp
    (variable-reference->module-declaration-inspector
     (#%variable-reference))))

(define (serializable-fun* name id sym env)
  (define b (identifier-binding id))
  (unless (list? b)
    (error 'serializable-lambda "INTERNAL ERROR: bad identifier binding"))
  (define def-mod (car b))
  ;; (define def-sym (cadr b))
  (serializable-fun name def-mod sym env))

(define-syntax (lambda/s stx)
  (if (eq? (syntax-local-context) 'expression)
      (syntax-parse stx
        [(_ (arg:id ...) . body)
         (let ([e-lambda
                (local-expand #'(lambda (arg ...) . body) 'expression null)])
           (with-syntax ([e-lambda e-lambda]
                         [(fv ...) (free-vars e-lambda insp)])
             (define lifted-id
               (syntax-local-lift-expression #'(lambda (fv ...) e-lambda)))
             (with-syntax ([lifted-id lifted-id]
                           [sym-id (genname)]
                           [name (syntax-local-infer-name stx)])
               (syntax-local-lift-provide
                #'(rename lifted-id sym-id))
               #'(serializable-fun* 'name (quote-syntax lifted-id) 'sym-id
                                    (list fv ...)))))])
      #`(#%expression #,stx)))

(define-syntax (define/s stx)
  (define-values (name rhs)
    (normalize-definition stx #'lambda/s #t #f))
  (with-syntax ([name name] [rhs rhs])
    #'(define name rhs)))
