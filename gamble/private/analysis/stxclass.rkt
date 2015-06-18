;; Copyright (c) 2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/base
         racket/list
         racket/syntax
         syntax/parse
         syntax/id-table
         (for-template
          (only-in racket/contract/private/provide
                   contract-rename-id-property
                   provide/contract-info?
                   provide/contract-info-original-id)
          (only-in "../instrument-data.rkt"
                   non-random-first-order-funs
                   observation-propagators))
         "known-functions.rkt")
(provide final-arg-prop-fun
         all-args-prop-fun
         nrfo-fun
         contracted-export-id
         contract-indirection-id
         contracted-final-arg-prop-fun
         lifted-contracted-final-arg-prop-fun)

;; ============================================================
;; Syntax classes for recognizing observation propagators

(define-syntax-class final-arg-prop-fun
  #:attributes (pred inverter scaler)
  (pattern f:id
           #:with (#:final-arg pred inverter scaler)
           (free-id-table-ref observation-propagators #'f #f)))

(define-syntax-class all-args-prop-fun
  #:attributes (pred [inverter 1])
  (pattern f:id
           #:with (#:all-args pred (inverter ...))
           (free-id-table-ref observation-propagators #'f #f)))

(define-syntax-class nrfo-fun
  (pattern f:id
           #:when (or (eq? (classify-function #'f) 'non-random-first-order)
                      (free-id-table-ref non-random-first-order-funs #'f #f))))

;; Suppose contracted function f st (f e ...) => (f* blah e ...)
;; then the f* identifier has a property contract-rename-id-property
;; containing f-ren, where (syntax-local-value f-ren) is a
;; provide/contract-info structure.

(define-syntax-class contracted-export-id
  #:attributes (original-id)
  (pattern (~var x (static provide/contract-info? 'contracted-function))
           #:with original-id (provide/contract-info-original-id (attribute x.value))))

(define-syntax-class contract-indirection-id
  (pattern x:id
           #:with c:contracted-export-id (contract-rename-id-property #'x)
           #:with original-id #'c.original-id))

(define-syntax-class contracted-final-arg-prop-fun
  #:attributes (pred inverter scaler)
  (pattern c:contract-indirection-id
           #:when (not (regexp-match? #rx"^lifted\\." (symbol->string (syntax-e #'c))))
           #:with :final-arg-prop-fun #'c.original-id))

;; The other contract expansion form:
;; Suppose contracted function f st (f e ...) => (lifted.f e ...)
;; then the lifted.f identifier has a property contract-rename-id-property
;; containing f-ren, ....

(define-syntax-class lifted-contracted-final-arg-prop-fun
  #:attributes (pred inverter scaler)
  (pattern c:contract-indirection-id
           #:when (regexp-match? #rx"^lifted\\." (symbol->string (syntax-e #'c)))
           #:with :final-arg-prop-fun #'c.original-id))
