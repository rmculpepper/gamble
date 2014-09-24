;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Defines macros for building arrays and matrices.  Cannot define in
;; prob/matrix because macro from typed module cannot be used in
;; untyped module.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
         (prefix-in t: math/array)
         (prefix-in t: math/matrix)
         (only-in typed/racket/base :)
         "matrix-base.rkt")
(provide array
         mutable-array
         matrix
         row-matrix
         col-matrix
         for/matrix
         for*/matrix)

(define-syntax-rule (array elts)
  (ImmArray (t:array elts : Real)))
(define-syntax-rule (mutable-array elts)
  (MutArray (t:mutable-array elts : Real)))
(define-syntax-rule (matrix elts)
  (ImmArray (t:matrix elts : Real)))
(define-syntax-rule (row-matrix elts)
  (ImmArray (t:row-matrix elts : Real)))
(define-syntax-rule (col-matrix elts)
  (ImmArray (t:col-matrix elts : Real)))

;; ----------------------------------------

(begin-for-syntax
  (define-splicing-syntax-class maybe-fill
    #:attributes (fill)
    (pattern (~optional (~seq #:fill fill:expr)))))

#|
;; For some reason, for/matrix and for*/matrix trigger "Macro from
;; typed module used in untyped code" error when wrapped the obvious
;; way:
(define-syntax (for/matrix stx)
  (syntax-parse stx
    [(_ m:expr n:expr :maybe-fill (clause ...) . body)
     (template/loc stx
       (ImmArray
        (t:for/matrix: m n (?? (?@ #:fill fill)) (clause ...) : Real . body)))]))
|#

(begin-for-syntax
  (define (do-for/matrix who for/vector-id stx)
    (syntax-parse stx
      [(_ me:expr ne:expr :maybe-fill (clause ...) . body)
       (with-syntax ([who who] [for/vector for/vector-id])
         (template/loc stx
           (let* ([m me] [n ne])
             (ImmArray
              (t:vector->matrix m n
                (for/vector #:length (* m n) #:fill (?? fill 0) (clause ...)
                  (let ([e (let () . body)])
                    (unless (real? e)
                      (error 'who "expected real value as result of body expression\n  got: ~e" e))
                    e)))))))])))

(define-syntax (for/matrix stx) (do-for/matrix 'for/matrix #'for/vector stx))
(define-syntax (for*/matrix stx) (do-for/matrix 'for*/matrix #'for*/vector stx))
