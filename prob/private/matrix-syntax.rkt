;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Defines macros for building arrays and matrices.  Cannot define in
;; prob/matrix because macro from typed module cannot be used in
;; untyped module.

#lang racket/base
(require (prefix-in t: math/array)
         (prefix-in t: math/matrix)
         (only-in typed/racket/base :)
         "matrix-base.rkt")
(provide array
         mutable-array
         matrix
         row-matrix
         col-matrix)

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
