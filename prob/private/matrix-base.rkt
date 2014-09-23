;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Defines wrapped (monomorphic) types used by prob/matrix.

#lang typed/racket/base
(require (prefix-in t: math/array)
         (prefix-in t: math/matrix))

(provide (struct-out ImmArray)
         (struct-out MutArray)
         Array
         Array?
         Array-contents)

(struct: ImmArray ([contents : (t:Array Real)]) #:transparent)
(struct: MutArray ([contents : (t:Mutable-Array Real)]) #:transparent)
(define-type Array (U ImmArray MutArray))

(define (Array? x)
  (or (ImmArray? x) (MutArray? x)))

(: Array-contents : (U ImmArray MutArray) -> (t:Array Real))
(define (Array-contents x)
  (cond [(ImmArray? x) (ImmArray-contents x)]
        [(MutArray? x) (MutArray-contents x)]))
