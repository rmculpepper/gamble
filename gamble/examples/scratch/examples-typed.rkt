;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble/typed

;; Preliminary typed version of language.
;; Not all bindings are provided.

(enumerate
 (define A (flip))
 (define B (flip))
 (observe/fail (or A B))
 A)

(define s-mh
  (mh-sampler
   (define A (flip))
   (define B (flip))
   (observe/fail (or A B))
   A))
(s-mh)

(: geom : -> Nonnegative-Integer)
(define (geom)
  (if (flip) 0 (add1 (geom))))
