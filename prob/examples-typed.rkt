#lang prob/typed

;; Very preliminary typed version of language.
;; Only a few bindings are provided.

;; Code in the module works, but require doesn't, and
;; the DrRacket REPL doesn't either. To be investigated...

(: f : Integer -> Integer)
(define (f x)
  (add1 x))

(enumerate
 (define A (flip))
 (define B (flip))
 A
 #:when (or A B))

(define s-mh
  (mh-sampler
   (define A (flip))
   (define B (flip))
   A
   #:when (or A B)))
(s-mh)

(: geom : -> Exact-Nonnegative-Integer)
(define (geom)
  (if (flip) 0 (add1 (geom))))

#|
(parameterize ((verbose? #t))
  (for/list ([i 10]) (s-mh)))
|#

;; mem: can't type correctly
