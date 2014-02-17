#lang prob/typed

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
