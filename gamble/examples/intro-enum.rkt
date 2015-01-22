;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble

;; sum-n-flips : Nat -> Nat
(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (bernoulli) (sum-n-flips (sub1 n)))))

(printf "enumerate (sum-n-flips 10)\n")
(enumerate (sum-n-flips 10))

;; ------------------------------------------------------------

;; geom : Prob -> Nat
(define (geom p)
  (if (flip p) 0 (add1 (geom p))))

(printf "enumerate (geom 1/2) with limit 1e-3\n")
(enumerate (geom 1/2) #:limit 1e-3)

(printf "enumerate (geom 1/2) with limit 1e-3, unnormalized\n")
(enumerate (geom 1/2) #:limit 1e-3 #:normalize? #f)

(newline)

;; ------------------------------------------------------------

(printf "enumerate 2 flips w/ at least one true\n")
(enumerate
 (define A (flip))
 (define B (flip))
 A
 #:when (or A B))

(printf "lazy enumeration of infinite dists w/ limit\n")
(enumerate 
   (define A (geom 1/2))
   A
   #:when (< 10 A 20)
   #:limit 1e-3)

(newline)

;; ------------------------------------------------------------

(printf "drunk-flip example from EPP\n")
(enumerate
 (define (drunk-flip)
   (if (flip 0.9)
       (fail) ;; dropped the coin
       (flip .5)))
 (define (drunk-andflips n)
   (cond [(zero? n)
          #t]
         [else
          (and (drunk-flip)
               (drunk-andflips (sub1 n)))]))
 (drunk-andflips 10)
 #:normalize? #f
 ;; Need to disable limit to detect #t case
 #:limit #f)

(newline)

;; ------------------------------------------------------------

;; reify/reflect, aka enumerate/sample

(define (xor a b) (and (or a b) (not (and a b))))

(define (xor-flips n)
  (let loop ([n n])
    (if (zero? n)
        #f
        (xor (flip) (loop (sub1 n))))))

(printf "naive xor-flips is slow (exponential)\n")
(time (enumerate (xor-flips 12)))

(define (xor-flips* n)
  (let loop ([n n])
    (if (zero? n)
        #f
        (let ([r (sample (enumerate (loop (sub1 n))))])
          (xor (flip) r)))))

(printf "enumerate then sample is faster (linear)\n")
(time (enumerate (xor-flips* 120)))

(newline)

;; ------------------------------------------------------------

;; mem generalizes EPP letlazy

(define (flips-all-true n)
  (enumerate
   (define Flips (for/list ([i n]) (flip)))
   (andmap values Flips)))

(printf "slow to generate all flips, despite many being unused\n")
(time (flips-all-true 12))

(define (flips-all-true* n)
  (enumerate
   (define LFlips (for/list ([i n]) (mem flip)))
   (andmap (lambda (p) (p)) LFlips)))

(printf "do flips lazily; only force when needed\n")
(time (flips-all-true* 12))

(newline)
