;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble
(require racket/format racket/class gamble/viz)

;; sum-n-flips : Nat -> Nat
(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (bernoulli) (sum-n-flips (sub1 n)))))

(define s-mh (mh-sampler (sum-n-flips 10)))

;; Setting (verbose? #t) causes MH sampler to print lines like
;; - NEW <dist> = <value>: <address>
;; - PERTURBED <dist> = <value>: <address>
;; - REUSED <dist> = <value>: <address>
;; depending on whether the choice is fresh, changed by the 
;; current MH proposal, or reused unchanged.
(parameterize ((verbose? #t))

  (printf "Generating first sample\n")
  (define sample1 (s-mh))
  (printf "Sample 1 = ~s\n\n" sample1)

  (printf "Generating second sample\n")
  (define sample2 (s-mh))
  (printf "Sample 2 = ~s\n\n" sample2))

;; The samples should be at most 1 apart, because all but
;; one flip should have been reused (single-site MH by default).

(printf "sample dist of (sum-n-flips 10)\n")
(sampler->discrete-dist s-mh 100)

(printf "histogram of (sum-n-flips 10)\n")
(hist (repeat s-mh 100))

(newline)

;; ------------------------------------------------------------

;; tail-recursive version works too

(define (sum-n-flips* n)
  (let loop ([n n] [acc 0])
    (if (zero? n)
        acc
        (loop (sub1 n) (+ acc (bernoulli))))))

(define s-mh* (mh-sampler (sum-n-flips* 10)))

(printf "sample dist of (sum-n-flips* 10)\n")
(sampler->discrete-dist s-mh* 100)

;; ------------------------------------------------------------

;; mem

(define mem-mh
  (mh-sampler
   ;; Note: need to call mem within mh-sampler, not outside
   (define get-the-flip (mem (lambda (n) (bernoulli))))
   (for/sum ([i 10]) (get-the-flip (modulo i 5)))))

(printf "should only see 5 NEW lines (second 5 are memoized)\n")
(parameterize ((verbose? #t))
  (mem-mh))

(newline)

;; ------------------------------------------------------------

;; observation

(define s-obs
  (mh-sampler
   (define R (normal 10 1))
   (observe-sample (normal-dist R 1) 9)
   R))

(printf "observation affects posterior\n")
(sampler->mean+variance s-obs 10000)

(printf "the analytic solution, for comparison\n")
(printf "  NOTE: dist uses stddev, not variance\n")
(dist-update-prior (normal-dist 10 1) '(normal-dist _ 1) (vector 9))

(newline)
