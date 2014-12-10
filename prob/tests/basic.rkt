;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang prob
(require (for-syntax racket/base)
         (except-in rackunit fail))

;; Parameterized by sampler method and tolerance?

(define (make-basic-tests name compute-dist tolerance)
  (define-syntax (test stx)
    (syntax-case stx ()
      [(test expr expected)
       #`(test-case (format "~a line ~s: ~.s" name '#,(syntax-line stx) 'expr)
           (define actual (compute-dist (lambda () expr)))
           (check <=
                  (discrete-dist-error actual (make-discrete-dist expected))
                  tolerance))]))

  (test (flip 1/2)
        '((#t . 1/2)
          (#f . 1/2)))

  (test (flip 2/3)
        '((#t . 2/3)
          (#f . 1/3)))

  (test (flip 1/10)
        '((#t . 1/10)
          (#f . 9/10)))

  (test (let ()
          (define A (flip))
          (define B (flip))
          (unless (or A B) (fail))
          A)
        '((#t . 2/3)
          (#f . 1/3)))

  )

(define ((rejection-compute-dist iters) proc)
  (sampler->discrete-dist (rejection-sampler (proc)) iters))

(define ((imp-compute-dist iters) proc)
  (sampler->discrete-dist (importance-sampler (proc)) iters))

(define ((mh-compute-dist iters) proc)
  (sampler->discrete-dist (mh-sampler (proc)) iters))

(define ((enumerate-compute-dist) proc)
  (enumerate (proc)))

(define ((egibbs-compute-dist iters) proc)
  (sampler->discrete-dist (mh-sampler (proc) #:transition (enumerative-gibbs)) iters))

(define ((slice-compute-dist iters) proc)
  (sampler->discrete-dist (mh-sampler (proc) #:transition (slice)) iters))

(make-basic-tests 'rejection (rejection-compute-dist 1000) 0.05)
(make-basic-tests 'imp-sampl (imp-compute-dist 1000)       0.05)
(make-basic-tests 'mh        (mh-compute-dist 1000)        0.10)
(make-basic-tests 'enumerate (enumerate-compute-dist)      1e-6)
(make-basic-tests 'egibbs    (egibbs-compute-dist 1000)    0.05)
(make-basic-tests 'slice     (slice-compute-dist 1000)     0.05)
