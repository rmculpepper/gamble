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

(define ((mh-compute-dist iters) proc)
  (sampler->discrete-dist (mh-sampler (proc)) iters))

(define ((rejection-compute-dist iters) proc)
  (sampler->discrete-dist (rejection-sampler (proc)) iters))

(define ((enumerate-compute-dist) proc)
  (enumerate (proc)))

(define ((enum-imp-compute-dist iters) proc)
  (sampler->discrete-dist (enum-importance-sampler (proc)) iters))

(make-basic-tests 'rejection (rejection-compute-dist 1000) 0.05)
(make-basic-tests 'mh        (mh-compute-dist 1000)        0.10)
(make-basic-tests 'enumerate (enumerate-compute-dist)      1e-6)
(make-basic-tests 'enum-imp  (enum-imp-compute-dist 1000)  0.05)

;; ----

#|
;; Pathological case for lazy-tree-based importance sampler

(define (bad)
  (if (flip)
      (if (flip) 1 (fail))
      (if (flip)
          (if (flip) 2 2)
          (if (flip) (fail) (fail)))))

(enumerate (bad))
;; => {1 -> 1/2, 2 -> 1/2}
(sampler->discrete-dist (enum-importance-sampler (bad)))
;; => {1 -> 0.66, 2 -> 0.33}

Might be able to fix by weighting immediate successes differently
depending on whether any successes were found along the chosen choice.
(But does that fix the problem or just push it a little further under the rug?)
|#
