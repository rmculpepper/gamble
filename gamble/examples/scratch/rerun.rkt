#lang gamble
(require racket/class)

;; Demonstrates that rerun is necessary after changing observations.

;; used in observation, mutated below!
(define expected 1)

(define s
  (mh-sampler
   (define b (uniform 0 1))
   (for ([i 10]) (observe (bernoulli b) expected))
   b))

(printf "with expected = 1\n")
(sampler->mean+variance s 1000)
(newline)

(printf "the last sample with expected = 1\n")
(s)
(newline)

(set! expected 0)

(printf "set expected = 0, BUT w/o rerun\n")
(printf "  same result as above, because all moves rejected by MH!\n")
(sampler->mean+variance s 1000)
(newline)

(void (send s rerun))

(printf "after rerun\n")
(printf "  now get a reasonable result\n")
(sampler->mean+variance s 1000)
(newline)
