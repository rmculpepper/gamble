#lang prob

#|
Somebody gives Norman a hat containing five slips of paper, numbered 1
to 5 respectively. Norman draws a slip from the hat. The number on the
slip is called n. Norman then repeats the following procedure ten
times:

Take n dice from the bag, throw them, report the total t, then put the
dice back in the bag.

The totals reported are 21, 15, 34, 12, 18, 38, 46, 13, 24, and 27.

The question is, what is the number on the slip Norman drew? (That is,
what is n?)

Assume all of the dice in the bag are 20-sided.

Several solutions are presented below.
|#

(define (draw-slip) (+ 1 (discrete-uniform 5)))
(define (roll-die) (+ 1 (discrete-uniform 20)))
(define OBSERVED-TOTALS '(21 15 34 12 18 38 46 13 24 27))

;; ----

;; Naive solution. Doesn't work --- rejection rate is too high.

(define (naive)
  (define n (draw-slip))
  (define (sum) (for/sum ([i n]) (roll-die)))
  (for ([obs OBSERVED-TOTALS])
    (unless (= (sum) obs) (fail)))
  n)

(define s-naive (rejection-sampler (naive)))

;; ----

;; Evidence-softened version. Instead of equality, sum only has to be
;; "close" to observed value. Faster, but gives non-zero probability
;; to infeasible answers.

(define (softened)
  (define n (draw-slip))
  (define (sum) (for/sum ([i n]) (roll-die)))
  (for ([obs OBSERVED-TOTALS])
    (observe (normal (sum) 1) obs))
  n)

(define s-softened (mh-sampler (softened)))

;; ----

;; Less naive: instead of rejection sampling, build discrete
;; distribution of possible totals from rolling n dice, then observe
;; (uses likelihood weighting). Still takes a long time.

(define (less-naive)
  (define n (draw-slip))
  (define sum-dist (enumerate (for/sum ([i n]) (roll-die))))
  (for ([obs OBSERVED-TOTALS])
    (observe (sample sum-dist) obs))
  n)

(define s-less-naive (importance-sampler (less-naive)))

;; ----

;; Smart: Lift out repeated enumerations.

;; Note: defmem at top level is *naive* memoization; that's okay,
;; because the function here is actually deterministic; all of the
;; random behavior happens within an enumerate.

;; sum-dist : Nat -> Discrete-Dist[Nat]
;; Distribution of possible totals of rolling n dice.
(defmem (sum-dist n)
  (if (zero? n)
      (enumerate 0)
      (enumerate (+ (roll-die) (sample (sum-dist (sub1 n)))))))

;; (Optional) Force the relevant entries of sum-dist ahead of time.
(void (sum-dist 5))

(define (smart)
  (define n (draw-slip))
  (for ([obs OBSERVED-TOTALS])
    (observe (sample (sum-dist n)) obs))
  n)

(define s-smart (importance-sampler (smart)))
;; (sampler->discrete-dist s-smart 1000)

;; Can also solve exactly:
;; (enumerate (smart))

;; ============================================================

;; Here's a solution that uses incremental initialization.
;; See also prob/examples/hard-to-initialize.rkt.

(require racket/class)
(define stage (make-parameter 0))

(define (staged)
  (define n (draw-slip))
  (define (sum) (for/sum ([i n]) (with-zone i (roll-die))))
  (for ([obs OBSERVED-TOTALS]
        [i (stage)])
    (unless (= (sum) obs) (fail)))
  n)

(define s-staged (mh-sampler (staged)))

(define (initialize-s-staged)
  (for ([k (in-range 1 (add1 (length OBSERVED-TOTALS)))])
    ;; (eprintf "stage ~s\n" k)
    (stage k)
    (send s-staged reinitialize (multi-site proposal:resample #:zone k))))
