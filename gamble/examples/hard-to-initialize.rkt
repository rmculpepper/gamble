;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble
(require racket/class
         (except-in rackunit fail))

(define N 100)

;; ----

;; The following model is difficult to initialize; a randomly chosen
;; state has only a 1 in 2^100 chance of satisfying the observations.

(define bad
  (mh-sampler
   (define xs (for/list ([i N]) (flip)))
   (for ([i N])
     (unless (list-ref xs i) (fail)))
   xs))

(let ([th (thread (lambda () (bad)))])
  (check-equal? (sync/timeout 2 th) #f)
  (kill-thread th))
(printf "`bad' sampler doesn't initialize, as expected\n")

;; ----

;; The model can be initialized incrementally by mutating an external
;; variable that controls how many observations are performed, but a
;; legal state may become illegal when new observations are
;; added. Let's call that the "reinitialization problem". There are
;; two ways of dealing with it.

(define stage (make-parameter 0))

;; ----

;; One solution is to call the reinitialize method with a transition;
;; the transition is used to search from the old, now-illegal state to
;; find a legal state. A legal state must be reachable in only one
;; step from the old state.

(define incr1
  (mh-sampler
   (define xs (for/list ([i N]) (flip)))
   (for ([i (stage)])
     (unless (list-ref xs i) (fail)))
   xs))

;; Incrementally initialize incr1, one observation at a time.
(for ([k N])
  (stage k)
  (send incr1 reinitialize (single-site)))
(printf "`incr1' sampler initialized (slowly)\n")

;; ----

;; The previous solution is still somewhat inefficient. At stage k,
;; the single-site transition only has a 1/k chance of changing the
;; relevant choice. The model can be more efficiently initialized by
;; specifying the choice to be changed using zones.

(define incr1a
  (mh-sampler
   (define xs (for/list ([i N]) (with-zone i (flip))))
   (for ([i (stage)])
     (unless (list-ref xs i) (fail)))
   xs))

;; Incrementally initialize incr1, one observation at a time.
(for ([k (in-range 1 (add1 N))])
  (stage k)
  (send incr1a reinitialize (single-site proposal:resample #:zone (sub1 k))))
(printf "`incr1a' sampler initialized\n")

;; ----

;; Another solution is to incrementally add to the state as well as to
;; the observations, so that an old state is not inconsistent with new
;; evidence. Instead, the old state can be simply extended to a legal
;; new state.

(define incr2
  (mh-sampler
   (define xs (for/list ([i (stage)]) (flip)))
   (for ([i (stage)])
     (unless (list-ref xs i) (fail)))
   xs))

;; Incrementally initialize incr2, one choice and observation at a time.
(for ([k N])
  (stage k)
  (send incr2 reinitialize #| default: (rerun) |#))
(printf "`incr2' sampler initialized\n")

;; ----

;; A variation on the previous solution is to use laziness. The state
;; only consists of choices that are forced. But the sampler result
;; must also avoid forcing too much.

(define incr3
  (mh-sampler
   (define xs (for/list ([i N]) (pdelay (flip))))
   (for ([i (stage)])
     (unless (pforce (list-ref xs i)) (fail)))
   ;; Until we're done initializing, we don't actually care about the
   ;; sampler result.
   (and (< (stage) N) xs)))

(for ([k N])
  (stage k)
  (send incr3 reinitialize))
(printf "`incr3' sampler initialized\n")
