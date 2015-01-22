;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (rename-in racket/match [match-define defmatch])
         racket/class
         racket/format
         "base.rkt"
         "db.rkt"
         "../interfaces.rkt"
         "../dist.rkt")
(provide (all-defined-out))

;; ============================================================

;; proposal:<X> : ... -> Proposal
(define (proposal:resample) (new resample-proposal%))
(define (proposal:drift) (new adaptive-drift-proposal%))

;; default-proposal : (parameterof Proposer)
(define default-proposal (make-parameter proposal:drift))

;; ============================================================

(define proposal<%>
  (interface ()
    propose  ;; Key Zones Dist Value -> (U (cons Value Real) #f)
    info     ;; Nat -> Void
    feedback ;; Key Boolean -> Void
    ))

(define (proposal? x)
  (is-a? x proposal<%>))

(define proposal-base%
  (class* object% (proposal<%>)
    (super-new)
    (field [counter 0])
    (define/public (info i)
      (iprintf i "Count: ~s\n" counter))
    (define/public (propose key zones dist value)
      (set! counter (add1 counter))
      (propose* key zones dist value))
    (abstract propose*)
    (define/public (feedback key success?) (void))
    ))

(define resample-proposal%
  (class proposal-base%
    (inherit-field counter)
    (super-new)
    (define/override (info i)
      (iprintf i "-- Proposal (resample)\n")
      (super info i))
    (define/override (propose* key zones dist value)
      (propose:resample dist value))
    ))

(define static-drift-proposal%
  (class proposal-base%
    (init-field scale-factor)
    (inherit-field counter)
    (super-new)
    (define/override (info i)
      (iprintf i "-- Proposal (drift)\n")
      (super info i))
    (define/override (propose* key zones dist value)
      (propose:drift scale-factor dist value))
    ))

;; ============================================================

;; An Adapt is (adapt Real Nat Nat Nat Nat)
;; - scale-factor is the current scale factor
;; - batch-{trials,successes} is the number of trials/successes since the last adjustment
;; - old-{trials,successes} is the number of trials/successes before the last adjustment
(struct adapt (scale batch-trials batch-successes old-trials old-successes) #:mutable)
(define (adapt-all-trials a) (+ (adapt-batch-trials a) (adapt-old-trials a)))
(define (adapt-all-successes a) (+ (adapt-batch-successes a) (adapt-old-successes a)))

(define ADAPT-BATCH 100)
(define ADAPT-GOAL-HI 0.50)
(define ADAPT-GOAL-LO 0.25)
(define ADAPT-INIT 1.0)
(define ADAPT-UP 1.25)
(define ADAPT-DOWN 0.80)
(define ADAPT-MIN (exp -20))
(define ADAPT-MAX (exp 20))

;; FIXME: alternatives to Address as adaptation key?
;; FIXME: check single-site goal range
;; FIXME: multi-site has different goal range (?), may be fundamentally different

(define adaptive-drift-proposal%
  (class proposal-base%
    (field [table (make-hash)] ;; Hash[ Key => Adapt ]
           [incr-count 0]
           [decr-count 0]
           [stay-count 0])
    (define warned-out-of-range? #f) ;; suppresses multiple warnings
    (super-new)

    (define/override (info i)
      (iprintf i "-- Proposal (adaptive-drift)\n")
      (super info i)
      (iprintf i "Scale increased: ~s\n" incr-count)
      (iprintf i "Scale decreased: ~s\n" decr-count)
      (iprintf i "Scale maintained: ~s\n" stay-count)
      (when (verbose?)
        (for ([(key a) (in-hash table)])
          (iprintf i "- Key ~a: success rate = ~s, scale = ~s\n"
                   (~s key #:max-width 20)
                   (%age (adapt-all-successes a) (adapt-all-trials a))
                   (adapt-scale a)))))

    (define/override (propose* key zones dist value)
      (define a (hash-ref! table key (lambda () (adapt ADAPT-INIT 0 0 0 0))))
      (propose:drift (adapt-scale a) dist value))

    (define/override (feedback key success?)
      (define a (hash-ref table key))
      (set-adapt-batch-trials! a (add1 (adapt-batch-trials a)))
      (when success? (set-adapt-batch-successes! a (add1 (adapt-batch-successes a))))
      (when (> (adapt-batch-trials a) ADAPT-BATCH)
        (cond [(> (adapt-batch-successes a) (* ADAPT-GOAL-HI (adapt-batch-trials a))) ;; high
               ;; (eprintf "increasing scale for ~s from ~s\n" key (adapt-scale a))
               (set! incr-count (add1 incr-count))
               (set-adapt-scale! a (* ADAPT-UP (adapt-scale a)))]
              [(< (adapt-batch-successes a) (* ADAPT-GOAL-LO (adapt-batch-trials a))) ;; low
               ;; (eprintf "decreasing scale for ~s from ~s\n" key (adapt-scale a))
               (set! decr-count (add1 decr-count))
               (set-adapt-scale! a (* ADAPT-DOWN (adapt-scale a)))]
              [else
               (set! stay-count (add1 stay-count))
               (void)])
        (when (> (adapt-scale a) ADAPT-MAX)
          (unless warned-out-of-range?
            (eprintf "adaptive-drift-proposal: scale increased out of range\n")
            (set! warned-out-of-range? #t))
          (set-adapt-scale! a ADAPT-MAX))
        (when (< (adapt-scale a) ADAPT-MIN)
          (unless warned-out-of-range?
            (eprintf "adaptive-drift-proposal: scale decreased out of range\n")
            (set! warned-out-of-range? #t))
          (set-adapt-scale! a ADAPT-MIN))
        (set-adapt-old-trials! a (+ (adapt-old-trials a) (adapt-batch-trials a)))
        (set-adapt-old-successes! a (+ (adapt-old-successes a) (adapt-batch-successes a)))
        (set-adapt-batch-trials! a 0)
        (set-adapt-batch-successes! a 0)))
    ))

;; ============================================================

(define (propose:drift scale-factor dist value)
  (define r (*drift dist value scale-factor))
  (vprintf "DRIFTED from ~e to ~e\n" value (car r))
  r)

(define (propose:resample dist value)
  ;; Just resample from same dist.
  ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
  ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
  (define value* (dist-sample dist))
  (define R (dist-pdf dist value #t))
  (define F (dist-pdf dist value* #t))
  (when (verbose?)
    (vprintf "RESAMPLED from ~e to ~e\n" value value*)
    (vprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
  (cons value* (- R F)))
