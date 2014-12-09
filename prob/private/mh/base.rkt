;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         "db.rkt"
         "../interfaces.rkt"
         "../dist.rkt")
(provide (all-defined-out))

;; ============================================================

(define mh-transition<%>
  (interface ()
    run  ;; (-> A) Trace -> TransitionResult
    info ;; Nat -> Void
    feedback ;; Boolean -> Void
    ))

;; A TransitionResult is one of
;; - Trace  -- run completed, threshold already checked (if applicable)
;; - #f     -- run failed or mh-rejected

;; A Trace is (trace Any DB Nat Real Real)
(struct trace (value db nchoices ll-free ll-obs))
(define init-trace (trace #f '#hash() 0 0 0))

;; ============================================================

(define mh-transition-base%
  (class* object% (mh-transition<%>)
    (field [accepts 0]
           [mh-rejects 0]
           [cond-rejects 0])
    (super-new)

    (define/public (info i)
      (define total (+ accepts cond-rejects mh-rejects))
      (iprintf i "Total runs: ~s\n" total)
      (iprintf i "Accepted traces: ~s, ~a%\n"
               accepts (%age accepts total))
      (iprintf i "Traces rejected by condition: ~s, ~a%\n"
               cond-rejects (%age cond-rejects total))
      (iprintf i "Traces rejected by MH threshold: ~s, ~a%\n"
               mh-rejects (%age mh-rejects total)))

    ;; run : (-> A) Trace -> TransitionResult
    (define/public (run thunk last-trace)
      (vprintf "Starting transition (~s)\n" (object-name this%))
      (match (run* thunk last-trace)
        [(cons (? real? threshold) trace)
         (vprintf "accept threshold = ~s\n" (exp threshold))
         (define u (log (random)))
         (cond [(< u threshold)
                (vprintf "Accepted MH step with ~s\n" (exp u))
                (set! accepts (add1 accepts))
                (feedback #t)
                trace]
               [else
                (vprintf "Rejected MH step with ~s\n" (exp u))
                (set! mh-rejects (add1 mh-rejects))
                (feedback #f)
                #f])]
        [(cons 'fail reason)
         (set! cond-rejects (add1 cond-rejects))
         (feedback #f)
         (vprintf "Rejected condition, reason = ~e\n" reason)
         #f]))

    ;; run* : (-> A) Trace -> (U (cons Real Trace) (cons 'fail any))
    (abstract run*)

    ;; feedback : Boolean -> Void
    (define/public (feedback success?) (void))
    ))

;; ============================================================

(define (trace-ll t) (+ (trace-ll-free t) (trace-ll-obs t)))

(define (error-no-key who zone)
  (error who "no random choice available to change~a"
         (if zone (format "\n  zone: ~e" zone) "")))

(define (error-structural who)
  (error who "illegal for structural choice"))

(define (check-not-structural who nchoices last-trace)
  (unless (= nchoices (trace-nchoices last-trace))
    ;; Note: This check is not sufficient to catch all structural choices;
    ;; if one choice is lost and another added, nchoices stays the same.
    ;; See also disallow-fresh? in db-stochastic-ctx%.
    (error-structural who)))
