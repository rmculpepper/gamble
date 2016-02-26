;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/list
         (rename-in racket/match [match-define defmatch])
         "db.rkt"
         "../interfaces.rkt"
         "interfaces.rkt"
         "../context.rkt"
         "../dist.rkt")
(provide (all-defined-out)
         (all-from-out "interfaces.rkt"))

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

    ;; run : (-> A) Trace -> (cons (U Trace #f) TxInfo)
    (define/public (run thunk last-trace)
      (vprintf "Starting transition (~s)\n" (object-name this%))
      (match (run* thunk last-trace)
        [(list* (? real? threshold) trace txinfo)
         (vprintf "accept threshold = ~s (density dimension ~s -> ~s)\n"
                  (exp threshold) (trace-dens-dim last-trace) (trace-dens-dim trace))
         (define u (log (random)))
         (cond [(< u threshold)
                (vprintf "Accepted MH step with ~s\n" (exp u))
                (set! accepts (add1 accepts))
                (feedback #t)
                (cons trace txinfo)]
               [else
                (vprintf "Rejected MH step with ~s\n" (exp u))
                (set! mh-rejects (add1 mh-rejects))
                (feedback #f)
                (cons #f txinfo)])]
        [(list* 'fail reason txinfo)
         (set! cond-rejects (add1 cond-rejects))
         (feedback #f)
         (vprintf "Rejected condition, reason = ~e\n" reason)
         (cons #f txinfo)]))

    ;; run* : (-> A) Trace -> (U (list* Real Trace TxInfo) (cons 'fail any TxInfo))
    (abstract run*)

    ;; feedback : Boolean -> Void
    (define/public (feedback success?) (void))
    ))

;; ll+dim->ll : Real Int -> Real
;; Takes a log likelihood and a density dimension difference (current - last)
;; and converts them into a single extended real.
;; A positive density dimension change means infinitely less likely.
;; A negative density dimension change means infintely more likely.
(define (ll+dim->ll ll dens-dim-diff)
  (cond [(zero? dens-dim-diff)
         ll]
        [(positive? dens-dim-diff)
         -inf.0]
        [(negative? dens-dim-diff)
         +inf.0]))

;; ============================================================

(define (error-no-key who zone)
  (error who "no random choice available to change~a"
         (if zone (format "\n  zone: ~e" zone) "")))

(define (error-structural who key)
  (error who "illegal transition for structural choice\n  choice: ~a"
         (address->string key)))

(define (check-not-structural who key last-trace current-trace)
  (unless (= (trace-nchoices current-trace) (trace-nchoices last-trace))
    ;; Note: This check is not sufficient to catch all structural choices;
    ;; if one choice is lost and another added, nchoices stays the same.
    ;; See also disallow-fresh? in db-stochastic-ctx%.
    (error-structural who key)))
