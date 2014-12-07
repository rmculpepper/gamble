;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         "db.rkt"
         "../interfaces.rkt"
         "../dist.rkt"
         "base.rkt")
(provide (all-defined-out))

;; ============================================================

(define single-site-slice-mh-transition%
  (class* object% (mh-transition<%>)
    (init-field zone
                scale-factor)
    (field [run-counter 0]
           [find-slice-counter 0]
           [in-slice-counter 0])
    (super-new)

    (define/public (info i)
      (iprintf i "== Transition (slice #:scale ~e #:zone ~e)\n" scale-factor zone)
      (iprintf i "Total runs: ~s\n" run-counter)
      (iprintf i "Evals to find slice: ~s\n" find-slice-counter)
      (iprintf i "Evals in slice: ~s\n" in-slice-counter))

    ;; run : (-> A) Trace -> TransitionResult
    (define/public (run thunk last-trace)
      (vprintf "Starting transition (~s)\n" (object-name this%))
      (set! run-counter (add1 run-counter))
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (define key-to-change (pick-a-key last-nchoices last-db zone))
      (vprintf "key to change = ~s\n" key-to-change)
      (unless key-to-change (error-no-key 'slice zone))
      (match (hash-ref last-db key-to-change)
        [(entry zones dist value ll #f)
         (unless (slice-dist? dist)
           (error 'slice "distribution does not support slice sampling\n  dist: ~e" dist))
         (perturb/slice key-to-change dist value zones thunk last-trace)]))

    (define/private (perturb/slice key-to-change dist value zones thunk last-trace)
      (define (make-entry value*)
        (entry zones dist value* (dist-pdf dist value* #t) #f))
      (define threshold (log (* (random) (exp (trace-ll last-trace)))))
      (vprintf "slice threshold = ~s\n" (exp threshold))
      (define-values (support-min support-max)
        (match (dist-support dist)
          [(integer-range min max) (values min max)]
          [(real-range min max) (values min max)]
          [_ (values -inf.0 +inf.0)]))
      (define (find-slice-bound value W)
        (define value* (real-dist-adjust-value dist value W))
        ;; clip value* to [support-min, support-max]
        (cond [(<= value* support-min) support-min]
              [(>= value* support-max) support-max]
              [else (find-slice-bound* value* W)]))
      (define (find-slice-bound* value* W)
        (define entry* (make-entry value*))
        (define delta-db (hash key-to-change entry*))
        (define ctx
          (new db-stochastic-ctx% 
               (last-db (trace-db last-trace))
               (delta-db delta-db)
               (record-obs? #f)))
        (set! find-slice-counter (add1 find-slice-counter))
        (match (send ctx run thunk)
          [(cons 'okay sample-value)
           (define ll (+ (get-field ll-obs ctx) (get-field ll-free ctx)))
           (check-not-structural 'slice (get-field nchoices ctx) last-trace)
           (cond [(<= ll threshold)
                  value*]
                 [else
                  (find-slice-bound value* W)])]
          [(cons 'fail fail-reason)
           value*]))
      ;; inclusive bounds
      (vprintf "Finding slice lower bound\n")
      (define lo (with-verbose> (find-slice-bound value (- scale-factor))))
      (vprintf "Finding slice upper bound\n")
      (define hi (with-verbose> (find-slice-bound value scale-factor)))
      (let loop ([lo lo] [hi hi])
        (vprintf "slice = [~s, ~s]\n" lo hi)
        (define value*
          (cond [(> lo hi)
                 (error 'slice "empty slice; program is inconsistent with last trace")]
                [(= lo hi) lo]
                [(integer-dist? dist)
                 (+ lo (random (add1 (inexact->exact (- hi lo)))))]
                [else
                 (+ lo (* (random) (- hi lo)))]))
        (define entry* (entry zones dist value* (dist-pdf dist value* #t) #f))
        (define delta-db (hash key-to-change entry*))
        (define ctx
          (new db-stochastic-ctx% 
               (last-db (trace-db last-trace))
               (delta-db delta-db)
               (record-obs? #f)))
        (set! in-slice-counter (add1 in-slice-counter))
        (define (retry)
          (if (integer-dist? dist)
              (if (< value* value)
                  (loop (add1 value*) hi)
                  (loop lo (sub1 value*)))
              (if (< value* value)
                  (loop value* hi)
                  (loop lo value*))))
        (vprintf "Trying value* = ~e\n" value*)
        (match (with-verbose> (send ctx run thunk))
          [(cons 'okay sample-value)
           (define ll (+ (get-field ll-obs ctx) (get-field ll-free ctx)))
           (cond [(> ll threshold)
                  ;; ok
                  (define current-db (get-field current-db ctx))
                  (define nchoices (get-field nchoices ctx))
                  (define ll-free (get-field ll-free ctx))
                  (define ll-obs (get-field ll-obs ctx))
                  (define current-trace
                    (trace sample-value current-db nchoices ll-free ll-obs))
                  (check-not-structural 'slice nchoices last-trace)
                  current-trace]
                 [else ;; whoops, found hole in slice interval
                  (vprintf "Retry (found dip in slice)")
                  (retry)])]
          [(cons 'fail fail-reason)
           (vprintf "Retry (found hole in slice)\n")
           (retry)])))

    (define/public (feedback success?) (void))
    ))
