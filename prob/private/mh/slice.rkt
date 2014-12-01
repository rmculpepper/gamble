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
      (set! run-counter (add1 run-counter))
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (define key-to-change (pick-a-key last-nchoices last-db zone))
      (when (verbose?)
        (eprintf "# perturb: changing ~s\n" key-to-change))
      (unless key-to-change
        (error 'slice:run "no key to change"))
      (match (hash-ref last-db key-to-change)
        [(entry zones dist value ll #f)
         (unless (slice-dist? dist)
           (error 'slice:run "chosen distribution does not support slice sampling\n  dist: ~e" dist))
         (perturb/slice key-to-change dist value zones thunk last-trace)]))

    (define/private (perturb/slice key-to-change dist value zones thunk last-trace)
      (define (make-entry value*)
        (entry zones dist value* (dist-pdf dist value* #t) #f))
      (define threshold (log (* (random) (exp (trace-ll last-trace)))))
      (when (verbose?)
        (eprintf "* slice threshold = ~s\n" (exp threshold)))
      (define (find-slice-bound value W)
        (define value* (real-dist-adjust-value dist value W))
        (define entry* (entry zones dist value* (dist-pdf dist value* #t) #f))
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
           (cond [(<= ll threshold)
                  value*]
                 [else
                  (find-slice-bound value* W)])]
          [(cons 'fail fail-reason)
           value*]))
      (define lo (find-slice-bound value (- scale-factor)))
      (define hi (find-slice-bound value scale-factor))
      (let loop ([lo lo] [hi hi])
        (when (verbose?)
          (eprintf "* slice [~s, ~s]\n" lo hi))
        (define value* (+ lo (* (random) (- hi lo))))
        (define entry* (entry zones dist value* (dist-pdf dist value* #t) #f))
        (define delta-db (hash key-to-change entry*))
        (define ctx
          (new db-stochastic-ctx% 
               (last-db (trace-db last-trace))
               (delta-db delta-db)
               (record-obs? #f)))
        (set! in-slice-counter (add1 in-slice-counter))
        (match (send ctx run thunk)
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
                  (define threshold 1)
                  current-trace]
                 [else ;; whoops, found hole in slice interval
                  (if (< value* value)
                      (loop value* hi)
                      (loop lo value*))])]
          [(cons 'fail fail-reason)
           (if (< value* value)
               (loop value* hi)
               (loop lo value*))])))
    ))
