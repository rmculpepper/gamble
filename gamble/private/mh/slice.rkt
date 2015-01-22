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

(define slice-mh-transition%
  (class* object% (mh-transition<%>)
    (init-field zone
                [method 'double] ;; (U 'step 'double 'unimodal)
                [W 1.0]          ;; slice search width
                [M +inf.0]       ;; max # of widths to grow slice by
                [small-dist 10])  ;; limit of small-dist optimization, 0 to disable
    (field [run-counter 0]
           [find-slice-counter 0]
           [in-slice-counter 0])
    (super-new)

    (define/public (info i)
      (iprintf i "== Transition (slice ... #:zone ~e)\n" zone)
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
         (unless (or (real-dist? dist) (integer-dist? dist))
           (error 'slice "distribution does not support slice sampling\n  dist: ~e" dist))
         (perturb/slice key-to-change dist value zones thunk last-trace)]))

    (define/private (perturb/slice key-to-change dist value zones thunk last-trace)
      (define threshold (+ (log (random)) (trace-ll last-trace)))
      (vprintf "slice threshold = ~s\n" (exp threshold))
      (define-values (support-min support-max) (get-support-bounds dist))
      (define trace-cache (make-hash)) ;; (hashof Real => Trace/#f)
      (define (eval-trace value*)
        (hash-ref! trace-cache value* (lambda () (eval-trace* value*))))
      (define (eval-trace* value*)
        (cond [(<= support-min value* support-max)
               (define delta-db 
                 (hash key-to-change (entry zones dist value* (dist-pdf dist value* #t) #f)))
               (define ctx
                 (new db-stochastic-ctx% 
                      (last-db (trace-db last-trace))
                      (delta-db delta-db)
                      (record-obs? #f)
                      (on-fresh-choice (lambda () (error-structural 'slice key-to-change)))))
               (set! find-slice-counter (add1 find-slice-counter))
               (match (send ctx run thunk)
                 [(cons 'okay sample-value)
                  (check-not-structural 'slice key-to-change (get-field nchoices ctx) last-trace)
                  (define current-db (get-field current-db ctx))
                  (define nchoices (get-field nchoices ctx))
                  (define ll-free (get-field ll-free ctx))
                  (define ll-obs (get-field ll-obs ctx))
                  (trace sample-value current-db nchoices ll-free ll-obs)]
                 [(cons 'fail fail-reason)
                  #f])]
              [else #f]))
      (define (eval-ll value*)
        (trace-ll* (eval-trace value*)))
      (vprintf "Finding slice bounds\n")
      ;; inclusive bounds
      (define-values (lo hi)
        (with-verbose> (find-slice-bounds dist value eval-ll threshold)))
      (select-value* dist value lo hi eval-trace threshold))

    (define/private (find-slice-bounds dist value eval-ll threshold)
      (cond [(small-dist? dist)
             (vprintf "Small integer distribution, using whole support\n")
             (get-support-bounds dist)]
            [else
             (case method
               [(step) (find-slice-bounds/step dist value eval-ll threshold)]
               [(double) (find-slice-bounds/double dist value eval-ll threshold)]
               [(unimodal) (find-slice-bounds/unimodal dist value eval-ll threshold)])]))

    ;; Implements the stepping-out method from
    ;; http://people.ee.duke.edu/~lcarin/slice.pdf
    (define/private (find-slice-bounds/step dist value eval-ll threshold)
      (define (loop-lo k lo lo-ll)
        (if (or (zero? k) (< lo-ll threshold))
            lo
            (let ([lo* (- lo W)])
              (vprintf "Stepping to ~e for lower bound\n" lo*)
              (loop-lo (sub1 k) lo* (with-verbose> (eval-ll lo*))))))
      (define (loop-hi k hi hi-ll)
        (if (or (zero? k) (< hi-ll threshold))
            hi
            (let ([hi* (+ hi W)])
              (vprintf "Stepping to ~e for upper bound\n" hi*)
              (loop-hi (sub1 k) hi* (with-verbose> (eval-ll hi*))))))
      (let* ([u (* (random) W)]
             [lo (- value u)]
             [hi (+ value W (- u))]
             [ku (random)]
             [lo-k (if (= M +inf.0) +inf.0 (random M))]
             [hi-k (if (= M +inf.0) +inf.0 (- M 1 lo-k))])
        (values (loop-lo lo-k lo (eval-ll lo))
                (loop-hi hi-k hi (eval-ll hi)))))

    ;; Implements the doubling method from
    ;; http://people.ee.duke.edu/~lcarin/slice.pdf
    (define/public (find-slice-bounds/double dist value eval-ll threshold)
      (define (loop lo lo-ll hi hi-ll)
        (if (and (< lo-ll threshold) (< hi-ll threshold))
            (values lo hi)
            (if (zero? (random 2))
                (let ([lo* (- lo (- hi lo))])
                  (vprintf "Stepping to ~e for lower bound\n" lo*)
                  (loop lo* (with-verbose> (eval-ll lo*)) hi hi-ll))
                (let ([hi* (+ hi (- hi lo))])
                  (vprintf "Stepping to ~e for upper bound\n" hi*)
                  (loop lo lo-ll hi* (with-verbose> (eval-ll hi*)))))))
      (let* ([u (* (random) W)]
             [lo (- value u)]
             [hi (+ value W (- u))]
             [lo-ll (eval-ll lo)]
             [hi-ll (eval-ll hi)])
        (loop lo lo-ll hi hi-ll)))

    ;; Implements a simpler doubling suitable only for unimodal distributions
    (define/public (find-slice-bounds/unimodal dist value eval-ll threshold)
      (define (loop-lo k lo lo-ll)
        (if (< lo-ll threshold)
            lo
            (let ([lo* (- lo (- value lo))])
              (vprintf "Stepping to ~e for lower bound\n" lo*)
              (loop-lo lo* (with-verbose> (eval-ll lo*))))))
      (define (loop-hi k hi hi-ll)
        (if (< hi-ll threshold)
            hi
            (let ([hi* (+ hi (- hi value))])
              (vprintf "Stepping to ~e for upper bound\n" hi*)
              (loop-hi hi* (with-verbose> (eval-ll hi*))))))
      (let* ([lo (- value (* 0.5 W))]
             [hi (+ value (* 0.5 W))])
        (values (loop-lo lo (eval-ll lo))
                (loop-hi hi (eval-ll hi)))))

    (define/private (select-value* dist value lo0 hi0 eval-trace threshold)
      (define-values (support-min support-max) (get-support-bounds dist))
      (define lo (max lo0 support-min))
      (define hi (min hi0 support-max))
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
        (vprintf "Trying value* = ~e\n" value*)
        (set! in-slice-counter (add1 in-slice-counter))
        (define current-trace (with-verbose> (eval-trace value*)))
        (cond [(and current-trace
                    (> (trace-ll current-trace) threshold)
                    (acceptable? dist value value* lo0 hi0 eval-trace threshold))
               current-trace]
              [else
               (vprintf "Retrying (~a)\n"
                        (cond [(and current-trace (> (trace-ll current-trace) threshold))
                               "value not acceptable"]
                              [current-trace
                               "below threshold"]
                              [else
                               "trace failed"]))
               (if (integer-dist? dist)
                   (if (< value* value)
                       (loop (add1 value*) hi)
                       (loop lo (sub1 value*)))
                   (if (< value* value)
                       (loop value* hi)
                       (loop lo value*)))])))

    (define/private (acceptable? dist value value* lo hi eval-trace threshold)
      (or (small-dist? dist)
          (case method
            [(step unimodal) #t]
            [(double) (acceptable?/double value value* lo hi eval-trace threshold)])))

    (define/private (acceptable?/double value value* lo hi eval-trace threshold)
      (define (eval-ll x) (trace-ll* (eval-trace x)))
      (let loop ([lo lo] [hi hi])
        (cond [(< (- hi lo) (* W 1.1)) ;; to prevent rounding problems
               #t]
              [else
               (define mid (* 0.5 (+ lo hi)))
               (define D
                 (or (and (<  value mid) (>= value* mid))
                     (and (>= value mid) (<  value* mid))))
               (define lo* (if (< value* mid) lo mid))
               (define hi* (if (< value* mid) mid hi))
               (if (and D
                        (<= (eval-ll lo*) threshold)
                        (<= (eval-ll hi*) threshold))
                   #f ;; not acceptable
                   (loop lo* hi*))])))

    (define/private (get-support-bounds dist)
      (match (dist-support dist)
        [(integer-range min max) (values min max)]
        [(real-range min max) (values min max)]
        [_ (values -inf.0 +inf.0)]))

    (define/private (small-dist? dist)
      (match (dist-support dist)
        [(integer-range min max) (< (- max min) small-dist)]
        [_ #f]))

    (define/public (feedback success?) (void))
    ))

(define (trace-ll* t)
  (if (trace? t) (trace-ll t) -inf.0))
