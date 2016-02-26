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

    (define/public (accinfo)
      (define total-evals (+ find-slice-counter in-slice-counter))
      (Info "== slice transition"
            ["Zone" zone]
            ["Total runs" run-counter]
            ["Total evals" total-evals]
            [% "Evals to find slice" find-slice-counter total-evals]
            [% "Evals in slice" in-slice-counter total-evals]))

    ;; run : (-> A) Trace -> (cons (U Trace #f) TxInfo)
    (define/public (run thunk last-trace)
      (vprintf "Starting transition (~s)\n" (object-name this%))
      (set! run-counter (add1 run-counter))
      (defmatch (trace _ last-db _ _ _) last-trace)
      (define key-to-change (db-pick-a-key last-db zone))
      (vprintf "key to change = ~s\n" key-to-change)
      (unless key-to-change (error-no-key 'slice zone))
      (define slice
        (new slice%
             (parent this)
             (thunk thunk)
             (last-trace last-trace)
             (key-to-change key-to-change)
             (last-entry (hash-ref last-db key-to-change))))
      (send slice perturb/slice))
    ))

(define slice%
  (class object%
    (init-field parent
                thunk
                last-trace
                key-to-change
                last-entry)
    (super-new)

    (defmatch (entry zones dist value _) last-entry)
    (unless (or (real-dist? dist) (integer-dist? dist))
      (error 'slice "distribution does not support slice sampling\n  dist: ~e" dist))

    (define/private (get-method) (get-field method parent))
    (define/private (get-W) (get-field W parent))
    (define/private (get-integer-W) (max 1 (inexact->exact (round (get-W)))))
    (define/private (get-M) (get-field M parent))
    (define/private (get-small-dist) (get-field small-dist parent))
    (define/private (get-last-dens-dim) (trace-dens-dim last-trace))

    (define/private (incr-find-slice-counter! n)
      (set-field! find-slice-counter parent (+ n (get-field find-slice-counter parent))))
    (define/private (incr-in-slice-counter! n)
      (set-field! in-slice-counter parent (+ n (get-field in-slice-counter parent))))

    ;; ----------------------------------------
    ;; Entry point

    ;; perturb/slice : -> (cons (U Trace #f) TxInfo)
    (define/public (perturb/slice)
      (vprintf "last-ll = ~s\n" (trace-ll last-trace))
      (define threshold (+ (log (random)) (trace-ll last-trace)))
      (vprintf "slice threshold = ~s (logspace ~s)\n" (exp threshold) threshold)
      (vprintf "Finding slice bounds\n")
      ;; inclusive bounds
      (define-values (lo hi)
        (with-verbose> (find-slice-bounds threshold)))
      (select-value* lo hi threshold))

    ;; ----------------------------------------
    ;; Eval trace/ll

    (define trace-cache (make-hash)) ;; (hashof Real => Trace/#f)

    (define/private (make-delta-db value*)
      (hash key-to-change (entry zones dist value* (dist-pdf dist value* #t))))
    (define/private (eval-ll value*)
      (trace-ll/dim (eval-trace value*)))
    (define/private (eval-trace value*)
      (hash-ref! trace-cache value* (lambda () (eval-trace* value*))))
    (define/private (eval-trace* value*)
      (define-values (support-min support-max) (get-support-bounds dist))
      (cond [(<= support-min value* support-max)
             (define delta-db (make-delta-db value*))
             (define ctx
               (new db-stochastic-ctx% 
                    (last-db (trace-db last-trace))
                    (delta-db delta-db)
                    (on-fresh-choice (lambda () (error-structural 'slice key-to-change)))))
             (incr-find-slice-counter! 1)
             (match (send ctx run thunk)
               [(cons 'okay sample-value)
                (define current-trace (send ctx make-trace sample-value))
                (check-not-structural 'slice key-to-change
                                      last-trace current-trace)
                current-trace]
               [(cons 'fail fail-reason)
                #f])]
            [else #f]))

    ;; ----------------------------------------
    ;; Find slice bounds

    (define/private (find-slice-bounds threshold)
      (cond [(small-dist? dist)
             (vprintf "Small integer distribution, using whole support\n")
             (get-support-bounds dist)]
            [(integer-dist? dist)
             (case (get-method)
               [(step) (find-integer-slice-bounds/step threshold)]
               [(double) (find-integer-slice-bounds/double threshold)]
               [(unimodal) (find-slice-bounds/unimodal threshold)])]
            [else
             (case (get-method)
               [(step) (find-slice-bounds/step threshold)]
               [(double) (find-slice-bounds/double threshold)]
               [(unimodal) (find-slice-bounds/unimodal threshold)])]))

    ;; Implements the stepping-out method from
    ;; http://people.ee.duke.edu/~lcarin/slice.pdf
    (define/private (find-slice-bounds/step threshold)
      (define W (get-W))
      (define M (get-M))
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
             [lo-k (if (= M +inf.0) +inf.0 (random M))]
             [hi-k (if (= M +inf.0) +inf.0 (- M 1 lo-k))])
        (values (loop-lo lo-k lo (eval-ll lo))
                (loop-hi hi-k hi (eval-ll hi)))))

    (define/private (find-integer-slice-bounds/step threshold)
      (define M (get-M))
      (define Wint (get-integer-W))
      (define (loop-lo k lo lo-ll)
        (if (or (zero? k) (< lo-ll threshold))
            lo
            (let ([lo* (- lo Wint)])
              (vprintf "Stepping to ~e for lower bound\n" lo*)
              (loop-lo (sub1 k) lo* (with-verbose> (eval-ll lo*))))))
      (define (loop-hi k hi hi-ll)
        (if (or (zero? k) (< hi-ll threshold))
            hi
            (let ([hi* (+ hi Wint)])
              (vprintf "Stepping to ~e for upper bound\n" hi*)
              (loop-hi (sub1 k) hi* (with-verbose> (eval-ll hi*))))))
      (let* ([u (inexact->exact (round (* (random) Wint)))]
             [lo (- value u)]
             [hi (+ value Wint (- u))]
             [lo-k (if (= M +inf.0) +inf.0 (random M))]
             [hi-k (if (= M +inf.0) +inf.0 (- M 1 lo-k))])
        (values (loop-lo lo-k lo (eval-ll lo))
                (loop-hi hi-k hi (eval-ll hi)))))

    ;; Implements the doubling method from
    ;; http://people.ee.duke.edu/~lcarin/slice.pdf
    (define/public (find-slice-bounds/double threshold)
      (define W (get-W))
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

    (define/public (find-integer-slice-bounds/double threshold)
      (define Wint (get-integer-W))
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
      (let* ([u (inexact->exact (round (* (random) Wint)))]
             [lo (- value u)]
             [hi (+ value Wint (- u))]
             [lo-ll (eval-ll lo)]
             [hi-ll (eval-ll hi)])
        (loop lo lo-ll hi hi-ll)))

    ;; Implements a simpler doubling suitable only for unimodal distributions
    (define/public (find-slice-bounds/unimodal threshold)
      (define W (get-W))
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
      (define halfW
        (if (integer-dist? dist)
            (inexact->exact (ceiling (* 0.5 W)))
            (* 0.5 W)))
      (let* ([lo (- value halfW)]
             [hi (+ value halfW)])
        (values (loop-lo lo (eval-ll lo))
                (loop-hi hi (eval-ll hi)))))

    ;; ----------------------------------------
    ;; Select value in slice

    ;; select-value* : Real Real Real -> (cons (U Trace #f) TxInfo)
    (define/private (select-value* lo0 hi0 threshold)
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
        (incr-in-slice-counter! 1)
        (define current-trace (with-verbose> (eval-trace value*)))
        (define current-ll (trace-ll/dim current-trace))
        (cond [(and current-trace
                    (> current-ll threshold)
                    (acceptable? value* lo0 hi0 threshold))
               ;; FIXME: add more info to TxInfo
               (cons current-trace (vector 'slice key-to-change lo0 hi0))]
              [else
               (vprintf "Retrying (~a)\n"
                        (cond [(and current-trace
                                    (> current-ll threshold))
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

    (define/private (acceptable? value* lo hi threshold)
      (or (small-dist? dist)
          (case (get-method)
            [(step unimodal) #t]
            [(double)
             (if (integer-dist? dist)
                 (acceptable?/double/integer value* lo hi threshold)
                 (acceptable?/double value* lo hi threshold))])))

    (define/private (acceptable?/double value* lo hi threshold)
      (define W (get-W))
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

    (define/private (acceptable?/double/integer value* lo hi threshold)
      (define Wint (get-integer-W))
      (let loop ([lo lo] [hi hi])
        (cond [(<= (- hi lo) Wint)
               #t]
              [else
               (define mid (round (/ (+ lo hi) 2)))
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

    ;; ----------------------------------------
    ;; Utils

    (define/private (small-dist? dist)
      (match (dist-support dist)
        [(integer-range min max) (< (- max min) (get-small-dist))]
        [_ #f]))

    (define/private (trace-ll/dim t)
      (if (trace? t)
          (ll+dim->ll (trace-ll t) (- (trace-dens-dim t) (get-last-dens-dim)))
          -inf.0))
    ))

(define (trace-ll* t)
  (if (trace? t) (trace-ll t) -inf.0))

(define (get-support-bounds dist)
  (match (dist-support dist)
    [(integer-range min max) (values min max)]
    [(real-range min max) (values min max)]
    [_ (values -inf.0 +inf.0)]))
