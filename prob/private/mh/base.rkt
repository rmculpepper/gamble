;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/list
         (rename-in racket/match [match-define defmatch])
         "db.rkt"
         "../interfaces.rkt"
         "../context.rkt"
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

(define single-selector<%>
  (interface ()
    select-one ;; Trace -> Addr
    info       ;; Nat -> Void
    ))

(define select:one-random%
  (class* object% (single-selector<%>)
    (super-new)

    (define/public (info i)
      (iprintf i "-- Random selector\n"))

    (define/public (select-one last-trace zone)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (define nchoices/zone
        (cond [(eq? zone #f) last-nchoices]
              [else (db-count-unpinned last-db #:zone zone)]))
      (and (positive? nchoices/zone)
           (db-nth-unpinned last-db (random nchoices/zone) #:zone zone)))
    ))

;; Note: approximates "round-robin" by selecting least key greater
;; than last key each time.
(define select:one-round-robin%
  (class* object% (single-selector<%>)
    (field [last-key #f]
           [rounds 0])
    (super-new)

    (define/public (info i)
      (iprintf i "-- Round-robin selector\n")
      (iprintf i "Rounds started: ~s\n" rounds))

    (define/public (select-one last-trace zone)
      (vprintf "last-key = ~s\n" last-key)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      ;; Take first key > last-key; or failing that, take first key.
      (define-values (first-key next-key)
        (for/fold ([first-key #f] [next-key #f])
                  ([(key entry) (in-hash last-db)]
                   #:when (not (entry-pinned? entry))
                   #:when (entry-in-zone? entry zone))
          (values (cond [(not first-key) key]
                        [(address<? key first-key) key]
                        [else first-key])
                  (cond [(not last-key) #f]
                        [(and (not next-key)
                              (address<? last-key key))
                         key]
                        [(and (address<? last-key key)
                              (address<? key next-key))
                         key]
                        [else next-key]))))
      (cond [next-key
             (vprintf "using next-key\n")
             (set! last-key next-key)
             next-key]
            [else
             (vprintf "using first-key\n")
             (set! last-key first-key)
             (set! rounds (add1 rounds))
             first-key]))
    ))

;; FIXME: comparison can probably be made faster by writing custom
;; loop, if it matters

(define (address<? a b) (eq? (address-cmp a b) '<))

(define (address-cmp a b)
  (define lena (length a))
  (define lenb (length b))
  (define len (min lena lenb))
  (case (address-cmp* (take-right a len) (take-right b len))
    [(<) '<]
    [(>) '>]
    [(=) (cond [(< lena lenb) '<]
               [(> lena lenb) '>]
               [else '=])]))

(define (address-cmp* a b) ;; equal lengths
  (cond [(and (pair? a) (pair? b))
         (case (address-cmp* (cdr a) (cdr b))
           [(<) '<]
           [(>) '>]
           [(=) (cond [(< (car a) (car b)) '<]
                      [(> (car a) (car b)) '>]
                      [else '=])])]
        [else '=]))

;; ============================================================

(define (trace-ll t) (+ (trace-ll-free t) (trace-ll-obs t)))

(define (error-no-key who zone)
  (error who "no random choice available to change~a"
         (if zone (format "\n  zone: ~e" zone) "")))

(define (error-structural who key)
  (error who "illegal transition for structural choice\n  choice: ~a"
         (address->string key)))

(define (check-not-structural who key nchoices last-trace)
  (unless (= nchoices (trace-nchoices last-trace))
    ;; Note: This check is not sufficient to catch all structural choices;
    ;; if one choice is lost and another added, nchoices stays the same.
    ;; See also disallow-fresh? in db-stochastic-ctx%.
    (error-structural who key)))
