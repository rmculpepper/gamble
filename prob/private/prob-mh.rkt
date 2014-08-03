;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/list
         racket/class
         (rename-in racket/match [match-define defmatch])
         data/order
         "context.rkt"
         "db.rkt"
         "util.rkt"
         "interfaces.rkt"
         "../dist.rkt"
         (only-in "dist.rkt" dists-same-type?)
         "prob-util.rkt")
(provide mh-sampler*)

#|
MH Acceptance

Let X, X' be full traces (dbs)

X  = Xsame  U Xdiff  U Xstale  U Xfresh
X' = Xsame' U Xdiff' U Xstale' U Xfresh'

  Xsame = Xsame' -- part of trace that stayed same
  Xdiff != Xdiff' -- part of trace w/ changed params, kept value
                  -- OR is the directly perturbed part
  Xstale -- part of old trace not reused
  Xfresh' -- new part of new trace

  Note: Xfresh = empty, Xstale' = empty

MH acceptance ratio (from Sean):

  P(X' | Obs)   Q(X | X')
  ----------- * ---------
  P(x  | Obs)   Q(X' | X)

  where Q is whole-trace proposal distribution

MH acceptance ratio from Bher paper:

  P(X')   Kt(x|x', theta)   P(Xstale)
  ----- * --------------- * ----------
  P(X)    Kt(x'|x, theta)   P(Xfresh')

  where x,x' are values of a single ERP

Many things cancel, resulting in

  P(Xdiff')   Kt(x|x', theta)
  --------- * ---------------
  P(Xdiff)    Kt(x'|x, theta)

So, need to accumulate

  lldiff = log Kt(x|x', theta) - log Kt(x'|x, theta)
           + log P(Xdiff') - log P(Xdiff')

depending on only choices reused w/ different params.
|#

;; default-reject-mode : one of the following
;; - 'retry-from-top : re-pick db key to change on rejection
;; - 'retry/same-choice : keep same db key to change on rejection
;; - 'last : replay last state (ie, produce same sample as prev)
(define default-reject-mode 'last)

;; default-threshold-mode : one of the following
;; - 'retain : R - F + ll_new - ll_old + ll_stale - ll_fresh
;; - 'purge : same, but purge unused entries from db
(define default-threshold-mode 'purge)

;; default-perturb-mode : list containing some subset of the following
;; - 'normal : use specialized proposal distribution for normal dist
;; (the default is to simply resample)
(define default-perturb-mode '(normal))

;; ----

(define (mh-sampler* thunk spconds)
  (new mh-sampler% (thunk thunk) (spconds spconds)))

(define mh-sampler%
  (class sampler-base%
    (init-field thunk
                spconds)
    (field [last-db '#hash()]
           [last-nchoices 0]
           [accepts 0]
           [cond-rejects 0]
           [mh-rejects 0]
           [perturb-mode default-perturb-mode]
           [reject-mode default-reject-mode]
           [threshold-mode default-threshold-mode]
           [last-sample #f])
    (super-new)

    (define/public (get-modes) (values reject-mode threshold-mode))
    (define/public (set-modes! reject-mode* threshold-mode*)
      (when reject-mode* (set! reject-mode reject-mode*))
      (when threshold-mode* (set! threshold-mode threshold-mode*)))

    (define/override (sample)
      (sample/picked-key (pick-a-key last-nchoices last-db)))

    (define/private (sample/picked-key key-to-change)
      (define (reject)
        (case reject-mode
          [(retry-from-top) (sample)]
          [(retry/same-choice) (sample/picked-key key-to-change)]
          [(last) last-sample]))
      (when (verbose?)
        (eprintf "# perturb: changing ~s\n" key-to-change))
      (defmatch (cons delta-db R-F) (perturb key-to-change))
      (define ctx
        (new db-stochastic-ctx%
             (last-db last-db)
             (delta-db delta-db)
             (spconds spconds)))
      ;; Run program
      (define result (send ctx run thunk))
      (define current-db (get-field current-db ctx))
      (define nchoices (get-field nchoices ctx))
      (define ll-diff (get-field ll-diff ctx))
      ;; Accept/reject
      (match result
        [(cons 'okay sample-value)
         (define threshold (accept-threshold R-F nchoices last-nchoices ll-diff))
         (when (verbose?)
           (eprintf "# accept threshold = ~s\n" (exp threshold)))
         (define u (log (random)))
         (cond [(< u threshold)
                (when (verbose?)
                  (eprintf "# Accepted MH step with ~s\n" (exp u)))
                (when (eq? threshold-mode 'retain)
                  ;; If retaining, copy stale choices to current-db.
                  (db-copy-stale last-db current-db))
                (set! last-db current-db)
                (set! last-nchoices nchoices)
                (set! accepts (add1 accepts))
                (set! last-sample sample-value)
                (when (verbose?)
                  (eprintf "# Accepted condition\n"))
                sample-value]
               [else
                (set! mh-rejects (add1 mh-rejects))
                (when (verbose?)
                  (eprintf "# Rejected MH step with ~s\n" (exp u)))
                (reject)])]
        [(cons 'fail fail-reason)
         (set! cond-rejects (add1 cond-rejects))
         (when (verbose?)
           (eprintf "# Rejected condition (~s)" fail-reason))
         (reject)]))

    ;; perturb : (U Address #f) -> (cons DB Real)
    ;; Updates db and returns (log) R-F.
    (define/private (perturb key-to-change)
      (if key-to-change
          (match (hash-ref last-db key-to-change)
            [(entry dist value ll #f)
             (or (perturb/proposal key-to-change dist value)
                 (perturb/resample key-to-change dist value))])
          (cons '#hash() 0)))

    ;; perturb/resample : Address Dist Any -> (cons DB Real)
    (define/private (perturb/resample key-to-change dist value)
      ;; Fallback: Just resample from same dist.
      ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
      ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
      (define value* (dist-sample dist))
      (define R (dist-pdf dist value #t))
      (define F (dist-pdf dist value* #t))
      (when (verbose?)
        (eprintf "  RESAMPLED from ~e to ~e\n" value value*)
        (eprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
      (cons (hash key-to-change (entry dist value* F #f)) (- R F)))

    ;; perturb/proposal : Address Dist Any -> (U (cons DB Real) #f)
    ;; If applicable proposal dist avail, updates db and returns (log) R-F,
    ;; otherwise returns #f and perturb! should just resample.
    (define/private (perturb/proposal key-to-change dist value)
      ;; return : Real Real Any -> Real
      (define (return R F value*)
        (when (verbose?)
          (eprintf "  PROPOSED from ~e to ~e\n" value value*)
          (eprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
        (define ll* (dist-pdf dist value* #t))
        (cons (hash key-to-change (entry dist value* ll* #f)) (- R F)))
      (match dist
        [(normal-dist mean stddev)
         (and (memq 'normal perturb-mode)
              (let ()
                (define forward-dist
                  (normal-dist value (/ stddev 4.0)))
                (define value* (dist-sample forward-dist))
                (define backward-dist
                  (normal-dist value* (/ stddev 4.0)))
                (define R (dist-pdf backward-dist value #t))
                (define F (dist-pdf forward-dist value* #t))
                (return R F value*)))]
        [_ #f]))

    ;; accept-threshold : ... -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/private (accept-threshold R-F nchoices last-nchoices ll-diff)
      (if (zero? last-nchoices)
          +inf.0
          (+ R-F (accept-threshold/nchoices nchoices last-nchoices) ll-diff)))

    (define/private (accept-threshold/nchoices nchoices last-nchoices)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      ;; Note: assumes we pick uniformly from all choices.
      (case threshold-mode
        [(purge)
         ;; R = (log (/ 1 (hash-count current-db))) = (- (log ....))
         ;; F = (log (/ 1 (hash-count last-db)))    = (- (log ....))
         ;; convert to inexact so (log 0.0) = -inf.0
         (define R (- (log (exact->inexact nchoices))))
         (define F (- (log (exact->inexact last-nchoices))))
         (- R F)]
        [else 0]))

    (define/public (info)
      (define total (+ accepts cond-rejects mh-rejects))
      (cond [(zero? total)
             (printf "No traces taken.\n")]
            [else
             (printf "Accepted traces: ~s, ~a%\n"
                     accepts (* 100.0 (/ accepts total)))
             (printf "Traces rejected by condition: ~s, ~a%\n"
                     cond-rejects (* 100.0 (/ cond-rejects total)))
             (printf "Traces rejected by MH threshold: ~s, ~a%\n"
                     mh-rejects (* 100.0 (/ mh-rejects total)))]))
    ))

;; pick-a-key : DB -> (U Address #f)
;; Returns a key s.t. the value is not pinned.
(define (pick-a-key nchoices db)
  (and (positive? nchoices)
       (db-nth-unpinned db (random nchoices))))
