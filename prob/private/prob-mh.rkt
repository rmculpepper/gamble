;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/list
         racket/class
         racket/match
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
      ;; FIXME: have perturb create delta-db (immutable?)
      (define delta-db (make-hash))
      (define R-F (if key-to-change (perturb! last-db delta-db key-to-change) 0))
      (define current-db (make-hash))
      (define nchoices #f) ;; mutated below
      (define ll-diff #f) ;; mutated below
      ;; Run program
      (define result
        (let/ec escape
          (define ctx
            (new db-stochastic-ctx%
                 (current-db current-db)
                 (last-db last-db)
                 (delta-db delta-db)
                 (spconds spconds)
                 (escape escape)))
          (parameterize ((current-stochastic-ctx ctx))
            (define v (apply/delimit thunk))
            (set! nchoices (get-field nchoices ctx))
            (set! ll-diff (get-field ll-diff ctx))
            (cons 'okay v))))
      ;; Accept/reject
      (match result
        [(cons 'okay sample-value)
         (define threshold (accept-threshold R-F current-db last-db delta-db))
         (when (verbose?)
           (eprintf "# ll-diff = ~s\n" ll-diff)
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

    ;; perturb! : DB DB Address -> Real
    ;; Updates db and returns (log) R-F.
    (define/private (perturb! last-db delta-db key-to-change)
      (match (hash-ref last-db key-to-change)
        [(entry dist value ll #f)
         (or (perturb!/proposal delta-db key-to-change dist value)
             (perturb!/resample delta-db key-to-change dist value))]))

    ;; perturb!/resample : DB Address Dist Any -> Real
    (define/private (perturb!/resample delta-db key-to-change dist value)
      ;; Fallback: Just resample from same dist.
      ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
      ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
      (define value* (dist-sample dist))
      (define R (dist-pdf dist value #t))
      (define F (dist-pdf dist value* #t))
      (when (verbose?)
        (eprintf "  RESAMPLED from ~e to ~e\n" value value*)
        (eprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
      (hash-set! delta-db key-to-change (entry dist value* F #f))
      (- R F))

    ;; perturb!/proposal : DB Address Dist Any -> (U Real #f)
    ;; If applicable proposal dist avail, updates db and returns (log) R-F,
    ;; otherwise returns #f and perturb! should just resample.
    (define/private (perturb!/proposal delta-db key-to-change dist value)
      ;; update! : ... -> Real
      (define (update! R F value*)
        (when (verbose?)
          (eprintf "  PROPOSED from ~e to ~e\n" value value*)
          (eprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
        (define ll* (dist-pdf dist value* #t))
        (hash-set! delta-db key-to-change (entry dist value* ll* #f))
        (- R F))
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
                (update! R F value*)))]
        [_ #f]))

    ;; accept-threshold : ... -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/private (accept-threshold R-F current-db last-db delta-db)
      (if (zero? (hash-count last-db))
          +inf.0
          (accept-threshold* R-F current-db last-db delta-db)))

    (define/private (accept-threshold* R-F current-db last-db delta-db)
      (define last-ll (db-ll last-db))
      (define current-ll (db-ll current-db))
      (define stale-ll (db-ll/fresh last-db current-db delta-db))
      (define fresh-ll (db-ll/fresh current-db last-db delta-db))
      (+ R-F (accept-threshold/nchoices current-db last-db)
         (- current-ll last-ll) (- stale-ll fresh-ll)))

    (define/private (accept-threshold/nchoices current-db last-db)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      ;; Note: assumes we pick uniformly from all choices.
      (case threshold-mode
        [(purge)
         ;; R = (log (/ 1 (hash-count current-db))) = (- (log ....))
         ;; F = (log (/ 1 (hash-count last-db)))    = (- (log ....))
         ;; convert to inexact so (log 0.0) = -inf.0
         (define R (- (log (exact->inexact (hash-count current-db)))))
         (define F (- (log (exact->inexact (hash-count last-db)))))
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
