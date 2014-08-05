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

;; ProposalMap = hash[ Zone => (listof ProposalFun) ])
;; where ProposalFun = (Dist Value -> (U (cons Value Real) #f))
;; The function returns a new value and the proposal's R-F.

;; extend-proposal-map : ProposalMap {Zone ProposalFun}* -> ProposalMap
(define (extend-proposal-map pm . args)
  (unless (even? (length args))
    (error 'extend-proposal-map "expected an even number of {zone,function} arguments"))
  (let loop ([args args])
    (cond [(and (pair? args) (pair? (cdr args)))
           (extend-proposal-map1 (loop (cddr args)) (car args) (cadr args))]
          [else pm])))

;; extend-proposal-map1 : ProposalMap Zone ProposalFun -> ProposalMap
(define (extend-proposal-map1 pm zone fun)
  (hash-set pm zone (cons fun (hash-ref pm zone null))))

;; apply-proposal-map : ProposalMap (Listof Zone) Dist Value -> (U (cons Value Real) #f)
;; Try all functions for primary (closest enclosing) zone, then for second zone, etc.
(define (apply-proposal-map pm zones dist val)
  (or (apply-proposal-map* pm zones dist val)
      (apply-proposal-map* pm '(#f) dist val)))

(define (apply-proposal-map* pm zonelist dist val)
  (for/or ([z (in-list zonelist)])
    (for/or ([fun (in-list (hash-ref pm z null))])
      (fun dist val))))

;; proposal-map : (parameterof ProposalMap)
(define proposal-map (make-parameter '#hash()))

(define (make-normal-proposal stddev-factor)
  ;; Proposal function for Normal dist
  (lambda (dist value)
    (match dist
      [(normal-dist mean stddev)
       (define forward-dist
         (normal-dist value (* stddev stddev-factor)))
       (define value* (dist-sample forward-dist))
       (define backward-dist
         (normal-dist value* (* stddev stddev-factor)))
       (define R (dist-pdf backward-dist value #t))
       (define F (dist-pdf forward-dist value* #t))
       (cons value* (- R F))]
      [_ #f])))

(proposal-map
 (extend-proposal-map
  (proposal-map)
  #f (make-normal-proposal 1/4)))

;; ============================================================

;; A RunResult is one of
;; - Trace              -- run completed, includes threshold (not yet checked)
;; - (cons 'failed any) -- run failed

;; A Trace is (trace Any DB Nat Real Real)
(struct trace (value db nchoices ll-total threshold))
(define init-trace (trace #f '#hash() 0 0 0))

(define mh-transition<%>
  (interface ()
    run ;; (-> A) SPConds Trace -> RunResult
    ))

(define mh-transition-base%
  (class* object% (mh-transition<%>)
    (super-new)

    ;; run : (-> A) SPConds Trace -> TransitionResult
    (define/public (run thunk spconds last-trace)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (defmatch (cons delta-db R-F) (perturb last-db last-nchoices))
      (define ctx
        (new db-stochastic-ctx%
             (last-db last-db)
             (delta-db delta-db)
             (spconds spconds)))
      ;; Run program
      (define result (send ctx run thunk))
      (define current-db (get-field current-db ctx))
      (define nchoices (get-field nchoices ctx))
      (define ll-total (get-field ll-total ctx))
      (define ll-diff (get-field ll-diff ctx))
      (match result
        [(cons 'okay sample-value)
         (define threshold
           (accept-threshold R-F current-db nchoices last-db last-nchoices ll-diff))
         (trace sample-value current-db nchoices ll-total threshold)]
        [(cons 'fail fail-reason)
         result]))

    ;; perturb : DB Nat -> (cons DB Real)
    (abstract perturb)

    ;; accept-threshold : Real DB Nat DB Nat Real -> Real
    (abstract accept-threshold)
    ))

(define single-site-mh-transition%
  (class mh-transition-base%
    (init-field [zone #f])
    (super-new)

    ;; perturb : DB Nat -> (cons DB Real)
    (define/override (perturb last-db last-nchoices)
      (define key-to-change (pick-a-key last-nchoices last-db))
      (when (verbose?)
        (eprintf "# perturb: changing ~s\n" key-to-change))
      (if key-to-change
          (match (hash-ref last-db key-to-change)
            [(entry zones dist value ll #f)
             (or (perturb/proposal key-to-change dist value zones)
                 (perturb/resample key-to-change dist value zones))])
          (cons '#hash() 0)))

    ;; perturb/resample : Address Dist Any List -> (cons DB Real)
    (define/private (perturb/resample key-to-change dist value zones)
      ;; Fallback: Just resample from same dist.
      ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
      ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
      (define value* (dist-sample dist))
      (define R (dist-pdf dist value #t))
      (define F (dist-pdf dist value* #t))
      (when (verbose?)
        (eprintf "  RESAMPLED from ~e to ~e\n" value value*)
        (eprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
      (cons (hash key-to-change (entry zones dist value* F #f)) (- R F)))

    ;; perturb/proposal : Address Dist Any List -> (U (cons DB Real) #f)
    (define/private (perturb/proposal key-to-change dist value zones)
      (match (apply-proposal-map (proposal-map) zones dist value)
        [(cons value* R-F)
         (when (verbose?)
           (eprintf "  PROPOSED from ~e to ~e\n" value value*)
           (eprintf "  R-F = ~s\n" R-F))
         (define ll* (dist-pdf dist value* #t))
         (cons (hash key-to-change (entry zones dist value* ll* #f)) R-F)]
        [#f #f]))

    ;; accept-threshold : Real DB Nat DB Nat Real -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/override (accept-threshold R-F current-db nchoices last-db last-nchoices ll-diff)
      (let ([nchoices
             (cond [(eq? zone #f) nchoices]
                   [else (db-count-unpinned current-db #:zone zone)])]
            [last-nchoices
             (cond [(eq? zone #f) last-nchoices]
                   [else (db-count-unpinned last-db #:zone zone)])])
        (if (zero? last-nchoices)
            +inf.0
            (+ R-F (accept-threshold/nchoices nchoices last-nchoices) ll-diff))))

    (define/private (accept-threshold/nchoices nchoices last-nchoices)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      ;; Note: assumes we pick uniformly from all choices.
      ;; R = (log (/ 1 (hash-count current-db))) = (- (log ....))
      ;; F = (log (/ 1 (hash-count last-db)))    = (- (log ....))
      ;; convert to inexact so (log 0.0) = -inf.0
      (define R (- (log (exact->inexact nchoices))))
      (define F (- (log (exact->inexact last-nchoices))))
      (- R F))

    ;; pick-a-key : DB -> (U Address #f)
    ;; Returns a key s.t. the value is not pinned.
    (define/private (pick-a-key nchoices db)
      ;; FIXME: what if zone has no choices?
      (define nchoices/zone
        (cond [(eq? zone #f) nchoices]
              [else (db-count-unpinned db #:zone zone)]))
      (and (positive? nchoices/zone)
           (db-nth-unpinned db (random nchoices/zone) #:zone zone)))
    ))

(define multi-site-mh-transition%
  (class mh-transition-base%
    (init-field [zone #f])
    (super-new)

    ;; perturb : DB Nat -> (cons DB Real)
    (define/override (perturb last-db last-nchoices)
      (when (verbose?)
        (eprintf "# perturb: changing all sites\n"))
      (define-values (delta-db R-F)
        (for/fold ([delta-db '#hash()] [R-F 0])
            ([(key e) (in-hash last-db)]
             #:when (not (entry-pinned? e))
             #:when (entry-in-zone? e zone))
          (match e
            [(entry zones dist value ll #f)
             (defmatch (cons e* R-F*)
               (or (perturb/proposal key dist value zones)
                   (perturb/resample key dist value zones)))
             (values (hash-set delta-db key e*) (+ R-F R-F*))])))
      (cons delta-db R-F))

    ;; perturb/resample : Address Dist Any List -> (cons Entry Real)
    (define/private (perturb/resample key-to-change dist value zones)
      ;; Fallback: Just resample from same dist.
      ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
      ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
      (define value* (dist-sample dist))
      (define R (dist-pdf dist value #t))
      (define F (dist-pdf dist value* #t))
      (when (verbose?)
        (eprintf "  RESAMPLED from ~e to ~e\n" value value*)
        (eprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
      (cons (entry zones dist value* F #f) (- R F)))

    ;; perturb/proposal : Address Dist Any List -> (U (cons Entry Real) #f)
    (define/private (perturb/proposal key-to-change dist value zones)
      (match (apply-proposal-map (proposal-map) zones dist value)
        [(cons value* R-F)
         (when (verbose?)
           (eprintf "  PROPOSED from ~e to ~e\n" value value*)
           (eprintf "  R-F = ~s\n" R-F))
         (define ll* (dist-pdf dist value* #t))
         (cons (entry zones dist value* ll* #f) R-F)]
        [#f #f]))

    ;; accept-threshold : Real Nat Nat Real -> Real
    (define/override (accept-threshold R-F nchoices last-nchoices ll-diff)
      (if (zero? last-nchoices)
          +inf.0
          ;; FIXME: what if nchoices != last-nchoices ???
          ;; FIXME: nchoices, last-nchoices are not zone-specific
          (+ R-F ll-diff)))
    ))

;; FIXME: don't rotate if transition rejected; need communication w/ sampler
(define cycle-mh-transition%
  (class* object% (mh-transition<%>)
    (init-field transitions)
    (super-new)

    (set! transitions
          (let ([p (make-placeholder)])
            (placeholder-set! p (append transitions p))
            (make-reader-graph p)))

    (define/public (run thunk spconds last-trace)
      (begin0 (send (car transitions) run thunk spconds last-trace)
        (set! transitions (cdr transitions))))
    ))

(define (cycle . txs)
  (new cycle-mh-transition% (transitions txs)))
(define single-site (new single-site-mh-transition%))
(define multi-site (new multi-site-mh-transition%))

;; ============================================================

(define (mh-sampler* thunk spconds)
  (new mh-sampler% (thunk thunk) (spconds spconds)))

(define mh-sampler%
  (class sampler-base%
    (init-field thunk
                spconds)
    (field [last-trace init-trace]
           [accepts 0]
           [cond-rejects 0]
           [mh-rejects 0]
           [transition single-site])
    (super-new)

    (define/public (MAP-estimate iters)
      (*estimate iters trace-ll-total))
    (define/public (MLE-estimate iters)
      (define (trace-ll-obs t)
        (defmatch (trace _ db _ _ _) t)
        (db-ll-pinned db))
      (*estimate iters trace-ll-obs))

    (define/private (*estimate iters get-trace-ll)
      (void (sample))
      (define best-trace
        (for/fold ([best-trace last-trace]) ([n (in-range iters)])
          (void (sample))
          (if (> (get-trace-ll last-trace) (get-trace-ll best-trace))
              last-trace
              best-trace)))
      (trace-value best-trace))

    (define/override (sample)
      (define tr (send transition run thunk spconds last-trace))
      (match tr
        [(trace sample-value current-db nchoices _ threshold)
         (when (verbose?)
           (eprintf "# accept threshold = ~s\n" (exp threshold)))
         (define u (log (random)))
         (cond [(< u threshold)
                (when (verbose?)
                  (eprintf "# Accepted MH step with ~s\n" (exp u)))
                (set! accepts (add1 accepts))
                (set! last-trace tr)
                sample-value]
               [else
                (when (verbose?)
                  (eprintf "# Rejected MH step with ~s\n" (exp u)))
                (set! mh-rejects (add1 mh-rejects))
                (trace-value last-trace)])]
        [(cons 'fail reason)
         (when (verbose?)
           (eprintf "# Rejected condition (~s)" reason))
         (set! cond-rejects (add1 cond-rejects))
         (trace-value last-trace)]))

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
