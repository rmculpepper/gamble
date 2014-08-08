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
         "hmc/system.rkt"
         "hmc/step.rkt"
         "hmc/acceptance-threshold.rkt"
         "hmc/db-Denergy.rkt"
         (only-in "dist.rkt" dists-same-type?)
         "prob-util.rkt")
(provide mh-sampler*
         cycle
         single-site
         multi-site
         hamiltonian-mc)

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
       #|
       (define backward-dist
         (normal-dist value* (* stddev stddev-factor)))
       (define R (dist-pdf backward-dist value #t))
       (define F (dist-pdf forward-dist value* #t))
       |#
       (cons value* #|(- R F)|# 0)]
      [_ #f])))

(proposal-map
 (extend-proposal-map
  (proposal-map)
  #f (make-normal-proposal 1/4)))

;; ============================================================

;; A RunResult is one of
;; - (cons Real Trace)  -- run completed, includes threshold (not yet checked)
;; - (cons 'fail any)   -- run failed

;; A Trace is (trace Any DB Nat Real Real)
(struct trace (value db nchoices ll-free ll-obs))
(define init-trace (trace #f '#hash() 0 0 0))

(define mh-transition<%>
  (interface ()
    run ;; (-> A) SPConds Trace -> RunResult
    ))

(define mh-transition-base%
  (class* object% (mh-transition<%>)
    (init-field [record-obs? #f])  ;; FIXME: default #t ??
    (super-new)

    ;; run : (-> A) SPConds Trace -> TransitionResult
    (define/public (run thunk spconds last-trace)
      (define last-db (trace-db last-trace))
      (defmatch (cons delta-db R-F) (perturb last-trace))
      (define ctx
        (new db-stochastic-ctx%
             (last-db last-db)
             (delta-db delta-db)
             (spconds spconds)
             (record-obs? record-obs?)))
      ;; Run program
      (define result (send ctx run thunk))
      (match result
        [(cons 'okay sample-value)
         (define current-db (get-field current-db ctx))
         (define nchoices (get-field nchoices ctx))
         (define ll-free (get-field ll-free ctx))
         (define ll-obs (get-field ll-obs ctx))
         (define ll-diff (get-field ll-diff ctx))
         (define current-trace
           (trace sample-value current-db nchoices ll-free ll-obs))
         (define threshold
           (accept-threshold last-trace R-F current-trace ll-diff record-obs?))
         (cons threshold current-trace)]
        [(cons 'fail fail-reason)
         result]))

    ;; perturb : Trace -> (cons DB Real)
    (abstract perturb)

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    (abstract accept-threshold)
    ))

(define single-site-mh-transition%
  (class mh-transition-base%
    (init-field [zone #f])
    (super-new)

    ;; perturb : Trace -> (cons DB Real)
    (define/override (perturb last-trace)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
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

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/override (accept-threshold last-trace R-F current-trace ll-diff record-obs?)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (defmatch (trace _ current-db nchoices _ _) current-trace)
      (let ([nchoices
             (cond [(eq? zone #f) nchoices]
                   [else (db-count-unpinned current-db #:zone zone)])]
            [last-nchoices
             (cond [(eq? zone #f) last-nchoices]
                   [else (db-count-unpinned last-db #:zone zone)])])
        (if (zero? last-nchoices)
            +inf.0
            (let ([ll-diff-obs
                   (if record-obs?
                       0 ;; already in ll-diff
                       (- (trace-ll-obs current-trace) (trace-ll-obs last-trace)))])
              (+ R-F (accept-threshold/nchoices nchoices last-nchoices)
                 ll-diff ll-diff-obs)))))

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

    ;; perturb : Trace -> (cons DB Real)
    (define/override (perturb last-trace)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
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

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    (define/override (accept-threshold last-trace R-F current-trace ll-diff record-obs?)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (if (zero? last-nchoices)
          +inf.0
          ;; FIXME: what if nchoices != last-nchoices ???
          ;; FIXME: nchoices, last-nchoices are not zone-specific
          (let ([ll-diff-obs
                 (if record-obs?
                     0 ;; already in ll-diff
                     (- (trace-ll-obs current-trace) (trace-ll-obs last-trace)))])
            (+ R-F ll-diff ll-diff-obs))))
    ))


#|
HMC - Hamiltonian Monte Carlo

The usual database of random choices is now thought of as
the "position" of a particle and an auxiliary normally
distributed "momentum" random variable is added such that the
quantity:
  H(x,p) = U(x) + K(p)
is conserved.  Where U(x) is the "potential energy" of the position
and K(p) is the "kinetic energy" of the momentum.

Limitations:
1. All distributions in the database should be continous.

2. There should be no "structural" choices.  (ie, the values of random
choices do not affect control flow through the probabilistic program).
|#
(define hmc-transition%
  (class* object% (mh-transition<%>)
    (init-field epsilon 
                L)
    (field [gradients '#hash()])
    (super-new)

    (define/public (run thunk spconds last-trace)
      (if (equal? last-trace init-trace)
          (run/initial+gradients thunk spconds last-trace)
          (run/hmc thunk spconds last-trace)))
    
    (define/private (run/initial+gradients thunk spconds last-trace)
      (define delta-db (make-hash))
      (define last-db (make-hash))
      (define ctx 
        (new db-stochastic-derivative-ctx%
             (last-db last-db)
             (delta-db delta-db)
             (spconds spconds)))
      (define result (send ctx run thunk))
      (define ans-db (get-field current-db ctx))
      (define grads (get-field derivatives ctx))
      (when (verbose?)
        (eprintf " (recorded derivatives are) ~e\n" grads)
        (eprintf " (relevant labels were) ~e \n" (get-field relevant-labels ctx)))
      (match result
        [(cons 'okay ans)
         (set! gradients grads)
         (cons 1.0 ; always accept the first sample
               (trace ans ans-db
                      (get-field nchoices ctx)
                      (get-field ll-free ctx)
                      (get-field ll-obs ctx)))]
        [(cons 'fail fail-reason)
         result]))
    
    (define/private (run/hmc thunk spconds last-trace)
      (define last-accepted-sys
        (hmc-system (trace-db last-trace) (make-hash)))
      (define step-result
        (hmc-step last-accepted-sys epsilon L (db-Denergy gradients)
                  thunk spconds))
      (match step-result
        [(list 'fail reason)
         step-result]
        [(list 'okay initial-sys val proposal-sys)
         (let-values ([(proposal-energy alpha)
                       (hmc-acceptance-threshold initial-sys proposal-sys)])
           (cons alpha (hmc-system->trace last-trace val proposal-sys)))]))
    
    (define/private (hmc-system->trace last-trace sample-value proposal-sys)
      (defmatch (trace _ _ last-nchoices _ _) last-trace)
      (define proposal-db (hmc-system-X proposal-sys))
      ; XXX - is this right?
      (define ll-free 0.0)
      (define ll-obs 0.0)
      (trace sample-value proposal-db last-nchoices ll-free ll-obs))
    
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
(define (single-site [zone #f])
  (new single-site-mh-transition% [zone zone]))
(define (multi-site [zone #f])
  (new multi-site-mh-transition% [zone zone]))
(define (hamiltonian-mc epsilon L)
  (new hmc-transition% [epsilon epsilon] [L L]))

;; ============================================================

(define (mh-sampler* thunk spconds [transition (single-site)])
  (new mh-sampler% (thunk thunk) (spconds spconds) (transition transition)))

(define mh-sampler%
  (class sampler-base%
    (init-field thunk
                spconds
                transition)
    (field [last-trace init-trace]
           [accepts 0]
           [cond-rejects 0]
           [mh-rejects 0])
    (super-new)

    ;; Note: {MAP,MLE}-estimate is argmax over *all* unconditioned variables.
    ;; FIXME: figure out how to do subsets.
    (define/public (MAP-estimate iters)
      (define (trace-ll-total t)
        (+ (trace-ll-free t) (trace-ll-obs t)))
      (*estimate iters trace-ll-total))
    (define/public (MLE-estimate iters)
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
        [(cons (? real? threshold) trace)
         (when (verbose?)
           (eprintf "# accept threshold = ~s\n" (exp threshold)))
         (define u (log (random)))
         (cond [(< u threshold)
                (when (verbose?)
                  (eprintf "# Accepted MH step with ~s\n" (exp u)))
                (set! accepts (add1 accepts))
                (set! last-trace trace)
                (trace-value trace)]
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
