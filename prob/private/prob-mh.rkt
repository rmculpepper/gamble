;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         racket/math
         racket/vector
         "db.rkt"
         "interfaces.rkt"
         "dist.rkt"
         "../dist/discrete.rkt"
         "hmc/system.rkt"
         "hmc/step.rkt"
         "hmc/acceptance-threshold.rkt"
         "hmc/db-Denergy.rkt")
(provide mh-sampler*
         mh-transition?
         cycle
         sequence
         single-site
         multi-site
         hmc
         slice
         enumerative-gibbs)

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

(define (make-default-proposal scale-factor)
  (lambda (dist value)
    (or (*drift dist value scale-factor)
        #f)))

;; used by slice sampler
(define (real-dist-adjust-value dist value scale-factor)
  (*slice-adjust dist value scale-factor))

(proposal-map
 (extend-proposal-map
  (proposal-map)
  #f (make-default-proposal 1/4)))

;; ============================================================

;; A TransitionResult is one of
;; - Trace  -- run completed, threshold already checked (if applicable)
;; - #f     -- run failed or mh-rejected

;; A Trace is (trace Any DB Nat Real Real)
(struct trace (value db nchoices ll-free ll-obs))
(define init-trace (trace #f '#hash() 0 0 0))

(define (trace-ll t) (+ (trace-ll-free t) (trace-ll-obs t)))

(define mh-transition<%>
  (interface ()
    run  ;; (-> A) Trace -> RunResult
    info ;; Nat -> Void
    ))

(define (iprintf i fmt . args)
  (display (make-string i #\space))
  (apply printf fmt args))

(define (%age nom denom)
  (/ (* 100.0 nom) (exact->inexact denom)))

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

    ;; run : (-> A) Trace -> RunResult
    (define/public (run thunk last-trace)
      (match (run* thunk last-trace)
        [(cons (? real? threshold) trace)
         (when (verbose?)
           (eprintf "# accept threshold = ~s\n" (exp threshold)))
         (define u (log (random)))
         (cond [(< u threshold)
                (when (verbose?)
                  (eprintf "# Accepted MH step with ~s\n" (exp u)))
                (set! accepts (add1 accepts))
                trace]
               [else
                (when (verbose?)
                  (eprintf "# Rejected MH step with ~s\n" (exp u)))
                (set! mh-rejects (add1 mh-rejects))
                #f])]
        [(cons 'fail reason)
         (set! cond-rejects (add1 cond-rejects))
         (when (verbose?)
           (eprintf "# Rejected condition (~s)\n" reason))
         #f]))

    ;; run* : (-> A) Trace -> (U (cons Real Trace) (cons 'fail any))
    (abstract run*)
    ))


(define perturb-mh-transition-base%
  (class mh-transition-base%
    (init-field record-obs?
                [temperature 1])
    (field [proposed 0]
           [resampled 0])
    (super-new)

    (define/override (info i)
      (super info i)
      (iprintf i "Proposal perturbs: ~s\n" proposed)
      (iprintf i "Resample perturbs: ~s\n" resampled))

    ;; run* : (-> A) Trace -> (U (cons Real Trace) (cons 'fail any))
    (define/override (run* thunk last-trace)
      (define last-db (trace-db last-trace))
      (defmatch (cons delta-db R-F) (perturb last-trace))
      (define ctx
        (new db-stochastic-ctx%
             (last-db last-db)
             (delta-db delta-db)
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

    ;; perturb-a-key : Address Dist Value Zones -> (cons Entry Real)
    (define/public (perturb-a-key key dist value zones)
      (or (perturb/proposal key dist value zones)
          (perturb/resample key dist value zones)))

    ;; perturb/resample : Address Dist Any List -> (cons Entry Real)
    (define/public (perturb/resample key dist value zones)
      ;; Fallback: Just resample from same dist.
      ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
      ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
      (define value* (dist-sample dist))
      (define R (dist-pdf dist value #t))
      (define F (dist-pdf dist value* #t))
      (when (verbose?)
        (eprintf "  RESAMPLED from ~e to ~e\n" value value*)
        (eprintf "  R = ~s, F = ~s\n" (exp R) (exp F)))
      (set! resampled (add1 resampled))
      (cons (entry zones dist value* F #f) (- R F)))

    ;; perturb/proposal : Address Dist Any List -> (U (cons Entry Real) #f)
    (define/public (perturb/proposal key dist value zones)
      (match (apply-proposal-map (proposal-map) zones dist value)
        [(cons value* R-F)
         (when (verbose?)
           (eprintf "  PROPOSED from ~e to ~e\n" value value*)
           (eprintf "  R-F = ~s\n" R-F))
         (define ll* (dist-pdf dist value* #t))
         (set! proposed (add1 proposed))
         (cons (entry zones dist value* ll* #f) R-F)]
        [#f #f]))

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    (abstract accept-threshold)
    ))

(define single-site-mh-transition%
  (class perturb-mh-transition-base%
    (inherit perturb-a-key)
    (inherit-field temperature)
    (init-field [zone #f])
    (super-new)

    (define/override (info i)
      (iprintf i "== Transition (single-site #:zone ~e)\n" zone)
      (super info i))

    ;; perturb : Trace -> (cons DB Real)
    (define/override (perturb last-trace)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (define key-to-change (pick-a-key last-nchoices last-db zone))
      (when (verbose?)
        (eprintf "# perturb: changing ~s\n" key-to-change))
      (if key-to-change
          (match (hash-ref last-db key-to-change)
            [(entry zones dist value ll #f)
             (defmatch (cons e R-F)
               (perturb-a-key key-to-change dist value zones))
             (cons (hash key-to-change e) R-F)])
          (cons '#hash() 0)))

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/override (accept-threshold last-trace R-F current-trace ll-diff record-obs?)
      (define ll-diff-obs
        (if record-obs?
            0 ;; already in ll-diff
            (- (trace-ll-obs current-trace) (trace-ll-obs last-trace))))
      (+ R-F (accept-threshold/nchoices last-trace current-trace)
         (/ (+ ll-diff ll-diff-obs) temperature)))

    (define/private (accept-threshold/nchoices last-trace current-trace)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (defmatch (trace _ current-db nchoices _ _) current-trace)
      (cond [(zero? last-nchoices)
             +inf.0]
            [else
             (define nchoices*
               (cond [(eq? zone #f) nchoices]
                     [else (db-count-unpinned current-db #:zone zone)]))
             (define last-nchoices*
               (cond [(eq? zone #f) last-nchoices]
                     [else (db-count-unpinned last-db #:zone zone)]))
             ;; Note: assumes we pick uniformly from all choices.
             ;; R = (log (/ 1 nchoices))        =(- (log nchoices))
             ;; F = (log (/ 1 last-nchoices))   = (- (log last-nchoices))
             ;; convert to inexact so (log 0.0) = -inf.0
             (define R (- (log (exact->inexact nchoices*))))
             (define F (- (log (exact->inexact last-nchoices*))))
             (- R F)]))
    ))

;; pick-a-key : DB -> (U Address #f)
;; Returns a key s.t. the value is not pinned.
(define (pick-a-key nchoices db zone)
  ;; FIXME: what if zone has no choices?
  (define nchoices/zone
    (cond [(eq? zone #f) nchoices]
          [else (db-count-unpinned db #:zone zone)]))
  (and (positive? nchoices/zone)
       (db-nth-unpinned db (random nchoices/zone) #:zone zone)))

(define multi-site-mh-transition%
  (class perturb-mh-transition-base%
    (inherit perturb-a-key)
    (inherit-field temperature)
    (init-field [zone #f])
    (super-new)

    (define/override (info i)
      (iprintf i "== Transition (multi-site #:zone ~e)\n" zone)
      (super info i))

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
               (perturb-a-key key dist value zones))
             (values (hash-set delta-db key e*) (+ R-F R-F*))])))
      (cons delta-db R-F))

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    (define/override (accept-threshold last-trace R-F current-trace ll-diff record-obs?)
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (if (zero? last-nchoices)
          +inf.0
          ;; FIXME: what if nchoices != last-nchoices ???
          (let ([ll-diff-obs
                 (if record-obs?
                     0 ;; already in ll-diff
                     (- (trace-ll-obs current-trace) (trace-ll-obs last-trace)))])
            (+ R-F (/ (+ ll-diff ll-diff-obs) temperature)))))
    ))

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

(define enumerative-gibbs-mh-transition%
  (class* object% (mh-transition<%>)
    (init-field [zone #f]
                [record-obs? #t])
    (field [run-counter 0]
           [eval-counter 0])
    (super-new)

    (define/public (info i)
      (iprintf "== Transition (enumerative-gibbs #:zone ~e)\n" zone)
      (iprintf "Total runs: ~s\n" run-counter)
      (iprintf "Total evals: ~s\n" eval-counter))

    ;; run : (-> A) Trace -> TransitionResult
    (define/public (run thunk last-trace)
      (set! run-counter (add1 run-counter))
      (defmatch (trace _ last-db last-nchoices _ _) last-trace)
      (define key-to-change (pick-a-key last-nchoices last-db zone))
      (when (verbose?)
        (eprintf "# perturb: changing ~s\n" key-to-change))
      (unless key-to-change
        (error 'enumerative-gibbs:run "no key to change"))
      (match (hash-ref last-db key-to-change)
        [(entry zones dist value ll #f)
         (unless (finite-dist? dist)
           (error 'enumerative-gibbs:run
                  "chosen distribution is not finite\n  dist: ~e"
                  dist))
         (perturb/gibbs key-to-change value dist zones thunk last-trace)]))

    (define/private (perturb/gibbs key-to-change value dist zones thunk last-trace)
      (define (make-entry value*)
        (entry zones dist value* (dist-pdf dist value* #t) #f))
      (define enum (dist-enum dist))
      (define conditional-dist
        (make-discrete-dist
         (for*/list ([value* (dist-enum dist)]
                     [ll* (in-value (dist-pdf dist value* #t))]
                     #:when (ll-possible? ll*))
           (cond [(equal? value* value)
                  (when (verbose?)
                    (eprintf "# considering ~e (last value)\n" value*))
                  (cons last-trace (exp (trace-ll last-trace)))]
                 [else
                  (define entry* (make-entry value*))
                  (define delta-db (hash key-to-change entry*))
                  (when (verbose?)
                    (eprintf "# considering ~e\n" value*))
                  (define ctx (new db-stochastic-ctx%
                                   (last-db (trace-db last-trace))
                                   (delta-db delta-db)
                                   (record-obs? record-obs?)))
                  (match (send ctx run thunk)
                    [(cons 'okay sample-value)
                     (define current-db (get-field current-db ctx))
                     (define nchoices (get-field nchoices ctx))
                     (define ll-free (get-field ll-free ctx))
                     (define ll-obs (get-field ll-obs ctx))
                     (define current-trace
                       (trace sample-value current-db nchoices ll-free ll-obs))
                     (define ll (+ ll-free ll-obs))
                     (cons current-trace (exp ll))]
                    [(cons 'fail fail-reason)
                     (cons #f 0)])]))))
      (define t (dist-sample conditional-dist))
      (when (verbose?)
        (eprintf "# chose ~e w/ prob ~s\n"
                 (entry-value (hash-ref (trace-db t) key-to-change))
                 (dist-pdf conditional-dist t)))
      t)
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
  (class mh-transition-base%
    (init-field epsilon 
                L
                [zone #f])
    (field [gradients #f])
    (super-new)

    (define/override (info i)
      (iprintf i "== Transition (hmc ~e ~e #:zone ~e)\n" epsilon L zone)
      (super info i))

    (define/override (run* thunk last-trace)
      (define last-trace*
        (cond [(and gradients (not (equal? last-trace init-trace)))
               last-trace]
              [else
               (run/collect-gradients thunk last-trace)]))
      (when #f
        (eprintf "collected gradients ~e\n" gradients))
      (run/hmc thunk last-trace*))

    (define/private (run/collect-gradients thunk last-trace)
      (define delta-db '#hash())
      (define last-db (trace-db last-trace))
      (define ctx 
        (new db-stochastic-derivative-ctx%
             (last-db last-db)
             (delta-db delta-db)))
      (define result (send ctx run thunk))
      (define ans-db (get-field current-db ctx))
      (define grads (get-field derivatives ctx))
      (when (verbose?)
        (eprintf " (recorded derivatives are) ~e\n" grads)
        (eprintf " (relevant labels were) ~e \n" (get-field relevant-labels ctx)))
      (match result
        [(cons 'okay ans)
         (set! gradients grads)
         (trace ans ans-db
                (get-field nchoices ctx)
                (get-field ll-free ctx)
                (get-field ll-obs ctx))]
        [(cons 'fail fail-reason)
         (run/collect-gradients thunk last-trace)]))

    (define/private (run/hmc thunk last-trace)
      (define last-accepted-sys
        (hmc-system (trace-db last-trace) (make-hash)))
      (define step-result
        (hmc-step last-accepted-sys epsilon L (db-Denergy gradients zone)
                  thunk zone))
      (match step-result
        [(list 'fail reason)
         step-result]
        [(list 'okay initial-sys val proposal-sys)
         (let-values ([(proposal-energy alpha)
                       (hmc-acceptance-threshold initial-sys proposal-sys)])
           (when (verbose?)
             (eprintf "# Previous system energy: ~e (K = ~e + U = ~e)\n"
                      (hmc-system-energy initial-sys)
                      (db-kinetic-energy (hmc-system-P initial-sys))
                      (db-potential-energy (hmc-system-X initial-sys)))
             (eprintf "# Proposal energy: ~e (K = ~e + U = ~e)\n"
                      proposal-energy
                      (db-kinetic-energy (hmc-system-P proposal-sys))
                      (db-potential-energy (hmc-system-X proposal-sys)) ))
           (cons alpha (hmc-system->trace last-trace val proposal-sys)))]))

    (define/private (hmc-system->trace last-trace sample-value proposal-sys)
      (defmatch (trace _ _ last-nchoices _ _) last-trace)
      (define proposal-db (hmc-system-X proposal-sys))
      ; XXX - is this right?
      (define ll-free 0.0)
      (define ll-obs 0.0)
      (trace sample-value proposal-db last-nchoices ll-free ll-obs))
    ))

(define cycle-mh-transition%
  (class* object% (mh-transition<%>)
    (init-field transitions)
    (super-new)

    (define ctransitions
      (let ([p (make-placeholder (void))])
        (placeholder-set! p (append transitions p))
        (make-reader-graph p)))

    (define/public (info i)
      (iprintf i "== Transition (cycle ...)\n")
      (for ([tx (in-list transitions)])
        (send tx info (+ i 2))))

    (define/public (run thunk last-trace)
      (define r (send (car ctransitions) run thunk last-trace))
      (when r  ;; only advance on success
        (set! ctransitions (cdr ctransitions)))
      r)
    ))

(define sequence-mh-transition%
  (class* object% (mh-transition<%>)
    (init-field transitions)
    (super-new)

    (define ctransitions transitions)

    (define/public (info i)
      (iprintf i "== Transition (sequence ...)\n")
      (for ([tx (in-list transitions)])
        (send tx info (+ i 2))))

    (define/public (run thunk last-trace)
      (define r (send (car ctransitions) run thunk last-trace))
      (when (and r (pair? (cdr ctransitions)))
        ;; only advance on success; stop on last tx
        (set! ctransitions (cdr ctransitions)))
      r)
    ))

(define rerun-mh-transition%
  (class perturb-mh-transition-base%
    (super-new [record-obs? #f])
    (define/override (info i)
      (iprintf "== Transition (rerun)\n")
      (super info i))
    (define/override (perturb last-trace)
      (cons '#hash() +inf.0))
    (define/override (accept-threshold . _args)
      +inf.0)))
(define the-rerun-mh-transition (new rerun-mh-transition%))

(define (mh-transition? x) (is-a? x mh-transition<%>))

(define (sequence . txs)
  (new sequence-mh-transition% (transitions txs)))
(define (cycle . txs)
  (new cycle-mh-transition% (transitions txs)))
(define (single-site #:zone [zone #f] #:record-obs? [record-obs? #t])
  (new single-site-mh-transition% [zone zone] [record-obs? record-obs?]))
(define (multi-site #:zone [zone #f] #:record-obs? [record-obs? #t])
  (new multi-site-mh-transition% [zone zone] [record-obs? record-obs?]))
(define (hmc [epsilon 0.01] [L 10] #:zone [zone #f])
  (new hmc-transition% [epsilon epsilon] [L L] [zone zone]))
(define (slice #:scale [scale-factor 1] #:zone [zone #f])
  (new single-site-slice-mh-transition% (scale-factor scale-factor) (zone zone)))
(define (enumerative-gibbs #:zone [zone #f] #:record-obs? [record-obs? #t])
  (new enumerative-gibbs-mh-transition% (zone zone) (record-obs? record-obs?)))
(define (rerun)
  the-rerun-mh-transition)

;; ============================================================

(define (mh-sampler* thunk [transition (single-site)])
  (new mh-sampler% (thunk thunk) (transition transition)))

(define mh-sampler%
  (class sampler-base%
    (init-field thunk
                transition)
    (field [last-trace init-trace]
           [accepts 0]
           [cond-rejects 0]
           [mh-rejects 0]
           [transition-stack null])
    (super-new)

    (define/public (set-transition tx)
      (set! transition tx))
    (define/public (push-transition tx)
      (set! transition-stack (cons transition transition-stack))
      (set! transition tx))
    (define/public (pop-transition)
      (cond [(pair? transition-stack)
             (define tx transition)
             (set! transition (car transition-stack))
             (set! transition-stack (cdr transition-stack))
             tx]
            [else #f]))

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
      (sample/transition transition))

    (define/public (rerun)
      (define lt last-trace)
      (sample/transition the-rerun-mh-transition)
      (not (eq? lt last-trace)))

    (define/public (sample/transition tx)
      (match (send tx run thunk last-trace)
        [(? trace? t)
         (set! last-trace t)
         (trace-value t)]
        [#f
         (if (eq? last-trace init-trace)
             (sample)
             (trace-value last-trace))]))

    (define/public (info)
      (send transition info 0)
      (when (pair? transition-stack)
        (printf "\n== Pushed transitions\n")
        (for ([tx (in-list transition-stack)])
          (send tx info 2))))
    ))
