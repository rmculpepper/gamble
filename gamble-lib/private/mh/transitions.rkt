;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "base.rkt"
         "../dist.rkt"
         "../util/real.rkt"
         "../util/density.rkt")
(provide (all-defined-out))

;; ============================================================

(define mh-transition-base%
  (class* object% (mcmc-transition<%>)
    (super-new)

    ;; run : (-> A) Trace -> (values Trace/#f TxInfo)
    (define/public (run thunk prev-trace)
      (log-mh-info "Starting transition (~s)" (object-name this%))
      (define-values (laccept new-trace new-txinfo)
        (run* thunk prev-trace))
      (define u (log (random)))
      (cond [(< u laccept)
             (log-mh-info "Accepted MH step with threshold ~s" (exp laccept))
             (values new-trace new-txinfo)]
            [else
             (log-mh-info "Rejected MH step with threshold ~s" (exp laccept))
             (cons #f new-txinfo)]))

    ;; run* : (-> A) Trace -> (values Real Trace/#f TxInfo)
    (abstract run*)
    ))


;; ============================================================

(define perturb-mh-transition-base%
  (class mh-transition-base%
    (init-field [temperature 1])
    (super-new)

    ;; run* : (-> A) Trace -> (values Real Trace/#f TxInfo)
    (define/override (run* thunk prev-trace)
      (define prev-db (trace-db prev-trace))
      (define-values (delta-db delta-ll-R/F) (perturb prev-trace))
      (define ctx
        (new tracing-stochastic-ctx%
             (prev-db prev-db)
             (delta-db delta-db)
             (ll-R/F delta-ll-R/F)))
      (match (send ctx run thunk)
        [(list new-value)
         (define new-trace (send ctx make-trace new-value))
         (define ll-diff (send ctx get-ll-diff))
         (define threshold
           (accept-threshold prev-trace delta-ll-R/F new-trace ll-diff))
         (values threshold new-trace (vector 'delta delta-db))]
        [#f
         (values -inf.0 #f (vector 'delta delta-db))]))

    ;; perturb : Trace -> (values DB Real)
    (abstract perturb)

    ;; accept-threshold : Trace Real Trace Real -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/public (accept-threshold prev-trace ll-R/F new-trace ll-diff)
      (define other-factor (accept-threshold* prev-trace new-trace))
      (cond [(or (= other-factor -inf.0) (= other-factor +inf.0))
             other-factor]
            [else
             (define ll-diff-obs (traces-obs-diff new-trace prev-trace))
             (+ ll-R/F (/ (+ ll-diff ll-diff-obs) temperature) other-factor)]))

    ;; accept-threshold* : Trace Trace -> Real
    ;; Computes (log) of additional factors of accept threshold.
    ;; If +/-inf.0, then that is taken as accept factor (to avoid
    ;; possible NaN from arithmetic).
    (define/public (accept-threshold* prev-trace current-trace)
      0.0)
    ))

;; ============================================================

(define single-site-mh-transition%
  (class perturb-mh-transition-base%
    (init-field ok-addr?      ;; (Addr -> Boolean) or #f
                proposal)     ;; Proposal
    (super-new)

    ;; perturb : Trace -> (values DB Real)
    (define/override (perturb prev-trace)
      (define prev-db (trace-db prev-trace))
      (define addr (hash-random-key (trace-db prev-trace) ok-addr?))
      (cond [addr
             (log-mh-info "Addr to change = ~s\n" addr)
             (match (hash-ref prev-db addr)
               [(entry prev-dist prev-value prev-ll)
                (define-values (new-e ll-R/F)
                  (perturb-addr addr prev-dist prev-value))
                (values (hash addr new-e) ll-R/F)])]
            [else
             (log-mh-info "No suitable addr to change")
             (values (hash) -inf.0)]))

    ;; perturb-addr : Address Dist Value -> (values Entry Real)
    (define/public (perturb-addr addr dist prev-value)
      (define-values (new-value ll-R/F)
        (send proposal propose1 addr dist prev-value))
      (log-mh-info "PROPOSED ~s: ~e, ~e => ~e; R/F=~s" addr dist
                   prev-value new-value (exp ll-R/F))
      (define dn (dist-density dist new-value #t))
      (when (density-zero? dn)
        (log-mh-info "proposed impossible value: ~e, ~e" dist new-value))
      (cons (entry dist new-value dn) ll-R/F))

    (define/override (accept-threshold* prev-trace new-trace)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      (define new-nchoices (hash-count* (trace-db new-trace) ok-addr?))
      (define prev-nchoices (hash-count* (trace-db prev-trace) ok-addr?))
      (cond [(zero? prev-nchoices)
             +inf.0]
            [else
             ;; Note: assumes we pick uniformly from all choices.
             ;; R = (log (/ 1 new-nchoices))    = (- (log new-nchoices))
             ;; F = (log (/ 1 prev-nchoices))   = (- (log prev-nchoices))
             ;; convert to inexact so (log 0.0) = -inf.0
             (define lR (- (log (fl new-nchoices))))
             (define lF (- (log (fl prev-nchoices))))
             (- lR lF)]))
    ))

;; ============================================================

(define enumerative-gibbs-mh-transition%
  (class* object% (mcmc-transition<%>)
    (init-field ok-addr?)     ;; (Addr -> Boolean) or #f
    (super-new)

    ;; run : (-> A) Trace -> (values (U Trace #f) TxInfo)
    (define/public (run thunk prev-trace)
      (log-mh-info "Starting transition (~s)" (object-name this%))
      (define prev-db (trace-db prev-trace))
      (define addr (hash-random-key prev-db ok-addr?))
      (unless addr (error 'enumerative-gibbs "no suitable addr to change"))
      (log-mh-info "Addr to change = ~s" addr)
      (match-define (entry dist prev-value prev-dn) (hash-ref prev-db addr))
      (unless (finite-dist? dist)
        (error 'enumerative-gibbs
               "distribution is not finite\n  addr: ~e\n  dist: ~e" addr dist))
      (define (make-entry new-value)
        (entry dist new-value (dist-density dist new-value #t)))
      (define conditional-dist
        (log-hash->normalized-discrete-dist
         (for/fold ([lh (hash)]) ([new-value (in-dist-values dist)])
           (cond [(equal? new-value prev-value)
                  (hash-set lh prev-trace (trace-ll prev-trace))]
                 [else
                  (define new-entry (make-entry new-value))
                  (define delta-db (hash addr new-entry))
                  (define ctx (new tracing-stochastic-ctx%
                                   (prev-db prev-db)
                                   (delta-db delta-db)
                                   (disallow-new/who 'enumerative-gibbs)))
                  (match (send ctx run thunk)
                    [(list new-result)
                     (define new-trace (send ctx make-trace new-result))
                     (unless (traces-same-structure? prev-trace new-trace #t)
                       (error 'enumerative-gibbs "structural change not allowed"))
                     (hash-set lh new-trace (trace-ll new-trace))]
                    [#f lh])]))))
      (define new-trace (dist-sample conditional-dist))
      (values new-trace 'enumerative-gibbs))
    ))
