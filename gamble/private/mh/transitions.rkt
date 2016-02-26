;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         "db.rkt"
         "../interfaces.rkt"
         "../dist.rkt"
         "../../dist/discrete.rkt"
         "base.rkt"
         "proposal.rkt")
(provide (all-defined-out))

;; ============================================================

(define perturb-mh-transition-base%
  (class mh-transition-base%
    (init-field [temperature 1])
    (field [last-delta-db #f]) ;; HACK for feedback
    (super-new)

    ;; run* : (-> A) Trace -> (U (list* Real Trace TxInfo) (list* 'fail Any TxInfo))
    (define/override (run* thunk last-trace)
      (define last-db (trace-db last-trace))
      (defmatch (cons delta-db delta-ll-R/F) (perturb last-trace))
      (set! last-delta-db delta-db)
      (define ctx
        (new db-stochastic-ctx%
             (last-db last-db)
             (delta-db delta-db)
             (ll-R/F delta-ll-R/F)))
      ;; Run program
      (define result (with-verbose> (send ctx run thunk)))
      (match result
        [(cons 'okay sample-value)
         (define ll-diff (get-field ll-diff ctx))
         (define ll-R/F (get-field ll-R/F ctx))
         (define current-trace (send ctx make-trace sample-value))
         (define threshold
           (accept-threshold last-trace ll-R/F current-trace ll-diff))
         (list* threshold current-trace (vector 'delta delta-db))]
        [(cons 'fail fail-reason)
         (list* 'fail fail-reason (vector 'delta delta-db))]))

    ;; perturb : Trace -> (cons DB Real)
    (abstract perturb)

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/public (accept-threshold last-trace ll-R/F current-trace ll-diff)
      (define other-factor (accept-threshold* last-trace current-trace))
      (cond [(or (= other-factor -inf.0) (= other-factor +inf.0))
             other-factor]
            [else
             (define ll-diff-obs (traces-obs-diff current-trace last-trace))
             (+ ll-R/F (/ (+ ll-diff ll-diff-obs) temperature) other-factor)]))

    ;; accept-threshold* : Trace Trace -> Real
    ;; Computes (log) of additional factors of accept threshold.
    ;; If +/-inf.0, then that is taken as accept factor (to avoid
    ;; possible NaN from arithmetic).
    (define/public (accept-threshold* last-trace current-trace)
      0)

    (define/override (feedback success?)
      (for ([key (in-hash-keys last-delta-db)])
        (feedback/key key success?))
      (set! last-delta-db #f)
      (super feedback success?))
    (define/public (feedback/key key success?)
      (void))
    ))

;; ============================================================

(define single-site-mh-transition%
  (class perturb-mh-transition-base%
    (init-field zone
                proposal)
    (inherit-field last-delta-db)
    (field [proposed 0]
           [resampled 0])
    (super-new)

    (define/override (info i)
      (iprintf i "== Transition (single-site #:zone ~e)\n" zone)
      (super info i)
      (iprintf i "Proposal perturbs: ~s\n" proposed)
      (iprintf i "Fall-through perturbs: ~s\n" resampled)
      (send proposal info i))

    ;; perturb : Trace -> (cons DB Real)
    (define/override (perturb last-trace)
      (define last-db (trace-db last-trace))
      (define key-to-change
        (db-pick-a-key (trace-db last-trace) zone))
      (vprintf "key to change = ~s\n" key-to-change)
      (define-values (delta-db ll-R/F)
        (cond [key-to-change
               (match (hash-ref last-db key-to-change)
                 [(entry zones dist value ll)
                  (defmatch (cons e ll-R/F)
                    (perturb-a-key key-to-change dist value zones))
                  (values (hash key-to-change e) ll-R/F)])]
              [else (values '#hash() 0)]))
      (set! last-delta-db delta-db)
      (cons delta-db ll-R/F))

    ;; perturb-a-key : Address Dist Value Zones -> (cons Entry Real)
    (define/public (perturb-a-key key dist value zones)
      (defmatch (cons value* R-F)
        (cond [(send proposal propose1 key zones dist value)
               => (lambda (r) (set! proposed (add1 proposed)) r)]
              [else
               (set! resampled (add1 resampled))
               (propose:resample dist value)]))
      (vprintf "PROPOSED from ~e to ~e\n" value value*)
      (vprintf "  R/F = ~s\n" (exp R-F))
      (define ll* (dist-pdf dist value* #t))
      (unless (ll-possible? ll*)
        (eprintf "proposal produced impossible value\n  dist: ~e\n  value: ~e\n"
                 dist value*))
      (cons (entry zones dist value* ll*) R-F))

    (define/override (accept-threshold* last-trace current-trace)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      (define nchoices (db-count (trace-db current-trace) #:zone zone))
      (define last-nchoices (db-count (trace-db last-trace) #:zone zone))
      (cond [(zero? last-nchoices)
             +inf.0]
            [else
             ;; Note: assumes we pick uniformly from all choices.
             ;; R = (log (/ 1 nchoices))        = (- (log nchoices))
             ;; F = (log (/ 1 last-nchoices))   = (- (log last-nchoices))
             ;; convert to inexact so (log 0.0) = -inf.0
             (define R (- (log (exact->inexact nchoices))))
             (define F (- (log (exact->inexact last-nchoices))))
             (- R F)]))

    (define/override (feedback/key key success?)
      (send proposal feedback key success?))
    ))

;; ============================================================

(define multi-site-mh-transition%
  (class perturb-mh-transition-base%
    (init-field zone proposal)
    (inherit-field last-delta-db)
    (super-new)

    (define/override (info i)
      (iprintf i "== Transition (multi-site #:zone ~e)\n" zone)
      (super info i))

    ;; perturb : Trace -> (cons DB Real)
    (define/override (perturb last-trace)
      (define last-db (trace-db last-trace))
      (define delta-db
        (for/hash ([(key e) (in-hash last-db)]
                   #:when (entry-in-zone? e zone))
          (values key proposal)))
      (set! last-delta-db delta-db)
      (cons delta-db 0))

    ;; accept-threshold* : Trace Real Trace Real Boolean -> Real
    (define/override (accept-threshold* last-trace current-trace)
      (cond [(zero? (trace-nchoices last-trace))
             +inf.0]
            [else ;; FIXME: what if current-nchoices != last-nchoices ???
             0]))

    (define/override (feedback/key key success?)
      (send proposal feedback key success?))
    ))

;; ============================================================

(define mixture-mh-transition%
  (class* object% (mh-transition<%>)
    (init transitions weights)
    (super-new)

    (define tx-dist (make-discrete-dist* transitions weights #:normalize #f))

    (define/public (run thunk last-trace)
      (define r (send (dist-sample tx-dist) run thunk last-trace))
      r)

    (define/public (info i)
      (iprintf i "== Transition (mixture ...)\n")
      (for ([tx (discrete-dist-values tx-dist)])
        (send tx info (+ i 2))))
    ))

;; ============================================================

(define rerun-mh-transition%
  (class perturb-mh-transition-base%
    (super-new)
    (define/override (info i)
      (iprintf "== Transition (rerun)\n")
      (super info i))
    (define/override (perturb last-trace)
      (cons '#hash() +inf.0))
    (define/override (accept-threshold . _args)
      +inf.0)))
