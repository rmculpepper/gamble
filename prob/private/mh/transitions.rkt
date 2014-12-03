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
    (init-field proposal
                record-obs?
                [temperature 1])
    (field [proposed 0]
           [resampled 0])
    (super-new)

    (field [last-delta-db #f]) ;; HACK: used in feedback (FIXME)

    (define/override (info i)
      (super info i)
      (iprintf i "Proposal perturbs: ~s\n" proposed)
      (iprintf i "Fall-through perturbs: ~s\n" resampled)
      (send proposal info i))

    ;; run* : (-> A) Trace -> (U (cons Real Trace) (cons 'fail any))
    (define/override (run* thunk last-trace)
      (define last-db (trace-db last-trace))
      (defmatch (cons delta-db R-F) (perturb last-trace))
      (set! last-delta-db delta-db)
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
      (defmatch (cons value* R-F)
        (cond [(send proposal propose key zones dist value)
               => (lambda (r) (set! proposed (add1 proposed)) r)]
              [else
               (set! resampled (add1 resampled))
               (propose:resample dist value)]))
      (when (verbose?)
        (eprintf "  PROPOSED from ~e to ~e\n" value value*)
        (eprintf "    R/F = ~s\n" (exp R-F)))
      (define ll* (dist-pdf dist value* #t))
      (unless (ll-possible? ll*)
        (error 'perturb "proposal produced impossible value\n  dist: ~e\n  value: ~e"
               dist value*))
      (cons (entry zones dist value* ll* #f) R-F))

    ;; accept-threshold : Trace Real Trace Real Boolean -> Real
    (abstract accept-threshold)

    (define/override (feedback success?)
      (for ([key (in-hash-keys last-delta-db)])
        (send proposal feedback key success?))
      (set! last-delta-db #f)
      (super feedback success?))
    ))

;; ============================================================

(define single-site-mh-transition%
  (class perturb-mh-transition-base%
    (inherit perturb-a-key)
    (inherit-field temperature last-delta-db)
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

;; ============================================================

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

;; ============================================================

(define abstract-multi-tx-transition%
  (class* object% (mh-transition<%>)
    (super-new)

    (define/public (info i)
      (for ([tx (get-transitions)])
        (send tx info (+ i 2))))

    (define/public (run thunk last-trace)
      (define r (send (get-transition) run thunk last-trace))
      (update-transition (and r #t))
      r)

    (abstract get-transition)
    (abstract get-transitions)
    (abstract update-transition)
    ))

(define cycle-mh-transition%
  (class abstract-multi-tx-transition%
    (init transitions)
    (super-new)

    (define transitions* (list->vector transitions))
    (define current 0)

    (define/override (info i)
      (iprintf i "== Transition (cycle ...)\n")
      (super info i))

    (define/override (get-transition)
      (vector-ref transitions* current))
    (define/override (get-transitions)
      transitions*)
    (define/override (update-transition success?)
      (when success?
        (set! current (modulo (add1 current) (vector-length transitions*)))))
    ))

(define sequence-mh-transition%
  (class abstract-multi-tx-transition%
    (init transitions)
    (super-new)

    (define transitions* (list->vector transitions))
    (define current 0)

    (define/override (info i)
      (iprintf i "== Transition (sequence ...)\n")
      (super info i))

    (define/override (get-transition)
      (vector-ref transitions* current))
    (define/override (get-transitions)
      transitions*)
    (define/override (update-transition success?)
      (when (and success?
                 (< current (sub1 (vector-length transitions*))))
        (set! current (add1 current))))
    ))

(define mixture-mh-transition%
  (class abstract-multi-tx-transition%
    (init transitions weights)
    (super-new)

    (define tx-dist (make-discrete-dist* transitions weights #:normalize #f))

    (define/override (info i)
      (iprintf i "== Transition (mixture ...)\n")
      (super info i))

    (define/override (get-transition)
      (dist-sample tx-dist))
    (define/override (get-transitions)
      (discrete-dist-values tx-dist))
    (define/override (update-transition success?) (void))
    ))

;; ============================================================

(define rerun-mh-transition%
  (class perturb-mh-transition-base%
    (super-new [proposal (proposal:resample)] [record-obs? #f])
    (define/override (info i)
      (iprintf "== Transition (rerun)\n")
      (super info i))
    (define/override (perturb last-trace)
      (cons '#hash() +inf.0))
    (define/override (accept-threshold . _args)
      +inf.0)))
