;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (rename-in racket/match [match-define defmatch])
         "db.rkt"
         "../interfaces.rkt"
         "../dist.rkt")
(provide (all-defined-out))

;; ============================================================

;; Proposal = (Zones Dist Value -> (U (cons Value Real) #f))
;; The function returns a new value and the proposal's R-F.

(define ((proposal:drift scale-factor) key zones dist value)
  (define r (*drift dist value))
  (when (verbose?)
    (match r
      [(cons value* R-F)
       (eprintf "  DRIFTED from ~e to ~e\n" value value*)
       (eprintf "    R/F = ~s\n" (exp R-F))]
      [_ (void)]))
  r)

(define (propose:resample key zones dist value)
  ;; Just resample from same dist.
  ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
  ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
  (define value* (dist-sample dist))
  (define R (dist-pdf dist value #t))
  (define F (dist-pdf dist value* #t))
  (when (verbose?)
    (eprintf "  RESAMPLED from ~e to ~e\n" value value*)
    (eprintf "    R = ~s, F = ~s\n" (exp R) (exp F)))
  (cons value* (- R F)))

(define (proposal:resample) propose:resample)

(define ((proposal:or . proposals) key zones dist value)
  (for/or ([proposal (in-list proposals)])
    (proposal key zones dist value)))

;; ============================================================

;; default-proposal : (parameterof Proposer)
(define default-proposal (make-parameter propose:resample))
