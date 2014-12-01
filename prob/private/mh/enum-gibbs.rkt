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
         "base.rkt")
(provide (all-defined-out))

;; ============================================================

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
        (error 'enumerative-gibbs:run "no key to change~a"
               (cond [zone (format "\n  zone: ~e" zone)]
                     [else ""])))
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
                     (unless (= nchoices (trace-nchoices last-trace))
                       ;; FIXME: This check is not sufficient to catch all structural choices:
                       ;; if one choice is lost and another added, nchoices stays the same.
                       ;; Could fix by also maintaining and checking number of fresh choices.
                       (error 'enumerative-gibbs "illegal for structural choice"))
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

