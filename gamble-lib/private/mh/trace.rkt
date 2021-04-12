;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/list
         racket/class
         racket/match
         racket/vector
         "../interfaces.rkt"
         "../dist/base.rkt"
         "../dist/density.rkt")
(provide (all-defined-out))

;; ============================================================

;; An Address is Any value, used to distinguish a call to sample.



;; ============================================================

;; A DeltaDB is (Hashof Address DeltaEntry)
;; A DeltaEntry is one of
;; - Entry      -- single-site changes only
;; - Proposal

;; ============================================================

(define db-stochastic-ctx%
  (class* object% (stochastic-ctx/run<%>)
    (init-field last-db     ;; not mutated
                delta-db    ;; not mutated
                [on-fresh-choice #f] ;; (U #f (-> Any))
                [ll-R/F 0]  ;; sum of all Reverse proposal log-densities
                            ;; minus sum of all Forward proposal log-densities
                [escape-prompt (make-continuation-prompt-tag)])

    (field [current-db (make-hash)]
           [ll-free  0]  ;; sum of ll of all entries in current-db
           [ll-obs   0]  ;; sum of ll of all observations
           [ll-diff  0]  ;; see below
           [ddim     0]) ;; density dimension

    (super-new)

    ;; ll-diff = SUM_{k in K} (- (entry-ll current-db[k]) (entry-ll last-db[k]))
    ;;           where K = dom(current-db) intersected with dom(last-db)

    ;; Observations do not affect ll-diff, only ll-obs.

    ;; make-trace : Any -> Trace
    ;; Should only be called after run, once current-db has stopped changing.
    (define/public (make-trace value)
      (trace value current-db ll-free ll-obs ddim))

    ;; db-add! : Address Entry -> Void
    ;; Add entry to current-db and update ll-free, ll-obs.
    ;; When last-e is not #f, also update ll-diff.
    (define/private (db-add! context e [last-e #f])
      (hash-set! current-db context e)
      (set! ll-free (+ ll-free (entry-ll e)))
      (when last-e
        (set! ll-diff (+ ll-diff (- (entry-ll e) (entry-ll last-e))))))

    ;; run : (-> A) -> (U (cons 'okay A) (cons 'fail any))
    ;; Run a prob prog using this stochastic ctx, populate current-db, etc.
    (define/public (run thunk)
      (parameterize ((current-stochastic-ctx this))
        (call-with-continuation-prompt
         (lambda () (cons 'okay (apply/delimit thunk)))
         escape-prompt)))

    (define/public (fail reason)
      (abort-current-continuation
       escape-prompt
       (lambda () (cons 'fail reason))))

    ;; The sample method records random choices by mutating
    ;; current-db. At the end of execution, current-db contains a
    ;; complete record of all random choices made by the program;
    ;; if accepted, it typically becomes a new execution's last-db.

    (define/public (sample dist)
      (define context (ADDR-mark))
      ;; If choice address (context) is in current-db, likely error unless
      ;; memoized function (FIXME: shouldn't happen w/ real memoization).
      ;; Otherwise, consult delta-db (represents proposed changes), then
      ;; last-db; they are kept separate to avoid data structure copy and
      ;; also to provide more precise debugging messages.
      (cond [(hash-ref current-db context #f)
             => (lambda (e)
                  (sample/collision dist context e))]
            [(hash-ref delta-db context #f)
             => (lambda (e)
                  (sample/delta dist context e))]
            [(hash-ref last-db context #f)
             => (lambda (e)
                  (sample/last dist context e))]
            [else (sample/new dist context #t)]))

    (define/private (sample/collision dist context e)
      (vprintf "- COLLISION~a ~e: ~s\n" (if (mem-context? context) " (MEM)" "") dist context)
      (collision-error context))

    (define/private (sample/new dist context print?)
      (when on-fresh-choice (on-fresh-choice))
      (define value (dist-sample dist))
      (define ll (dist-pdf dist value #t))
      (when print? (vprintf "NEW ~e: ~s = ~e\n" dist context value))
      (db-add! context (entry (current-zones) dist value ll))
      value)

    (define/private (sample/delta dist context e)
      (define last-e (hash-ref last-db context #f))
      (cond [(not last-e)
             (error 'sample "internal error: choice in delta but not last\n  context: ~e"
                    context)]
            [(proposal? e)
             (define zones (current-zones))
             (defmatch (entry _ old-dist old-value old-ll) last-e)
             (match (send e propose2 context zones old-dist dist old-value)
               [(list* new-value R F)
                (vprintf "PERTURBED ~e: ~s = ~e\n" dist context new-value)
                (define new-ll (dist-pdf dist new-value #t))
                (db-add! context (entry zones dist new-value new-ll) last-e)
                (set! ll-R/F (+ ll-R/F (- R F)))
                new-value]
               [#f
                (error 'sample "no proposal for dist\n  dist: ~e" dist)])]
            [(and (entry? e) (equal? (entry-dist e) dist))
             (vprintf "PERTURBED ~e: ~s = ~e\n" dist context (entry-value e))
             (db-add! context e last-e)
             (entry-value e)]
            [else
             (vprintf "MISMATCH ~e / ~e: ~s\n" e dist context)
             (error 'sample "internal error: choice in delta has wrong type\n  context: ~e"
                    context)]))

    (define/private (sample/last dist context e)
      (cond [(equal? (entry-dist e) dist)
             (vprintf "REUSED ~e: ~s = ~e\n" dist context (entry-value e))
             (db-add! context e)
             (entry-value e)]
            [(dists-same-type? (entry-dist e) dist)
             (let ([new-ll (dist-pdf dist (entry-value e) #t)])
               (cond [(logspace-nonzero? new-ll)
                      (define value (entry-value e))
                      (define new-e (entry (entry-zones e) dist value new-ll))
                      (vprintf "RESCORE ~e: ~s = ~e\n" dist context value)
                      (db-add! context new-e e)
                      value]
                     [else
                      (fail 'sample/rescore)]))]
            [else
             (vprintf "MISMATCH ~e / ~e: ~s\n" (entry-dist e) dist context)
             (sample/new dist context #f)]))

    (define/public (observe-sample dist val scale)
      (define context (ADDR-mark))
      (define lscale (log scale))
      (cond [(hash-ref current-db context #f) ;; COLLISION
             => (lambda (e)
                  (vprintf "OBS COLLISION ~e / ~e: ~s\n" (entry-dist e) dist context)
                  (collision-error context))]
            [(hash-ref delta-db context #f) ;; impossible
             => (lambda (e)
                  (error 'observe-sample "internal error: cannot perturb an observation"))]
            [else
             (vprintf "OBS ~e: ~s = ~e\n"  dist context val)
             (define ll (+ lscale (dist-pdf dist val #t)))
             (unless (dist-has-mass? dist)
               (set! ddim (add1 ddim)))
             (cond [(logspace-nonzero? ll)
                    (set! ll-obs (+ ll-obs ll))]
                   [else (fail 'observation)])]))

    (define/private (mem-context? context)
      (and (pair? context)
           (let ([frame (last context)])
             (and (list? frame) (memq 'mem frame)))))

    (define/private (collision-error context)
      (error 'sample
             (string-append "collision in random choice database"
                            ";\n check that the sampler module uses `#lang prob'"
                            "\n  address: ~e")
             context))

    (define/public (mem f)
      (let ([context (ADDR-mark)]
            [memo-table (make-hash)])
        (lambda args
          (call-with-immediate-continuation-mark OBS-mark
            (lambda (obs)
              (hash-ref! memo-table args
                         (lambda ()
                           (apply/delimit
                            (lambda ()
                              (with-continuation-mark ADDR-mark (list (list 'mem args context))
                                (with-continuation-mark OBS-mark obs
                                  (apply f args))))))))))))
    ))
