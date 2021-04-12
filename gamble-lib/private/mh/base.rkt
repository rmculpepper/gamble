;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/list
         racket/match
         "../dist/base.rkt"
         "../dist/density.rkt")
(provide (all-defined-out))

;; ============================================================
;; Pure MH

;; pure-mh0 : (X -> Dist[X]) (X -> Density) -> (X ~>> X)
;; pure-mh1 : (T -> Dist[T]) (T -> (values R Density)) -> (T ~>> (values T R))
;; pure-mh2 : (T -> Dist[T]) (T -> (values R Density)) T -> (rec S (~>> (values R S)))
(define (pure-mh q run t1)
  (define (make-sampler t1 q12 r1 dn1)
    (define (sampler)
      (define t2 (dist-sample q12))
      (define q21 (q t2))
      (define-values (r2 dn2) (run t2))
      (define accept
        (density-logratio (density* (dist-density q21 t1) dn2)
                          (density* (dist-density q12 t2) dn1)))
      (if (< (log (random)) accept)
          (values r2 (make-sampler t2 (q t2) r2 dn2))
          (values r1 sampler)))
    sampler)
  (define q12 (q t1))
  (define-values (r1 dn1) (run t1))
  (make-sampler t1 q12 r1 dn1))


;; ============================================================
;; Sampler w/ proposal dist (stateful)

(struct mh/q-sampler (q run [st #:mutable])
  #:methods gen:dist/sample
  [(define (*sample self)
     (match self
       [(mh/q-sampler q run (mh-state t1 q12 r1 dn1))
        (define t2 (dist-sample q12))
        (define q21 (q t2))
        (define-values (r2 dn2) (run t2))
        (define accept
          (density-logratio (density* (dist-density q21 t1) dn2)
                            (density* (dist-density q12 t2) dn1)))
        (cond [(< (log (random)) accept)
               (begin (set-mh/q-sampler-st! self (mh-state t2 (q t2) r2 dn2)) r2)]
              [else
               r1])]
       [(mh/q-sampler q run (list t1))
        (define q12 (q t1))
        (define-values (r1 dn1) (run t1))
        (set-mh/q-sampler-st! self (mh-state t1 q12 r1 dn1))
        (*sample self)]))])

(struct mh/q-state (t1 q12 r1 dn1) #:prefab)

;; mh/q-sampler : (T -> Dist[T]) (T -> (values R Density)) T -> Sampler[R]
(define (make-mh/q-sampler q run init-t)
  (mh/q-sampler q run (list init-t)))


;; ============================================================
;; Sampler (stateful)

;; mh-sampler : (PT ~>> (values T Real)) (T ~>> (values PT R Real Density)) PT
;;           -> Sampler[R]
(struct mh-sampler (tx run [st #:mutable])
  #:method gen:dist/sample
  [(define (*sample self)
     (match self
       [(mh/t-sampler tx run (mh-state t1 r1 dn1))
        (define-values (t2 t2logratio) (tx t1))
        (define-values (t2* r2 t2*logratio dn2) (run t2))
        (define accept (+ t2logratio t2*logratio (density-logratio dn2 dn1)))
        (cond [(< (log (random)) accept)
               (begin (set-mh-sampler-st! self (mh-state t2 r2 dn2)) r2)]
              [else r1])]
       [(mh/t-sampler tx run (list t1))
        (define-values (r1 dn1) (run t1))
        (set-mh/t-sampler-st! self (mh/t-state t1 r1 dn1))
        (*sample self)]))])

(struct mh-state (t1 r1 dn1) #:prefab)

(define (make-mh-sampler tx run init-t)
  (mh-sampler tx run (list init-t)))

;; The parameters of an MH sampler:

;; type PT : "pre-trace", may rely on program to "complete" to a full trace
;; type T  : "trace"
;; type R  : result

;; tx : Transition[T,PT] = T -> (values PT Real)
;; Produces a new T and the log ratio of Q(t-old|t-new) / Q(t-new|t-old).
;; Note: the ratio is a simple (log) real number, no ddim.

;; run : Run[PT,T,R] = PT -> (values T R Real Density)
;; Runs PT, producing a (completed) T, a result R, an addition proposal ratio
;; factor for Q(t-old|t-new)/Q(t-new|t-old), and a density for the observations.

;; ============================================================
;; Transitions

;; kernel-transition : (T -> Dist[T]) -> Transition[T,T]
(define ((kernel-transition q) t1)
  (define q12 (q t1))
  (define t2 (dist-sample q12))
  (define q21 (q t2))
  (values t2 (density-logratio (dist-density q21 t1) (dist-density q12 t2))))


;; single-site : Transition[Entry,EntryDelta] -> Transition[Trace,TraceDelta]
(define ((single-site etx) tr)
  (define addr (trace-pick-a-key tr))
  (match-define (and ent (entry dist value dn)) (hash-ref tr addr))
  (define-values (dent txlogratio) (etx ent))
  ;; Warning: txlogratio is not complete, if addr value affect control flow.
  (values (hash addr dent) txlogratio))

;; resample-tx : Transition[Entry,EntryDelta]
(define (resample-tx ent)
  (match-define (entry dist v1 dn) ent)
  (define v2 (dist-sample dist))
  (values (entry dist v2 (dist-density dist v2)) 0))



;; ============================================================
;; Traces

;; Trace = (Hashof Address Entry)
;; Entry = (entry Dist[X] X Density)
(struct entry (dist value dn) #:prefab)

;; trace-count : Trace -> Nat
(define (trace-count trace) (hash-count trace))

;; trace-pick-a-key : Trace -> (U Address #f)
(define (trace-pick-a-key trace)
  (define nchoices (trace-count trace))
  (and (positive? nchoices/zone) (trace-nth trace (random nchoices/zone))))

;; trace-nth : Trace Nat -> Address
(define (trace-nth trace index)
  (let loop ([iter (hash-iterate-first trace)] [i index])
    (cond [(zero? i) (hash-iterate-key trace iter)]
          [else (loop (hash-iterate-next trace iter) (sub1 i))])))


;; ============================================================
;; Pre-traces





;; ============================================================
;; Tracing stochastic context

(define-logger mh)

(define tracing-stochastic-ctx-v0%
  (class plain-stochastic-ctx/run%
    (init-field last-db     ;; not mutated
                delta-db)   ;; not mutated
    (field [current-db (make-hash)]
           [total-ll-R/F 0.0]
           [obs-dn one-density]) ;; density for all observations
    (super-new)

    ;; ll-diff = SUM_{k in K} (- (entry-ll current-db[k]) (entry-ll last-db[k]))
    ;;           where K = dom(current-db) intersected with dom(last-db)

    ;; get-ll-diff : -> Real
    (define/public (get-ll-diff)
      ...)

    ;; get-trace : Any -> Trace
    ;; Should only be called after run, once current-db has stopped changing.
    (define/public (get-trace value) current-db)

    ;; db-add! : Address Entry -> Void
    ;; Add entry to current-db.
    ;; When last-e is not #f, also update ll-diff.
    (define/private (db-add! addr e [last-e #f])
      (hash-set! current-db context e))

    ;; The sample method records random choices by mutating
    ;; current-db. At the end of execution, current-db contains a
    ;; complete record of all random choices made by the program;
    ;; if accepted, it typically becomes a new execution's last-db.

    (define/override (sample dist addr)
      (cond [(hash-ref current-db addr #f)
             => (lambda (e) (sample/collision dist addr e))]
            [(hash-ref delta-db addr #f)
             => (lambda (propose) (sample/delta dist addr propose))]
            [(hash-ref last-db addr #f)
             => (lambda (e) (sample/last dist addr e))]
            [else (sample/new dist addr #t)]))

    (define/private (sample/collision dist addr e)
      (error 'sample "address already used\n  address: ~e" addr))

    (define/private (sample/new dist addr)
      (define value (dist-sample dist))
      (define dn (dist-density dist value))
      (db-add! addr (entry dist value dn))
      value)

    (define/private (sample/delta dist addr propose)
      (define last-e (hash-ref last-db addr #f))
      (unless last-e
        (error 'sample "internal error: in delta but not last\n  address: ~e" addr))
      (define-values (new-value ll-R/F) (de dist last-e))
      (log-mh-debug "PERTURBED ~e: ~s = ~e\n" dist addr new-value)
      (define new-dn (dist-density dist new-value))
      (db-add! addr (entry dist new-value new-dn) last-e)
      (set! total-ll-R/F (+ total-ll-R/F ll-R/F))
      new-value)

    (define/private (sample/last dist addr e)
      (cond [(equal? (entry-dist e) dist)
             (log-mh-debug "REUSED ~e: ~s = ~e\n" dist addr (entry-value e))
             (db-add! addr e)
             (entry-value e)]
            [(dists-same-type? (entry-dist e) dist)
             (define value (entry-value e))
             (define dn (dist-density dist value))
             (cond [(density-zero? dn)
                    ;; FIXME: correct?
                    (define value (dist-sample dist))
                    (define dn (dist-density dist value))
                    (define dn/old (dist-density (entry-dist e) value))
                    (cond [(density-zero? dn/old)
                           ;; must resample forward and backward, okay!
                           (db-add! addr (entry dist value dn) e)
                           value]
                          [else
                           ;; backward would not resample, so fail
                           (fail 'sample/rescore)])]
                   [else
                    (define new-e (entry dist value dn))
                    (log-mh-debug "RESCORE ~e: ~s = ~e\n" dist addr value)
                    (db-add! addr new-e e)
                    value])]
            [else
             (log-mh-debug "MISMATCH ~e / ~e: ~s\n" (entry-dist e) dist addr)
             (sample/new dist addr)]))

    (define/override (dscore dn who)
      (when (density-zero? dn) (fail who))
      (set! obs-dn (density* obs-dn dn)))

    (define/private (mem-context? context)
      (and (pair? context)
           (let ([frame (last context)])
             (and (list? frame) (memq 'mem frame)))))
    ))


#;
(define tracing-stochastic-ctx%
  (class* object% (stochastic-ctx/run<%>)
    (init-field last-db     ;; not mutated
                delta-db    ;; not mutated
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
