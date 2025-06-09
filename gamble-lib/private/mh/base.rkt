;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/list
         racket/match
         "../dist.rkt")
(provide (all-defined-out))

(define-logger mh)

;; ============================================================

;; simple-mh-step : (X -> Real) X
;;                  #:next (U #f (X ->> (values X Real)))
;;                  #:kernel (U #f (X -> (Dist X)))
;;               -> (values Boolean X)
(define (simple-mh-step ll x #:next [next #f] #:kernel [q #f])
  (define-values (x* ll-R/F)
    (cond [next
           (next x)]
          [q
           (define qf (q x))
           (define x* (dist-sample qf))
           (define qb (q x*))
           (values x* (- (dist-pdf qb x #t) (dist-pdf qf x* #t)))]
          [else
           (error 'simple-mh-step "missing #:next or #:kernel argument")]))
  (define accept (+ ll-R/F (ll x*) (- (ll x))))
  (if (<= (log (random)) accept) (values #t x*) (values #f x)))


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

;; DV = (Hashof Address Entry)

;; Entry = (entry Dist[X] X Density)
(struct entry (dist value dn) #:prefab)

;; hash-random-key : Hash[K => V] -> K
(define (hash-random-key h)
  (when (zero? (hash-count h)) (error 'hash-random-key "empty hash"))
  (hash-nth-key h (random (hash-count h))))

;; hash-nth-key : Hash[K => V] Nat -> K
(define (hash-nth-key h n)
  (define iter
    (for/fold ([iter (hash-iterate-first h)]) ([i (in-range n)])
      (hash-iterate-next h iter)))
  (hash-iterate-key k iter))


;; ============================================================
;; Tracing stochastic context

(define tracing-stochastic-ctx%
  (class plain-stochastic-ctx%
    (init-field prev-db       ;; DB, not mutated
                delta-db      ;; DB, not mutated
                [ll-R/F 0.0]) ;; Real, log(Q(backward) / Q(forward))

    (field [current-db (make-hash)] ;; DB, mutated
           [ll-free  0.0]     ;; sum of ll of all entries in current-db
           [ll-obs   0.0]     ;; sum of ll of all observations
           [ll-diff  0.0]     ;; see below
           [obs-ddim   0])    ;; density dimension

    (super-new)

    ;; ll-diff = SUM_{k in K} (- (entry-ll current-db[k]) (entry-ll prev-db[k]))
    ;;           where K = dom(current-db) intersected with dom(prev-db)

    ;; Observations do not affect ll-diff, only ll-obs.

    ;; The sample method records random choices by mutating
    ;; current-db. At the end of execution, current-db contains a
    ;; complete record of all random choices made by the program;
    ;; if accepted, it typically becomes a new execution's prev-db.

    (define/public (sample dist addr)
      ;; If choice address is in current-db, likely error unless memoized
      ;; function (FIXME: shouldn't happen w/ real memoization).
      ;; Otherwise, consult delta-db (represents proposed changes), then
      ;; prev-db; they are kept separate to avoid data structure copy and
      ;; also to provide more precise debugging messages.
      (cond [(hash-ref current-db addr #f)
             ;; Collision
             (error 'sample
                    (string-append "collision in random choice database"
                                   "\n  address: ~e")
                    address)]
            [(hash-ref delta-db addr #f)
             => (lambda (e) (sample/delta dist addr e))]
            [(hash-ref prev-db addr #f)
             => (lambda (e) (sample/prev dist addr e))]
            [else (sample/new dist addr #f)]))

    (define/private (sample/delta dist addr e)
      (define prev-e (hash-ref prev-db addr #f))
      (unless prev-e (error 'sample "internal error: in delta, not in previous"))
      (log-mh-info "DELTA ~s: ~e, ~e => ~e, ~e" addr
                   (entry-dist prev-e) (entry-value prev-e)
                   (entry-dist e) (entry-value e))
      (unless (and (entry? e) (equal? (entry-dist e) dist))
        (error 'sample "internal error: delta has wrong dist"))
      (db-add! context e prev-e)
      (entry-value e))

    (define/private (sample/prev dist addr e)
      (cond [(equal? (entry-dist e) dist)
             (log-mh-info "REUSE ~s: ~e, ~e" addr dist (entry-value e))
             (db-add! addr e)
             (entry-value e)]
            [(eq? (dist-type (entry-dist e)) (dist-type dist))
             (define new-ll (dist-pdf dist (entry-value e) #t))
             (cond [(logspace-nonzero? new-ll)
                    (define value (entry-value e))
                    (define new-e (entry dist value new-ll))
                    (log-mh-info "RESCORE ~s: ~e, ~e" addr dist value)
                    (db-add! context new-e e)
                    value]
                   [else (fail 'sample-rescore)])]
            [else (sample/new dist context e)]))

    (define/private (sample/new dist addr prev-e)
      (when on-fresh-choice (on-fresh-choice))
      (define value (dist-sample dist))
      (define ll (dist-pdf dist value #t))
      (if prev-e
          (log-mh-info "MISMATCH ~s: ~e, ~e => ~e, ~e" addr
                       (entry-dist prev-e) (entry-value prev-e)
                       dist value)
          (log-mh-info "NEW ~s: ~e, ~e" addr dist value))
      (db-add! add (entry dist value ll) prev-e)
      value)

    (define/override (dscore dn)
      (set! ll-obs (+ ll-obs (density->real dn #t)))
      (set! obs-ddim (+ obs-ddim (density-ddim dn))))

    ;; ----------------------------------------

    ;; make-trace : Any -> Trace
    ;; Should only be called after run, once current-db has stopped changing.
    (define/public (make-trace value)
      (trace value current-db ll-free ll-obs ddim))

    ;; db-add! : Address Entry (U #f Entry) -> Void
    ;; Add entry to current-db and update ll-free, ll-obs.
    ;; When prev-e is not #f, also update ll-diff.
    (define/private (db-add! context e [prev-e #f])
      (hash-set! current-db context e)
      (set! ll-free (+ ll-free (entry-ll e)))
      (when prev-e
        (set! ll-diff (+ ll-diff (- (entry-ll e) (entry-ll prev-e))))))
    ))

;; ============================================================

(define ADDR-mark (gensym 'ADDR))

(define (get-addr who)
  (or (continuation-mark-set-first #f ADDR-mark)
      (error who "no address available")))

;; Delimit call-site tracking.
;; Can't test using normal (f arg ...) syntax, because testing call-sites 
;; would be part of context! Use (apply/delimit f arg ...) instead.
(define (apply/delimit f [args null])
  (with-continuation-mark ADDR-mark #f
    (apply f args)))

(define address-tracing-stochastic-ctx%
  (class tracing-stochastic-ctx%
    (super-new)

    ;; run : (-> A) -> (U (cons 'okay A) (cons 'fail any))
    ;; Run a prob prog using this stochastic ctx, populate current-db, etc.
    (define/override (run thunk)
      (super run (lambda () (apply/delimit thunk))))

    (define/override (sample dist addr)
      (let ([addr (or addr (get-addr))])
        (super sample dist addr)))

    (define/override (mem f)
      (define addr (get-addr))
      (super mem
             (lambda args
               (with-continuation-mark ADDR-mark (list (list 'mem args addr))
                 (apply/delimit f args)))))

    (define/private (mem-context? context)
      (and (pair? context)
           (let ([frame (last context)])
             (and (list? frame) (memq 'mem frame)))))
    ))
