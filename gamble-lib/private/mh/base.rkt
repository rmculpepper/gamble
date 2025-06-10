;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/list
         racket/match
         "../dist.rkt"
         "../interfaces.rkt"
         "../util/real.rkt"
         "../util/density.rkt")
(provide (all-defined-out))

(define-logger mh)

;; MCMC: sample X ~ f where f(x) is represented implicitly by program
;; - sample space is represented via `db` mapping "labels" to dist and value
;; - `trace` wraps `db` with summary information
;; - `transition` computes the next step in the Markov chain
;;   - eg, single-site MH, enumerative Gibbs, etc
;; - `proposal` determines how to change single db entry


;; ============================================================
;; Trace, DB, Entry

;; A Trace is (trace Any DB Real Real Nat)
(struct trace (value db ll-free ll-obs obs-ddim))

;; DB = (Hashof Address Entry)

;; Entry = (entry Dist[X] X Density)
(struct entry (dist value density) #:prefab)

;; trace-ll : Trace -> Real
(define (trace-ll tr)
  (match-define (trace _ _ ll-free ll-obs _) tr)
  (+ ll-free ll-obs))

;; traces-obs-diff : Trace Trace -> Real
(define (traces-obs-diff tr1 tr2)
  (match-define (trace _ _ _ ll-obs1 obs-ddim1) tr1)
  (match-define (trace _ _ _ ll-obs2 obs-ddim2) tr2)
  (cond [(= obs-ddim1 obs-ddim2) (- ll-obs1 ll-obs2)]
        [(< obs-ddim1 obs-ddim2) +inf.0]
        [else -inf.0]))

;; traces-same-structure? : Trace Trace Boolean -> Boolean
(define (traces-same-structure? prev-trace new-trace [quick? #f])
  (define prev-db (trace-db prev-trace))
  (define new-db (trace-db new-trace))
  (and (= (hash-count (trace-db prev-trace))
          (hash-count (trace-db new-trace)))
       (or quick?
           (for/and ([addr (in-hash-keys prev-db)])
             (hash-has-key? new-db addr)))))

;; hash-random-key : Hash[K => V] (K -> Boolean) -> K or #f
(define (hash-random-key h [ok-key? #f])
  (define n (hash-count* h ok-key?))
  (and (> n 0) (hash-nth-key h ok-key?)))

;; hash-count* : Hash[K => V] (U #f (K -> Boolean)) -> Nat
(define (hash-count* h [ok-key? #f])
  (if ok-key? (for/sum ([k (in-hash-keys h)] #:when (ok-key? k)) 1) (hash-count h)))

;; hash-nth-key : Hash[K => V] Nat (K -> Boolean) -> K
;; PRE: hash contains at least n+1 ok keys
(define (hash-nth-key h n [ok-key? #f])
  (cond [ok-key?
         (let loop ([iter (hash-iterate-first h)] [n n])
           (define key (hash-iterate-key h iter))
           (if (ok-key? key)
               (if (zero? n) key (loop (hash-iterate-next h iter) (sub1 n)))
               (loop (hash-iterate-next h iter) n)))]
        [else
         (define iter
           (for/fold ([iter (hash-iterate-first h)]) ([i (in-range n)])
             (hash-iterate-next h iter)))
         (hash-iterate-key h iter)]))


;; ============================================================
;; Transition interface

(define (mcmc-transition? v)
  (is-a? v mcmc-transition<%>))

(define mcmc-transition<%>
  (interface ()
    ;; type TxInfo
    run  ;; (-> A) Trace -> (values (U Trace #f) TxInfo)
    ))


;; ============================================================
;; Proposal interface

#;
(define proposal<%>
  (interface ()
    propose1 ;; Key Zones Dist Value -> (U (cons Value Real) #f)
    propose2 ;; Key Zones Dist Dist Value -> (U (list* Value Real Real) #f)
    accinfo  ;; -> AccInfo
    feedback ;; Key Boolean -> Void
    ))


;; ============================================================

#;
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

#;
;; kernel-transition : (T -> Dist[T]) -> Transition[T,T]
(define ((kernel-transition q) t1)
  (define q12 (q t1))
  (define t2 (dist-sample q12))
  (define q21 (q t2))
  (values t2 (density-logratio (dist-density q21 t1) (dist-density q12 t2))))

#;
;; single-site : Transition[Entry,EntryDelta] -> Transition[Trace,TraceDelta]
(define ((single-site etx) tr)
  (define addr (trace-pick-a-key tr))
  (match-define (and ent (entry dist value dn)) (hash-ref tr addr))
  (define-values (dent txlogratio) (etx ent))
  ;; Warning: txlogratio is not complete, if addr value affect control flow.
  (values (hash addr dent) txlogratio))

#;
;; resample-tx : Transition[Entry,EntryDelta]
(define (resample-tx ent)
  (match-define (entry dist v1 dn) ent)
  (define v2 (dist-sample dist))
  (values (entry dist v2 (dist-density dist v2)) 0))


;; ============================================================
;; Tracing stochastic context

(define tracing-stochastic-ctx%
  (class plain-stochastic-ctx%
    (inherit fail)
    (init-field prev-db       ;; DB, not mutated
                delta-db      ;; DB, not mutated
                [disallow-new/who #f]) ;; #f or Symbol
    (field [current-db (make-hash)] ;; DB, mutated
           [ll-free  0.0]     ;; sum of ll of all entries in current-db
           [ll-obs   0.0]     ;; sum of ll of all observations
           [ll-diff  0.0]     ;; see get-ll-diff below
           [obs-ddim   0])    ;; density dimension

    (super-new)

    ;; The sample method records random choices by mutating
    ;; current-db. At the end of execution, current-db contains a
    ;; complete record of all random choices made by the program;
    ;; if accepted, it typically becomes a new execution's prev-db.

    (define/override (sample dist addr)
      (when (hash-ref current-db addr #f)
        (error 'sample "duplicate label\n  label: ~e" addr))
      (define delta-e (hash-ref delta-db addr #f))
      (define prev-e (hash-ref prev-db addr #f))
      (cond [delta-e (sample/delta dist addr delta-e prev-e)]
            [prev-e (sample/prev dist addr prev-e)]
            [else (sample/new dist addr #f)]))

    (define/private (sample/delta dist addr delta-e prev-e)
      (unless prev-e (error 'sample "internal error: in delta, not in previous"))
      (log-mh-info "DELTA ~s: ~e, ~e => ~e, ~e" addr
                   (entry-dist prev-e) (entry-value prev-e)
                   (entry-dist delta-e) (entry-value delta-e))
      (unless (and (entry? delta-e) (equal? (entry-dist delta-e) dist))
        (error 'sample "internal error: delta has wrong dist"))
      (db-add! addr delta-e prev-e)
      (entry-value delta-e))

    (define/private (sample/prev dist addr prev-e)
      (cond [(equal? (entry-dist prev-e) dist)
             (log-mh-info "REUSE ~s: ~e, ~e" addr dist (entry-value prev-e))
             (db-add! addr prev-e)
             (entry-value prev-e)]
            [(eq? (dist-type (entry-dist prev-e)) (dist-type dist))
             (define new-ll (dist-pdf dist (entry-value prev-e) #t))
             (cond [(logspace-nonzero? new-ll)
                    (define value (entry-value prev-e))
                    (define new-e (entry dist value new-ll))
                    (log-mh-info "RESCORE ~s: ~e, ~e" addr dist value)
                    (db-add! addr new-e prev-e)
                    value]
                   [else (fail 'sample-rescore)])]
            [else (sample/new dist addr prev-e)]))

    (define/private (sample/new dist addr prev-e)
      (when disallow-new/who
        (error disallow-new/who
               "structural change (sampling new variable) not allowed"))
      (define value (dist-sample dist))
      (define ll (dist-pdf dist value #t))
      (if prev-e
          (log-mh-info "MISMATCH ~s: ~e, ~e => ~e, ~e" addr
                       (entry-dist prev-e) (entry-value prev-e)
                       dist value)
          (log-mh-info "NEW ~s: ~e, ~e" addr dist value))
      (db-add! addr (entry dist value ll) prev-e)
      value)

    (define/override (dscore dn)
      (set! ll-obs (+ ll-obs (density->real dn #t)))
      (set! obs-ddim (+ obs-ddim (density-ddim dn)))
      (when (logspace-zero? ll-obs) (fail 'dscore)))

    ;; ----------------------------------------

    ;; make-trace : Any -> Trace
    ;; Should only be called after run, once current-db has stopped changing.
    (define/public (make-trace value)
      (trace value current-db ll-free ll-obs obs-ddim))

    ;; get-ll-diff : -> Real
    ;; ll-diff = SUM_{k in K} (- (entry-ll current-db[k]) (entry-ll prev-db[k]))
    ;;           where K = dom(current-db) intersected with dom(prev-db)
    ;; Observations do not affect ll-diff, only ll-obs.
    (define/public (get-ll-diff) ll-diff)

    ;; db-add! : Address Entry (U #f Entry) -> Void
    ;; Add entry to current-db and update ll-free, ll-obs.
    ;; When prev-e is not #f, also update ll-diff.
    (define/private (db-add! context e [prev-e #f])
      (hash-set! current-db context e)
      (define ll (density->real (entry-density e) #t))
      (set! ll-free (+ ll-free ll))
      (when prev-e
        (define prev-ll (density->real (entry-density prev-e) #t))
        (set! ll-diff (+ ll-diff (- ll prev-ll)))))
    ))

;; ----------------------------------------
;; Implicit address support

(define ADDR-mark (string->uninterned-symbol "ADDR"))

(define (get-addr who)
  (or (continuation-mark-set-first #f ADDR-mark)
      (error who "no implicit address available")))

;; Delimit implicit address tracking.
(define (apply/delimit f [args null])
  (with-continuation-mark ADDR-mark #f
    (apply f args)))

(define address-tracing-stochastic-ctx%
  (class tracing-stochastic-ctx%
    (super-new)

    (define/override (run thunk)
      (super run (lambda () (apply/delimit thunk))))

    (define/override (sample dist addr)
      (super sample dist (or addr (get-addr 'sample))))

    (define/override (mem f)
      (define addr (get-addr 'mem))
      (super mem
             (lambda args
               (with-continuation-mark ADDR-mark (list (list 'mem args addr))
                 (apply f args)))))
    ))

;; ============================================================

(define mcmc%
  (class object%
    (init-field thunk)        ;; -> A
    (field [last-trace #f]    ;; Trace or #f
           [last-txinfo #f]   ;; ???
           [accepts 0]        ;; Nat
           [rejects 0])       ;; Nat
    (super-new)

    ;; FIXME: add option to keep history, maybe for convergence diagnostics?

    (define/public (step! transition)
      (define-values (new-trace new-txinfo)
        (send transition run thunk last-trace))
      (cond [new-trace
             (set! last-trace new-trace)
             (set! last-txinfo new-txinfo)
             (set! accepts (add1 accepts))
             new-trace]
            [else
             (set! last-txinfo new-txinfo)
             (set! rejects (add1 rejects))
             last-trace]))
    ))
