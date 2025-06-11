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
;; DeltaDB = (Hashof Address (U Entry Proposal))

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
;; Proposals

(define (proposal? v)
  (is-a? v proposal<%>))

(define proposal<%>
  (interface ()
    propose1    ;; Addr Dist[X] X -> (U #f (cons X Real))
    ;; Used for single-site proposal, or when adjusted variables are
    ;; known to be independent (dist parameters will not change from
    ;; previous values).

    propose2    ;; Addr Dist[X] Dist[X] X -> (U #f (cons X Real))
    ;; Used for multi-site proposal, when change to one variable might
    ;; affect parameters of other proposal variables.
    ))

;; propose1:resample : Dist[X] X -> (cons X Real)
(define (propose1:resample dist prev-value)
  ;; Just resample from same dist.
  ;; Then Kt(x|x') = Kt(x)  = (dist-pdf dist prev-value)
  ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist new-value)
  (propose2:resample dist dist prev-value))

;; propose2:resample : Dist[X] Dist[X] X -> (cons X Real)
(define (propose2:resample new-dist old-dist old-value)
  ;; If multiple variables changed, earlier changes may have affected dist params.
  ;; - (Forward) So resample from new-dist.
  ;; - (Reverse) Earlier reverse changes produce old-dist, so use old-dist pdf.
  ;; Then Q(x|x') = Q(x) =  (dist-pdf old-dist old-value)
  ;;  and Q(x'|x) = Q(x') = (dist-pdf new-dist new-value)
  (define new-value (dist-sample new-dist))
  (define lR (dist-pdf old-dist old-value #t))
  (define lF (dist-pdf new-dist new-value #t))
  (cons new-value (- lR lF)))

(define proposal%
  (class* object% (proposal<%>)
    (init-field propose1-proc   ;; Addr Dist[X] X -> (U #f (cons X Real))
                propose2-proc   ;; Addr Dist[X] Dist[X] X -> (U #f (cons X Real))
                propose-dist)   ;; Addr Dist[X] X -> Dist[X]
    (define/public (propose1 addr dist prev-value)
      (define r (propose1* addr dist prev-value))
      (if (proposal? r) (send r propose1 addr dist prev-value) r))
    (define/private (propose1* addr dist prev-value)
      (or (and propose1-proc (propose1-proc addr dist prev-value))
          (propose2* addr dist dist prev-value)))
    (define/public (propose2 addr new-dist prev-dist prev-value)
      (define r (propose2* addr new-dist prev-dist prev-value))
      (if (proposal? r) (send r propose2 addr new-dist prev-dist prev-value) r))
    (define/private (propose2* addr new-dist prev-dist prev-value)
      (or (and propose2-proc (propose2-proc addr new-dist prev-dist prev-value))
          (and propose-dist
               (cond [(propose-dist addr new-dist prev-value)
                      => (lambda (fd)
                           (define new-value (dist-sample fd))
                           (cond [(propose-dist addr prev-dist new-value)
                                  => (lambda (rd)
                                       (define lF (dist-pdf fd new-value #t))
                                       (define lR (dist-pdf rd prev-value #t))
                                       (cons new-value (- lR lF)))]
                                 [else #f]))]
                     [else #f]))))
    ))

(define resample-proposal%
  (class* object% (proposal<%>)
    (super-new)
    (define/public (propose1 addr dist value)
      (propose1:resample dist value))
    (define/public (propose2 addr new-dist prev-dist prev-value)
      (propose2:resample new-dist prev-dist prev-value))
    ))

(define drift-proposal%
  (class* object% (proposal<%>)
    (init-field params?     ;; Boolean
                scale)      ;; PosReal or (Addr Dist -> PosReal)
    (super-new)
    (define/public (propose1 addr dist value)
      (dist-drift1 dist value params? (get-scale addr dist)))
    (define/public (propose2 addr new-dist old-dist old-value)
      (dist-drift2 new-dist old-dist old-value params? (get-scale addr new-dist)))
    (define/private (get-scale addr dist)
      (if (real? scale) scale (scale addr dist)))
    ))

(define (proposal #:propose1 [propose1 #f]
                  #:propose2 [propose2 #f]
                  #:propose-dist [propose-dist #f])
  (new proposal%
       (propose1-proc propose1)
       (propose2-proc propose2)
       (propose-dist propose-dist)))

(define (resample-proposal)
  (new resample-proposal%))

(define (drift-proposal #:params? [params? #t] #:scale [scale 1.0])
  (new drift-proposal% (params? params?) (scale scale)))


;; ============================================================
;; Tracing stochastic context

(define tracing-stochastic-ctx%
  (class plain-stochastic-ctx%
    (inherit fail)
    (init-field prev-db       ;; DB, not mutated
                delta-db      ;; DB, not mutated
                [ll-R/F 0.0]  ;; real, mutated
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
      (cond [(entry? delta-e)
             (log-mh-info "DELTA ~s: ~e, ~e => ~e, ~e" addr
                          (entry-dist prev-e) (entry-value prev-e)
                          (entry-dist delta-e) (entry-value delta-e))
             (unless (equal? (entry-dist delta-e) dist)
               (error 'sample "internal error: delta has wrong dist"))
             (db-add! addr delta-e prev-e)
             (entry-value delta-e)]
            [(proposal? proposal)
             (match-define (entry prev-dist prev-value _) prev-e)
             (match-define (cons new-value l-R/F)
               (or (send proposal propose2 addr dist prev-dist prev-value)
                   (begin (log-mh-info "Late proposal returned #f; resampling")
                          (propose2:resample dist prev-dist prev-value))))
             (log-mh-info "DELTA ~s: ~e, ~e => ~e, ~e; R/F=~s" addr
                          prev-dist prev-value dist new-value (exp l-R/F))
             (define new-ll (dist-pdf dist new-value #t))
             (db-add! addr (entry dist new-value new-ll) prev-e)
             (set! ll-R/F (+ ll-R/F l-R/F))
             new-value]))

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

    ;; get-ll-R/F : -> Real
    ;; Mutated by late proposals, eg from multi-site MH.
    (define/public (get-ll-R/F) ll-R/F)

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
           [accepts 0]        ;; Nat
           [rejects 0])       ;; Nat
    (super-new)

    ;; step : Transition -> (values Boolean Trace TxInfo)
    (define/public (step transition)
      (define-values (new-trace new-txinfo)
        (send transition run thunk last-trace))
      (cond [new-trace
             (set! last-trace new-trace)
             (set! accepts (add1 accepts))
             (values #t new-trace new-txinfo)]
            [else
             (set! rejects (add1 rejects))
             (values #f last-trace new-txinfo)]))

    ;; steps : Nat Transition #:collect (Boolean Trace TxInfo -> X)
    ;;      -> (Vectorof X)
    (define/public (steps n transition
                          #:lag [lag 0]
                          #:collect [collect #f])
      (define v (and collect (make-vector n)))
      (for ([i (in-range n)])
        (for ([j (in-range lag)])
          (step transition))
        (call-with-values
         (lambda () (step transition))
         (lambda (accepted? trace txinfo)
           (when collect
             (vector-set! v i (collect accepted? trace txinfo))))))
      (or v (void)))
    ))
