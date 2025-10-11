;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/class
         racket/list
         racket/match
         "../dist.rkt"
         "../base.rkt"
         "../addr.rkt"
         "../model/graph-trace.rkt"
         "../util/real.rkt"
         "../util/dnum.rkt")
(provide (all-defined-out))

(define-logger mcmc)

;; MCMC: sample X ~ f where f(x) is represented implicitly by program
;; - sample space is represented via `db` mapping "keys" to dist and value
;; - `trace` wraps `db` with summary information
;; - `transition` computes the next step in the Markov chain
;;   - eg, single-site MH, enumerative Gibbs, etc
;; - `proposal` determines how to change single db entry


;; ============================================================
;; DB, Entry

;; Entry = (entry Dist[X] X Real Tag)
;; - lprior is redundant, always (dist-pdf dist value #t)
(struct entry (dist value lprior tag) #:prefab)

;; DeltaDB = (Hashof DBKey (U Entry Proposal))
;; DBKey = Addr

;; DB = (db (Vectorof DBKey) (Vectorof Entry) Nat (U #f (Hashof DBKey Entry)))
(struct tracedb (ks es n eh) #:mutable)

(define DB-INIT-CAPACITY 10)

;; new-db : Nat -> DB
(define (new-db [capacity DB-INIT-CAPACITY])
  (tracedb (make-vector capacity #f) (make-vector capacity #f) 0 #f))

;; db-ref : DB DBKey DB/#f -> Entry/#f
;; The hintdb is db-in-progress of running model; if model accesses
;; same RVs as previous run, then no need to create hash.
(define (db-ref db key #:hint [hintdb #f])
  (match-define (tracedb ks es n eh) db)
  (define hint (and hintdb (tracedb-n hintdb)))
  (define hintk (and hint (< hint n) (vector-ref ks hint)))
  (cond [(and hintk (equal? hintk key)) (vector-ref es hint)]
        [else (hash-ref (or eh (db-hash! db)) key #f)]))

(define (db-set! db key e)
  (define (grow n) (max DB-INIT-CAPACITY (* 2 n)))
  (match-define (tracedb ks es n _) db)
  (cond [(< n (vector-length ks))
         (vector-set! ks n key)
         (vector-set! es n e)
         (set-tracedb-n! db (add1 n))]
        [else
         (define n* (grow n))
         (define ks* (make-vector n* #f))
         (define es* (make-vector n* #f))
         (vector-copy! ks* 0 ks 0 n*)
         (vector-copy! es* 0 es 0 n*)
         (set-tracedb-ks! db ks*)
         (set-tracedb-es! db es*)
         (db-set! db key e)]))

(define (db-hash! db)
  (or (tracedb-eh db)
      (let ([eh (make-hash)])
        (match-define (tracedb ks es n #f) db)
        (for ([k (in-vector ks 0 n)] [e (in-vector es 0 n)])
          (hash-set! eh k e))
        (set-tracedb-eh! db eh)
        (unless (= n (hash-count eh))
          (error 'sample "internal address collision~a\n  address: ~e"
                 "detected after the model finished executing"
                 (for/first ([k (in-vector ks 0 n)]
                             [e (in-vector es 0 n)]
                             #:when (not (eq? (hash-ref eh k #f) e)))
                   k)))
        eh)))

(define (db-finish! db)
  (match-define (tracedb ks es n _) db)
  (void (db-hash! db))
  (when (< n (vector-length ks))
    (define ks* (make-vector n #f))
    (define es* (make-vector n #f))
    (vector-copy! ks* 0 ks 0 n)
    (vector-copy! es* 0 es 0 n)
    (set-tracedb-ks! db ks*)
    (set-tracedb-es! db es*)))

;; db-complete! : DB DB -> Void
;; Completes a slice DB, whose keys are a subset of prev-db. The completed DB
;; should be like prev-db except for the slice's entries.
(define (db-complete! slice-db prev-db)
  (match-define (tracedb prev-ks prev-es prev-n _) prev-db)
  (define slice-eh (db-hash! slice-db))
  (define ks* (make-vector prev-n))
  (define es* (make-vector prev-n))
  (for ([i (in-naturals)]
        [k (in-vector prev-ks 0 prev-n)]
        [prev-e (in-vector prev-es 0 prev-n)])
    (vector-set! ks* i k)
    (vector-set! es* i (hash-ref slice-eh k prev-e)))
  (set-tracedb-eh! slice-db #f)
  (set-tracedb-ks! slice-db ks*)
  (set-tracedb-es! slice-db es*)
  (set-tracedb-n! slice-db prev-n))

;; db->keys+tags+dists : DB -> (values (Listof DBKey) (Listof Tag) (Listof Dist))
;; List order corresponds to program order!
(define (db->keys+tags+dists db)
  (match-define (tracedb ks es n _) db)
  (values (for/list ([k (in-vector ks 0 n)]) k)
          (for/list ([e (in-vector es 0 n)]) (entry-tag e))
          (for/list ([e (in-vector es 0 n)]) (entry-dist e))))

;; db-count : DB (Tag -> Boolean) -> Nat
(define (db-count db [ok-tag? #f])
  (cond [ok-tag?
         (match-define (tracedb _ es n _) db)
         (for/sum ([e (in-vector es 0 n)])
           (if (ok-tag? (entry-tag e)) 1 0))]
        [else (tracedb-n db)]))

;; db-random : DB (Tag -> Boolean) -> #f or (cons DBKey Entry)
(define (db-random db [ok-tag? #f])
  (match-define (tracedb ks es n _) db)
  (cond [(zero? n) #f]
        [ok-tag?
         (let loop ([iters 2])
           (cond [(zero? iters)
                  (define okn (db-count db ok-tag?))
                  (and (> okn 0) (db-nth db (random okn) ok-tag?))]
                 [else (let ([i (random n)])
                         (define e (vector-ref es i))
                         (cond [(ok-tag? (entry-tag e)) (cons (vector-ref ks i) e)]
                               [else (loop (sub1 iters))]))]))]
        [else (let ([i (random n)]) (cons (vector-ref ks i) (vector-ref es i)))]))

;; db-nth : DB Nat (Tag -> Boolean) -> (cons DBKey Entry)
;; PRE: hash contains at least n+1 ok entries
(define (db-nth db m [ok-tag? #f])
  (match-define (tracedb ks es n _) db)
  (cond [ok-tag?
         (let loop ([index 0] [m m])
           (unless (< index n) (error 'db-nth "internal error"))
           (define e (vector-ref es index))
           (if (ok-tag? (entry-tag e))
               (if (zero? m)
                   (cons (vector-ref ks index) e)
                   (loop (add1 index) (sub1 m)))
               (loop (add1 index) m)))]
        [else (vector-ref es m)]))


;; ============================================================
;; Trace

;; A Trace is (trace Any DB Real Real)
;; - lprs is the sum of all log priors from db
;; - lobs is the sum of all log likelihoods of observations
(struct trace (value db lprs lobs))

(define init-trace (trace #f (new-db 0) -inf.0 -inf.0))

;; trace-lj : Trace/#f -> Real
;; Returns the log joint probability of the trace (priors and observations).
(define (trace-lj tr)
  (if tr (+ (trace-lprs tr) (trace-lobs tr)) -inf.0))

;; traces-obs-diff : Trace Trace -> Real
(define (traces-obs-diff tr1 tr2)
  (- (trace-lobs tr1) (trace-lobs tr2)))

#;
;; traces-same-structure? : Trace Trace Boolean -> Boolean
;; If quick?, we already know new-trace has no *new* keys.
(define (traces-same-structure? prev-trace new-trace [quick? #f])
  (define prev-db (trace-db prev-trace))
  (define new-db (trace-db new-trace))
  (and (= (db-count (trace-db prev-trace))
          (db-count (trace-db new-trace)))
       (or quick?
           (let ([prev-eh (db-hash! prev-db)]
                 [new-eh (db-hash! new-db)])
             (for/and ([key (in-hash-keys prev-eh)])
               (hash-has-key? new-eh key))))))

;; ============================================================
;; Transition interface

(define (mcmc-transition? v)
  (is-a? v mcmc-transition<%>))

(define mcmc-transition<%>
  (interface ()
    ;; type TxInfo
    run  ;; ModelRunner Trace -> (values (U Trace #f) TxInfo)
    ))

;; Transition/SingleSite[X] =
;; - #f                                           -- resample from prior
;; - (proposal-value X Real)                      -- new value, log(R/F)
;; - (proposal-kernel (Tag Dist[X] X -> Dist[X])  -- proposal kernel
;; - (Tag Dist[X] X -> Transition/SingleSite[X])  -- depends on chosen key
;; - implementation of mcmc-transition/single-site<%>

(define (mcmc-transition/single-site? v)
  (is-a? v mcmc-transition/single-site<%>))

(define mcmc-transition/single-site<%>
  (interface ()
    run/key    ;; ModelRunner Trace DBKey Entry -> (values Trace/#f TxInfo)
    ))

;; ============================================================
;; Proposals

;; A ProposalValue[X] is (propose-value X Real)
;; - l-R/F represents log(R/F) component of MH accept ratio.
(struct proposal-value (value l-R/F) #:transparent)

;; A ProposalKernel[X] is (proposal-kernel (X -> Dist[X]))
(struct proposal-kernel (kernel) #:transparent)

;; propose/kernel : (X -> Dist[X]) X -> (values X Real)
(define (propose/kernel kernel prev-value)
  (define dF (kernel prev-value))
  (define new-value (dist-sample dF))
  (define dR (kernel new-value))
  (define lF (dist-pdf dF new-value #t))
  (define lR (dist-pdf dR prev-value #t))
  (values new-value (- lR lF)))

;; propose/resample : Dist[X] X -> (values X Real)
(define (propose/resample dist prev-value)
  ;; Just resample from same dist.
  ;; Then Kt(x|x') = Kt(x)  = (dist-pdf dist prev-value)
  ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist new-value)
  (propose2/resample dist dist prev-value))

;; propose2/resample : Dist[X] Dist[X] X -> (values X Real)
(define (propose2/resample new-dist old-dist old-value)
  ;; If multiple variables changed, earlier changes may have affected dist params.
  ;; - (Forward) So resample from new-dist.
  ;; - (Reverse) Earlier reverse changes produce old-dist, so use old-dist pdf.
  ;; Then Q(x|x') = Q(x) =  (dist-pdf old-dist old-value)
  ;;  and Q(x'|x) = Q(x') = (dist-pdf new-dist new-value)
  (define new-value (dist-sample new-dist))
  (define lR (dist-pdf old-dist old-value #t))
  (define lF (dist-pdf new-dist new-value #t))
  (values new-value (- lR lF)))

;; ============================================================
;; Tracing stochastic context

(define tracing-stochastic-ctx%
  (class base-stochastic-ctx%
    (inherit fail)
    (inherit-field escape-prompt)
    (init-field prev-db       ;; DB, not mutated
                delta-db      ;; DeltaDB, not mutated
                [sumlprs 0.0] ;; real, mutated; sum of lprior of all entries in current-db
                [sumlobs 0.0] ;; real, mutated; sum of log likelihoods of all observations
                [disallow-new/who #f])  ;; #f or Symbol
    (field [current-db (new-db)]    ;; DB, mutated
           [l-R/F 0.0]              ;; real, mutated
           [diff-lprs  0.0])        ;; see get-diff-lprs below
    (super-new)

    ;; The sample method records random choices by mutating
    ;; current-db. At the end of execution, current-db contains a
    ;; complete record of all random choices made by the program;
    ;; if accepted, it typically becomes a new execution's prev-db.

    (define/override (sample dist tag addr)
      (if addr
          (super sample dist tag addr)
          (with-get-ADDR addr (super sample dist tag addr))))

    (define/override (-sample dist tag addr)
      (unless addr
        (error 'sample "unique address is required for MCMC sampler~a\n  dist: ~e"
               ";\n address management failed because of uninstrumented code"
               dist))
      (define delta-e (hash-ref delta-db addr #f))
      (define prev-e (db-ref prev-db addr #:hint current-db))
      (when prev-e
        (unless (equal? tag (entry-tag prev-e))
          (error 'sample "tag changed\n  old tag: ~e\n  new tag: ~e" (entry-tag prev-e) tag)))
      (cond [delta-e (sample/delta dist tag addr delta-e prev-e)]
            [prev-e (sample/prev dist tag addr prev-e)]
            [else (sample/new dist tag addr #f)]))

    (define/private (sample/delta dist tag addr delta-e prev-e)
      (unless prev-e (error 'sample "internal error: in delta, not in previous"))
      (match-define (entry prev-dist prev-value _ _) prev-e)
      (match delta-e
        [(entry delta-dist delta-value _ _)
         (log-mcmc-info "DELTA ~s: ~e, ~e => ~e, ~e" addr
                        prev-dist prev-value
                        delta-dist delta-value)
         (unless (equal? delta-dist dist)
           (error 'sample "internal error: delta has wrong dist"))
         (add-entry! addr delta-e prev-e)
         delta-value]
        [(proposal-value new-value proposal-l-R/F)
         (log-mcmc-info "DELTA ~s: ~e, ~e => ~e, ~e; R/F=~s" addr
                        prev-dist prev-value dist new-value (exp proposal-l-R/F))
         (define new-lpr (dist-pdf dist new-value #t))
         (add-entry! addr (entry dist new-value new-lpr tag) prev-e)
         (set! l-R/F (+ l-R/F proposal-l-R/F))
         new-value]
        [(proposal-kernel kernel)
         (define-values (new-value proposal-l-R/F)
           (propose/kernel kernel prev-value))
         (log-mcmc-info "DELTA ~s: ~e, ~e => ~e, ~e; R/F=~s" addr
                        prev-dist prev-value dist new-value (exp proposal-l-R/F))
         (define new-lpr (dist-pdf dist new-value #t))
         (add-entry! addr (entry dist new-value new-lpr tag) prev-e)
         (set! l-R/F (+ l-R/F proposal-l-R/F))
         new-value]))

    (define/private (sample/prev dist tag addr prev-e)
      (cond [(equal? (entry-dist prev-e) dist)
             (log-mcmc-info "REUSE ~s: ~e, ~e" addr dist (entry-value prev-e))
             (add-entry! addr prev-e)
             (entry-value prev-e)]
            [(eq? (dist-type (entry-dist prev-e)) (dist-type dist))
             (define new-lpr (dist-pdf dist (entry-value prev-e) #t))
             (cond [(logspace-nonzero? new-lpr)
                    (define value (entry-value prev-e))
                    (define new-e (entry dist value new-lpr tag))
                    (log-mcmc-info "RESCORE ~s: ~e, ~e" addr dist value)
                    (add-entry! addr new-e prev-e)
                    value]
                   [else (fail)])]
            [else (sample/new dist addr prev-e)]))

    (define/private (sample/new dist tag addr prev-e)
      (when disallow-new/who
        (error-structural 'sample disallow-new/who "new random variable"))
      (define value (dist-sample dist))
      (define lpr (dist-pdf dist value #t))
      (if prev-e
          (log-mcmc-info "MISMATCH ~s: ~e, ~e => ~e, ~e" addr
                         (entry-dist prev-e) (entry-value prev-e)
                         dist value)
          (log-mcmc-info "NEW ~s: ~e, ~e" addr dist value))
      (add-entry! addr (entry dist value lpr tag) prev-e)
      value)

    (define/override (-dscore who dn)
      (set! sumlobs (+ sumlobs (dnum->logspace-real dn)))
      (when (logspace-zero? sumlobs) (fail)))

    (define/override (mem f addr)
      (define (do-mem addr)
        (define (af . args)
          (with-put-ADDR (addr-add-mem addr args)
            (apply f args)))
        (super mem (procedure-reduce-arity af (procedure-arity f) 'memoized-function) addr))
      (if addr (do-mem addr) (with-get-ADDR addr (do-mem addr))))

    (define/override (run-model m addr)
      (if addr
          (super run-model m addr)
          (with-get-ADDR addr (super run-model m addr))))

    ;; ----------------------------------------

    ;; make-trace : Any -> Trace
    ;; Should only be called after run, once current-db has stopped changing.
    (define/public (make-trace value)
      (db-finish! current-db)
      (trace value current-db sumlprs sumlobs))

    ;; get-diff-lprs : -> Real
    ;; Returns SUM_{k in K} (- (entry-lprior current-db[k]) (entry-lprior prev-db[k]))
    ;; where K = dom(current-db) intersected with dom(prev-db).
    (define/public (get-diff-lprs) diff-lprs)

    ;; get-l-R/F : -> Real
    ;; Mutated by late proposals, eg from multi-site MH.
    (define/public (get-l-R/F) l-R/F)

    ;; add-entry! : DBKey Entry (U #f Entry) -> Void
    ;; Add entry to current-db and update sumlprs, sumlobs.
    ;; When prev-e is not #f, also update diff-lprs.
    (define/private (add-entry! key e [prev-e #f])
      (db-set! current-db key e)
      (define lpr (entry-lprior e))
      (set! sumlprs (+ sumlprs lpr))
      (when prev-e
        (define prev-lpr (entry-lprior prev-e))
        (set! diff-lprs (+ diff-lprs (- lpr prev-lpr)))))
    ))

;; initializing-tracing-stochastic-ctx%
;; Used to initialize model. If get-value fails, sample from prior.
(define initializing-tracing-stochastic-ctx%
  (class tracing-stochastic-ctx%
    (init-field get-value)  ;; (Tag Dist ProposalValue/#f) -> ProposalValue/#f
    (inherit-field prev-db current-db)
    (super-new [delta-db (hash)])

    (define real-prev-db prev-db)   ;; DB, not mutated
    (set! prev-db (new-db))         ;; DB, mutated

    ;; Hack: override -sample to add entries to prev-db on demand.
    (define/override (-sample dist tag addr)
      (define prev-e (db-ref real-prev-db addr #:hint current-db))
      (match (get-value tag dist (and prev-e (proposal-value (entry-value prev-e) 0.0)))
        [(proposal-value value _)
         (define lpr (dist-pdf dist value #t))
         (hash-set! prev-db addr (entry dist value lpr tag))]
        [#f
         (when prev-e (hash-set! prev-db addr prev-e))])
      (super -sample dist tag addr))
    ))

;; replay-stochastic-ctx%
;; Used to build graph w/o changes to previous db.
;; PRE: model runs w/o error, w/o failure
(define replay-stochastic-ctx%
  (class base-stochastic-ctx%
    (inherit fail)
    (init-field who         ;; Symbol
                prev-db)    ;; DB, not mutated
    (super-new)

    (define/override (-sample dist tag addr)
      (cond [(db-ref prev-db addr)
             => (lambda (e)
                  (entry-value e))]
            [addr
             (error-structural 'sample who "new random variable")]
            [else
             (error 'sample "unique address is required for MCMC sampler~a\n  dist: ~e"
                    ";\n address management failed because of uninstrumented code"
                    dist)]))

    (define/override (-dscore who dn)
      (when (dnum-zero? dn) (fail)))
    ))

;; ============================================================
;; Runner

;; How an MCMC transition runs a model depends on 3 questions:
;; - Does transition allow structural changes?
;; - Can I reuse existing graph/slice?
;; - Should I invest effort in creating graph/slice?
;;
;; Answers:
;; - Init:   yes, no,    no
;; - MH:     yes, try?,  don't care
;; - EGibbs: no,  reuse, create
;; - Slice:  no,  reuse, create

(define model-runner%
  (class object%
    (init-field mdl         ;; Model
                init-addr)  ;; Address
    (super-new)

    ;; EvalSlice = (DeltaDB Boolean -> Trace/#f)
    ;; If mini? (2nd) arg to eval is true, then only re-eval stochastic parts
    ;; (skip nodes recomputing result), and return partial trace.

    ;; For fixed values of structural variables, graph is constant.
    ;; For fixed graph, nodes in slice are constant, but restl{prs,obs} not constant.

    (define graph-cache #f)             ;; #f or Graph
    (define slice-cache (make-hash))    ;; DBKeys => Slice
    (define eval-cache #f)              ;; #f or (list* DBKeys (Box Boolean) EvalSlice)

    ;; In eval-cache, box indicates whether graph boxes are ok (consistent).
    ;; If a slice eval fails or partial, then boxes may be left in inconsistent state.
    ;; Then cannot switch slices w/ same graph, must discard (complicated to fix).

    (define/public (show)
      (when graph-cache
        (send graph-cache show)
        (match eval-cache
          [(list* keys _)
           (printf "Current slice: ~e\n" keys)
           (send graph-cache show-slice keys)]
          [#f (void)])))

    ;; invalidate-cache! : -> Void
    (define/private (invalidate-cache!)
      ;; Discarding graph, no need for run-fix-graph!.
      (set! graph-cache #f)
      (hash-clear! slice-cache)
      (set! eval-cache #f))

    ;; get-graph : Symbol Trace -> Graph
    (define/private (get-graph who prev-trace)
      (or graph-cache
          (let ()
            (define base-ctx (new replay-stochastic-ctx%
                                  (who who) (prev-db (trace-db prev-trace))))
            (define graph (new graph% (ctx base-ctx) (init-addr init-addr)))
            (send graph eval-top who mdl)
            (set! graph-cache graph)
            graph)))

    ;; get-slice : (Listof DBKey) -> Slice
    ;; PRE: graph-cache is set
    (define/private (get-slice keys)
      (hash-ref! slice-cache keys (lambda () (send graph-cache get-slice keys))))

    ;; get-cached-eval : (Listof DBKey) -> EvalSlice/#f
    (define/private (get-cached-eval keys)
      (let ([eval-cache eval-cache])
        (and eval-cache
             (unbox (cadr eval-cache))
             (equal? (car eval-cache) keys)
             (cddr eval-cache))))

    ;; ----------------------------------------

    ;; eval/ctx : StochasticCtx -> Trace/#f
    ;; - invalidate slices, graph; does full eval
    ;; - allows structural change (if ctx does)
    (define/public (eval/ctx ctx)
      (match (send ctx run-top mdl init-addr)
        [(list new-value)
         (invalidate-cache!)
         (send ctx make-trace new-value)]
        [#f #f]))

    ;; eval/fresh : DeltaDB Trace -> (values Trace/#f StochasticCtx)
    ;; - invalidate slices, graph; does full eval
    ;; - allows structural change
    (define/public (eval/fresh delta-db prev-trace)
      (define ctx (new tracing-stochastic-ctx%
                       (prev-db (trace-db prev-trace))
                       (delta-db delta-db)))
      (values (eval/ctx ctx) ctx))

    ;; eval/try-reuse : DeltaDB Trace -> (values Trace/#f StochasticCtx)
    ;; - try reuse slice, graph; if reuse fails, do full eval
    ;; - allows structural change
    (define/public (eval/try-reuse delta-db prev-trace)
      (cond [(get-cached-eval (hash-keys delta-db))
             => (lambda (eval-slice)
                  (with-handlers* ([exn:fail:gamble:structural?
                                    (lambda (e) (eval/fresh delta-db prev-trace))])
                    (define ctx-b (box #f))
                    (define new-trace (eval-slice delta-db #f ctx-b))
                    (values new-trace (unbox ctx-b))))]
            [else (eval/fresh delta-db prev-trace)]))

    ;; get-slice-posterior : Symbol DBKey Trace -> Dist/#f
    ;; Get dist of key conditioned on rest of trace (suitable for Gibbs).
    (define/public (get-slice-posterior who key prev-trace)
      (define graph (get-graph who prev-trace))
      (slice->posterior-dist (get-slice (list key))))

    ;; make-eval-slice : Symbol (Listof DBKey) Trace -> EvalSlice
    (define/public (make-eval-slice who keys prev-trace)
      (cond [(get-cached-eval keys)
             => values]
            [else
             (define-values (eval-slice consistent-b)
               (make-eval-slice* who keys prev-trace))
             (set! eval-cache (list* keys consistent-b eval-slice))
             eval-slice]))

    (define/private (make-eval-slice* who keys prev-trace)
      (define graph (get-graph who prev-trace))
      (define s (get-slice keys))
      (define-values (slice-lprs slice-lobs) (slice->lprs+lobs s))
      (define rest-lprs (- (trace-lprs prev-trace) slice-lprs))
      (define rest-lobs (- (trace-lobs prev-trace) slice-lobs))
      (define prev-db (trace-db prev-trace))
      (define consistent-b (box #t)) ;; mutated
      (define (eval-slice delta-db mini? [ctx-b #f])
        (define slice-ctx
          (new tracing-stochastic-ctx%
               (prev-db prev-db)
               (delta-db delta-db)
               (sumlprs rest-lprs)
               (sumlobs rest-lobs)
               (disallow-new/who who)))
        (set-box! consistent-b #f)
        (match (send slice-ctx run-top (lambda () (slice-eval who s slice-ctx mini?)))
          [(list result)
           (define new-trace (send slice-ctx make-trace result))
           (when ctx-b (set-box! ctx-b slice-ctx))
           (unless mini?
             (complete-slice-trace! new-trace prev-db)
             (set-box! consistent-b #t))
           new-trace]
          [#f #f]))
      (values eval-slice consistent-b))

    (define/private (complete-slice-trace! slice-trace prev-db)
      (db-complete! (trace-db slice-trace) prev-db))

    ;; ----------------------------------------

    ;; make-caching-eval-trace : Symbol DBKey[X] Trace -> (X Boolean -> Trace/#f)
    ;; Caching evaluator for single-key slices. Only mini evals are cached.
    (define/public (make-caching-eval-trace who key prev-trace)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) (db-ref prev-db key))
      (define trace-cache (make-hash)) ;; Hash[X => Trace/#f]
      (hash-set! trace-cache prev-value prev-trace)
      (define eval-slice (make-eval-slice who (list key) prev-trace))
      ;; eval-trace : X Boolean -> Trace/#f
      (define (eval-trace new-value mini?)
        (define new-lpr (dist-pdf dist new-value #t))
        (define new-trace (eval-slice (hash key (entry dist new-value new-lpr tag)) mini?))
        new-trace)
      ;; caching-eval-trace : X Boolean -> Trace/#f
      (define (caching-eval-trace new-value mini?)
        (if mini?
            (hash-ref! trace-cache new-value (lambda () (eval-trace new-value #t)))
            (eval-trace new-value #f)))
      caching-eval-trace)
    ))
