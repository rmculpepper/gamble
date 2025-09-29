;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/list
         racket/match
         "../dist.rkt"
         "../base.rkt"
         "../addr.rkt"
         "../model/graph-trace.rkt"
         "../util/real.rkt"
         "../util/density.rkt")
(provide (all-defined-out))

(define-logger mcmc)

;; MCMC: sample X ~ f where f(x) is represented implicitly by program
;; - sample space is represented via `db` mapping "keys" to dist and value
;; - `trace` wraps `db` with summary information
;; - `transition` computes the next step in the Markov chain
;;   - eg, single-site MH, enumerative Gibbs, etc
;; - `proposal` determines how to change single db entry


;; ============================================================
;; Trace, DB, Entry

;; A Trace is (trace Any DB Real Real)
;; - lprs is the sum of all log priors from db
;; - lobs is the sum of all log likelihoods of observations
(struct trace (value db lprs lobs))

(define init-trace (trace #f (hash) -inf.0 -inf.0))

;; trace-lj : Trace/#f -> Real
;; Returns the log joint probability of the trace (priors and observations).
(define (trace-lj tr)
  (if tr (+ (trace-lprs tr) (trace-lobs tr)) -inf.0))

;; traces-obs-diff : Trace Trace -> Real
(define (traces-obs-diff tr1 tr2)
  (- (trace-lobs tr1) (trace-lobs tr2)))

;; traces-same-structure? : Trace Trace Boolean -> Boolean
;; If quick?, we already know new-trace has no *new* keys.
(define (traces-same-structure? prev-trace new-trace [quick? #f])
  (define prev-db (trace-db prev-trace))
  (define new-db (trace-db new-trace))
  (and (= (hash-count (trace-db prev-trace))
          (hash-count (trace-db new-trace)))
       (or quick?
           (for/and ([key (in-hash-keys prev-db)])
             (hash-has-key? new-db key)))))

;; DB = (Hashof DBKey Entry)
;; DeltaDB = (Hashof DBKey (U Entry Proposal))
;; DBKey = Addr

;; Entry = (entry Dist[X] X Real Tag)
;; - lprior is redundant, always (dist-pdf dist value #t)
(struct entry (dist value lprior tag) #:prefab)

;; db-entry-tag : DBKey Entry -> Tag
(define (db-entry-tag key e) (entry-tag e))

;; db-random-key : DB (Tag -> Boolean) -> K or #f
(define (db-random-key h [ok-tag? #f])
  (define n (db-count* h ok-tag?))
  (and (> n 0) (db-nth-key h (random n) ok-tag?)))

;; db-count* : DB (Tag -> Boolean) -> Nat
(define (db-count* h [ok-tag? #f])
  (if ok-tag?
      (for/sum ([(k e) (in-hash h)] #:when (ok-tag? (db-entry-tag k e))) 1)
      (hash-count h)))

;; db-nth-key : DB Nat (Tag -> Boolean) -> DBKey
;; PRE: hash contains at least n+1 ok keys
(define (db-nth-key h n [ok-tag? #f])
  (cond [ok-tag?
         (let loop ([iter (hash-iterate-first h)] [n n])
           (define key (hash-iterate-key h iter))
           (if (ok-tag? (db-entry-tag key (hash-iterate-value h iter)))
               (if (zero? n) key (loop (hash-iterate-next h iter) (sub1 n)))
               (loop (hash-iterate-next h iter) n)))]
        [else
         (let loop ([iter (hash-iterate-first h)] [n n])
           (cond [(zero? n) (hash-iterate-key h iter)]
                 [else (loop (hash-iterate-next h iter) (sub1 n))]))]))

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
                delta-db      ;; DB, not mutated
                [sumlprs 0.0] ;; real, mutated; sum of lprior of all entries in current-db
                [sumlobs 0.0] ;; real, mutated; sum of log likelihoods of all observations
                [disallow-new/who #f] ;; #f or Symbol
                [init-addr (current-init-addr)])
    (field [current-db (make-hash)] ;; DB, mutated
           [l-R/F 0.0]              ;; real, mutated
           [diff-lprs  0.0])        ;; see get-diff-lprs below

    (super-new [logspace? #t])

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
      (when (hash-has-key? current-db addr)
        (error 'sample "duplicate address\n  address: ~e" addr))
      (define delta-e (hash-ref delta-db addr #f))
      (define prev-e (hash-ref prev-db addr #f))
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
         (db-add! addr delta-e prev-e)
         delta-value]
        [(proposal-kernel kernel)
         (define-values (new-value proposal-l-R/F)
           (propose/kernel kernel prev-value))
         (log-mcmc-info "DELTA ~s: ~e, ~e => ~e, ~e; R/F=~s" addr
                        prev-dist prev-value dist new-value (exp proposal-l-R/F))
         (define new-lpr (dist-pdf dist new-value #t))
         (db-add! addr (entry dist new-value new-lpr tag) prev-e)
         (set! l-R/F (+ l-R/F proposal-l-R/F))
         new-value]))

    (define/private (sample/prev dist tag addr prev-e)
      (cond [(equal? (entry-dist prev-e) dist)
             (log-mcmc-info "REUSE ~s: ~e, ~e" addr dist (entry-value prev-e))
             (db-add! addr prev-e)
             (entry-value prev-e)]
            [(eq? (dist-type (entry-dist prev-e)) (dist-type dist))
             (define new-lpr (dist-pdf dist (entry-value prev-e) #t))
             (cond [(logspace-nonzero? new-lpr)
                    (define value (entry-value prev-e))
                    (define new-e (entry dist value new-lpr tag))
                    (log-mcmc-info "RESCORE ~s: ~e, ~e" addr dist value)
                    (db-add! addr new-e prev-e)
                    value]
                   [else (fail '(gamble zero-score sample-rescore))])]
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
      (db-add! addr (entry dist value lpr tag) prev-e)
      value)

    (define/override (-dscore who dn)
      (set! sumlobs (+ sumlobs (density->real dn #t)))
      (when (logspace-zero? sumlobs) (fail `(gamble zero-score ,who))))

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

    (define/override (run-top top)
      (match top
        [(? model? m)
         (super run-top (lambda () (run-model m init-addr)))]
        [_ (super run-top top)]))

    ;; ----------------------------------------

    ;; make-trace : Any -> Trace
    ;; Should only be called after run, once current-db has stopped changing.
    (define/public (make-trace value)
      (trace value current-db sumlprs sumlobs))

    ;; get-diff-lprs : -> Real
    ;; Returns SUM_{k in K} (- (entry-lprior current-db[k]) (entry-lprior prev-db[k]))
    ;; where K = dom(current-db) intersected with dom(prev-db).
    (define/public (get-diff-lprs) diff-lprs)

    ;; get-l-R/F : -> Real
    ;; Mutated by late proposals, eg from multi-site MH.
    (define/public (get-l-R/F) l-R/F)

    ;; db-add! : DBKey Entry (U #f Entry) -> Void
    ;; Add entry to current-db and update sumlprs, sumlobs.
    ;; When prev-e is not #f, also update diff-lprs.
    (define/private (db-add! key e [prev-e #f])
      (hash-set! current-db key e)
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
    (init-field get-value)  ;; (Tag Dist[X] -> (U #f (list X)))
    (inherit-field prev-db)
    (super-new [prev-db (make-hash)] ;; mutated
               [delta-db (hash)])

    ;; Hack: override -sample to add entries to prev-db on demand.
    (define/override (-sample dist tag addr)
      (match (get-value tag dist)
        [(list value)
         (define lpr (dist-pdf dist value #t))
         (hash-set! prev-db addr (entry dist value lpr tag))]
        [_ (void)])
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
      (cond [(hash-ref prev-db addr #f)
             => (lambda (e)
                  (entry-value e))]
            [addr
             (error-structural 'sample who "new random variable")]
            [else
             (error 'sample "unique address is required for MCMC sampler~a\n  dist: ~e"
                    ";\n address management failed because of uninstrumented code"
                    dist)]))

    (define/override (-dscore who dn)
      (when (density-zero? dn) (fail `(gamble zero-score ,who))))
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
    (init-field mdl)  ;; Model
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
        (printf "Graph:\n")
        (send graph-cache show)
        (match eval-cache
          [(list* keys _)
           (eprintf "\nCurrent slice: ~e\n" keys)
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
            (define graph (new graph% (ctx base-ctx)))
            (void (send graph eval-top mdl))
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
      (match (send ctx run-top mdl)
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
    ))

(define (complete-slice-trace! slice-trace prev-db)
  (define slice-db (trace-db slice-trace))
  (for ([(key entry) (in-hash prev-db)])
    (unless (hash-has-key? slice-db key)
      (hash-set! slice-db key entry))))
