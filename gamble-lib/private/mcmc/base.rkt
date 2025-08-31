;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/list
         racket/match
         "../dist.rkt"
         "../base.rkt"
         "../model/addr.rkt"
         "../model/interp.rkt"
         "../util/real.rkt"
         "../util/density.rkt")
(provide (all-defined-out))

(define-logger mcmc)

;; MCMC: sample X ~ f where f(x) is represented implicitly by program
;; - sample space is represented via `db` mapping "labels" to dist and value
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

;; trace-lj : Trace -> Real
;; Returns the log joint probability of the trace (priors and observations).
(define (trace-lj tr)
  (+ (trace-lprs tr) (trace-lobs tr)))

;; traces-obs-diff : Trace Trace -> Real
(define (traces-obs-diff tr1 tr2)
  (- (trace-lobs tr1) (trace-lobs tr2)))

;; traces-same-structure? : Trace Trace Boolean -> Boolean
;; If quick?, we already know new-trace has no *new* labels.
(define (traces-same-structure? prev-trace new-trace [quick? #f])
  (define prev-db (trace-db prev-trace))
  (define new-db (trace-db new-trace))
  (and (= (hash-count (trace-db prev-trace))
          (hash-count (trace-db new-trace)))
       (or quick?
           (for/and ([label (in-hash-keys prev-db)])
             (hash-has-key? new-db label)))))

;; DB = (Hashof Label Entry)
;; DeltaDB = (Hashof Label (U Entry Proposal))

;; Entry = (entry Dist[X] X Real)
;; - lprior is redundant, always (dist-pdf dist value #t)
(struct entry (dist value lprior) #:prefab)

;; hash-random-key : Hash[K => V] (K -> Boolean) -> K or #f
(define (hash-random-key h [ok-key? #f])
  (define n (hash-count* h ok-key?))
  (and (> n 0) (hash-nth-key h (random n) ok-key?)))

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

(define (wrap-ok-label? ok-label?)
  (and ok-label? (lambda (key) (ok-label? (label-view key)))))

;; ============================================================
;; Transition interface

(define (mcmc-transition? v)
  (is-a? v mcmc-transition<%>))

(define mcmc-transition<%>
  (interface ()
    ;; type TxInfo
    run  ;; (Model A) Trace -> (values (U Trace #f) TxInfo)
    ))


;; ============================================================
;; Proposals

(define (proposal? v)
  (is-a? v proposal<%>))

(define proposal<%>
  (interface ()
    propose1    ;; Label Dist[X] X -> (U #f (cons X Real))
    ;; Used for single-site proposal, or when adjusted variables are
    ;; known to be independent (dist parameters will not change from
    ;; previous values).

    propose2    ;; Label Dist[X] Dist[X] X -> (U #f (cons X Real))
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
    (init-field propose1-proc   ;; Label Dist[X] X -> (U #f (cons X Real) Proposal)
                propose2-proc   ;; Label Dist[X] Dist[X] X -> (U #f (cons X Real) Proposal)
                propose-dist)   ;; Label Dist[X] X -> (U #f Dist[X])
    (define/public (propose1 label dist prev-value)
      (define r (propose1* label dist prev-value))
      (if (proposal? r) (send r propose1 label dist prev-value) r))
    (define/private (propose1* label dist prev-value)
      (or (and propose1-proc (propose1-proc label dist prev-value))
          (propose2* label dist dist prev-value)))
    (define/public (propose2 label new-dist prev-dist prev-value)
      (define r (propose2* label new-dist prev-dist prev-value))
      (if (proposal? r) (send r propose2 label new-dist prev-dist prev-value) r))
    (define/private (propose2* label new-dist prev-dist prev-value)
      (or (and propose2-proc (propose2-proc label new-dist prev-dist prev-value))
          (and propose-dist
               (cond [(propose-dist label new-dist prev-value)
                      => (lambda (fd)
                           (define new-value (dist-sample fd))
                           (cond [(propose-dist label prev-dist new-value)
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
    (define/public (propose1 label dist value)
      (propose1:resample dist value))
    (define/public (propose2 label new-dist prev-dist prev-value)
      (propose2:resample new-dist prev-dist prev-value))
    ))

(define drift-proposal%
  (class* object% (proposal<%>)
    (init-field params?     ;; Boolean
                scale)      ;; PosReal or (Label Dist -> PosReal)
    (super-new)
    (define/public (propose1 label dist value)
      (dist-drift1 dist value params? (get-scale label dist)))
    (define/public (propose2 label new-dist old-dist old-value)
      (dist-drift2 new-dist old-dist old-value params? (get-scale label new-dist)))
    (define/private (get-scale label dist)
      (if (real? scale) scale (scale label dist)))
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
  (class base-stochastic-ctx%
    (inherit fail)
    (inherit-field escape-prompt)
    (init-field prev-db       ;; DB, not mutated
                delta-db      ;; DB, not mutated
                [l-R/F 0.0]   ;; real, mutated
                [sumlprs 0.0] ;; real, mutated; sum of lprior of all entries in current-db
                [sumlobs 0.0] ;; real, mutated; sum of log likelihoods of all observations
                [disallow-new/who #f]) ;; #f or Symbol
    (field [current-db (make-hash)] ;; DB, mutated
           [diff-lprs  0.0])        ;; see get-diff-lprs below

    (super-new)

    ;; The sample method records random choices by mutating
    ;; current-db. At the end of execution, current-db contains a
    ;; complete record of all random choices made by the program;
    ;; if accepted, it typically becomes a new execution's prev-db.

    (define/override (sample dist label)
      (if label
          (super sample dist label)
          (with-get-ADDR addr (super sample dist (and addr (auto-label addr))))))

    (define/override (-sample dist label)
      (unless label
        (error 'sample "missing label, required for MCMC sampler~a\n  dist: ~e"
               (if (context-has-ADDR? escape-prompt)
                   ";\n auto-label management failed because of uninstrumented code"
                   "")
               dist))
      (when (hash-ref current-db label #f)
        (error 'sample "duplicate label\n  label: ~e" label))
      (define delta-e (hash-ref delta-db label #f))
      (define prev-e (hash-ref prev-db label #f))
      (cond [delta-e (sample/delta dist label delta-e prev-e)]
            [prev-e (sample/prev dist label prev-e)]
            [else (sample/new dist label #f)]))

    (define/private (sample/delta dist label delta-e prev-e)
      (unless prev-e (error 'sample "internal error: in delta, not in previous"))
      (cond [(entry? delta-e)
             (log-mcmc-info "DELTA ~s: ~e, ~e => ~e, ~e" label
                            (entry-dist prev-e) (entry-value prev-e)
                            (entry-dist delta-e) (entry-value delta-e))
             (unless (equal? (entry-dist delta-e) dist)
               (error 'sample "internal error: delta has wrong dist"))
             (db-add! label delta-e prev-e)
             (entry-value delta-e)]
            [(proposal? proposal)
             (match-define (entry prev-dist prev-value _) prev-e)
             (match-define (cons new-value proposal-l-R/F)
               (or (send proposal propose2 label dist prev-dist prev-value)
                   (begin (log-mcmc-info "Late proposal returned #f; resampling")
                          (propose2:resample dist prev-dist prev-value))))
             (log-mcmc-info "DELTA ~s: ~e, ~e => ~e, ~e; R/F=~s" label
                            prev-dist prev-value dist new-value (exp l-R/F))
             (define new-lpr (dist-pdf dist new-value #t))
             (db-add! label (entry dist new-value new-lpr) prev-e)
             (set! l-R/F (+ l-R/F proposal-l-R/F))
             new-value]))

    (define/private (sample/prev dist label prev-e)
      (cond [(equal? (entry-dist prev-e) dist)
             (log-mcmc-info "REUSE ~s: ~e, ~e" label dist (entry-value prev-e))
             (db-add! label prev-e)
             (entry-value prev-e)]
            [(eq? (dist-type (entry-dist prev-e)) (dist-type dist))
             (define new-lpr (dist-pdf dist (entry-value prev-e) #t))
             (cond [(logspace-nonzero? new-lpr)
                    (define value (entry-value prev-e))
                    (define new-e (entry dist value new-lpr))
                    (log-mcmc-info "RESCORE ~s: ~e, ~e" label dist value)
                    (db-add! label new-e prev-e)
                    value]
                   [else (fail 'sample-rescore)])]
            [else (sample/new dist label prev-e)]))

    (define/private (sample/new dist label prev-e)
      (when disallow-new/who
        (error disallow-new/who
               "structural change (sampling new variable) not allowed"))
      (define value (dist-sample dist))
      (define lpr (dist-pdf dist value #t))
      (if prev-e
          (log-mcmc-info "MISMATCH ~s: ~e, ~e => ~e, ~e" label
                         (entry-dist prev-e) (entry-value prev-e)
                         dist value)
          (log-mcmc-info "NEW ~s: ~e, ~e" label dist value))
      (db-add! label (entry dist value lpr) prev-e)
      value)

    (define/override (-dscore who dn)
      (set! sumlobs (+ sumlobs (density->real dn #t)))
      (when (logspace-zero? sumlobs) (fail who)))

    (define/override (mem f)
      (with-get-ADDR addr
        (define (af . args)
          (with-put-ADDR (addr-add-mem addr args)
            (apply f args)))
        (super mem (procedure-reduce-arity af (procedure-arity f) 'memoized-function))))

    (define/override (run-model m addr)
      (if addr
          (super run-model m addr)
          (with-get-ADDR addr (super run-model m addr))))

    (define/override (run-top top)
      (match top
        [(? model? m)
         (super run-top (lambda () (run-model m (current-init-addr))))]
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

    ;; db-add! : Label Entry (U #f Entry) -> Void
    ;; Add entry to current-db and update sumlprs, sumlobs.
    ;; When prev-e is not #f, also update diff-lprs.
    (define/private (db-add! context e [prev-e #f])
      (hash-set! current-db context e)
      (define lpr (entry-lprior e))
      (set! sumlprs (+ sumlprs lpr))
      (when prev-e
        (define prev-lpr (entry-lprior prev-e))
        (set! diff-lprs (+ diff-lprs (- lpr prev-lpr)))))
    ))

(define initializing-tracing-stochastic-ctx%
  (class tracing-stochastic-ctx%
    (init-field get-value)  ;; (Label Dist[X] -> (U #f (list X)))
    (inherit-field prev-db)
    (super-new [prev-db (make-hash)] ;; mutated
               [delta-db (hash)])

    ;; Hack: override -sample to add entries to prev-db on demand.
    (define/override (-sample dist label)
      (match (and label (get-value (label-view label) dist))
        [(list value)
         (define lpr (dist-pdf dist value #t))
         (hash-set! prev-db label (entry dist value lpr))]
        [_ (void)])
      (super -sample dist label))
    ))

;; ============================================================
;; Runner

(define (make-eval-slice who m prev-db labels)
  (define base-ctx (new tracing-stochastic-ctx%
                        (prev-db prev-db)
                        (delta-db (hash))
                        (disallow-new/who who)))
  (define interp (new interpreter% (ctx base-ctx)))
  (define base-value (send interp eval-top m))
  (define base-trace (send base-ctx make-trace base-value))
  #;(send interp show)
  #;(pretty-print (send interp get-slice-expr labels))
  (define-values (re slice-lprs slice-lobs)
    (send interp get-slice-eval #:labels labels))
  (define rest-lprs (- (trace-lprs base-trace) slice-lprs))
  (define rest-lobs (- (trace-lobs base-trace) slice-lobs))
  (define base-db (trace-db base-trace))
  (define (eval-slice delta-db)
    (define slice-ctx
      (new tracing-stochastic-ctx%
           (prev-db base-db)
           (delta-db delta-db)
           (sumlprs rest-lprs)
           (sumlobs rest-lobs)
           (disallow-new/who who)))
    (match (send slice-ctx run-top (lambda () (re slice-ctx)))
      [(list result store-update)
       (define new-trace (send slice-ctx make-trace result))
       new-trace]
      [#f #f]))
  eval-slice)

(define (complete-slice-trace! slice-trace prev-db)
  (define slice-db (trace-db slice-trace))
  (for ([(label entry) (in-hash prev-db)])
    (unless (hash-has-key? slice-db label)
      (hash-set! slice-db label entry))))
