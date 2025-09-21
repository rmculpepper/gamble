;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "base.rkt"
         "../dist.rkt"
         "../addr.rkt"
         "../util/real.rkt")
(provide (all-defined-out))

;; ============================================================

(define initialize-transition%
  (class* object% (mcmc-transition<%>)
    (init-field get-value)  ;; (Tag Dist -> (or/c (list X) #f))
    (super-new)

    ;; run : (Model A) #f -> (values Trace/#f TxInfo)
    (define/public (run mdl prev-trace)
      (define ctx
        (new initializing-tracing-stochastic-ctx%
             (get-value get-value)))
      (match (send ctx run-top mdl)
        [(list new-value)
         (define new-trace (send ctx make-trace new-value))
         (values new-trace 'initialize-transition)]
        [#f (values #f 'initialize-transition)]))
    ))

;; ============================================================

(define single-site-transition%
  (class* object% (mcmc-transition<%>)
    (init-field ok-tag?             ;; (Tag -> Boolean) or #f
                transition          ;; Transition/SingleSite
                [tempfactor 1.0])   ;; PositiveReal, inverse of temperature (mh)
    (super-new)

    ;; run : (Model A) Trace -> (values Trace/#f TxInfo)
    (define/public (run mdl prev-trace)
      (define prev-db (trace-db prev-trace))
      (define key (db-random-key (trace-db prev-trace) ok-tag?))
      (cond [key
             (define prev-e (hash-ref prev-db key))
             (log-mcmc-info "Key to change = ~.s; tag ~e; value ~e"
                            key (entry-tag prev-e) (entry-value prev-e))
             (run* mdl prev-trace key prev-e)]
            [else (error 'single-site-transition "no suitable key to change")]))

    ;; run* : ... -> (values Trace/#f TxInfo)
    (define/private (run* mdl prev-trace key prev-e)
      (match-define (entry dist prev-value prev-lpr tag) prev-e)
      (let loop ([transition transition])
        (match transition
          [#f
           (log-mcmc-info "No proposal (#f); resampling")
           (define-values (new-value l-R/F) (propose/resample dist prev-value))
           (mh mdl prev-trace key prev-e new-value l-R/F)]
          [(proposal-value new-value l-R/F)
           (mh mdl prev-trace key prev-e new-value l-R/F)]
          [(proposal-kernel kernel)
           (define-values (new-value l-R/F) (propose/kernel kernel prev-value))
           (mh mdl prev-trace key prev-e new-value l-R/F)]
          [(? procedure? get-transition)
           (loop (get-transition tag dist prev-value))]
          [_ (send transition run/key mdl prev-trace key prev-e)])))

    ;; ----------------------------------------
    ;; Metropolis-Hastings

    ;; mh : ... -> (values Trace/#f TxInfo)
    (define/private (mh mdl prev-trace key prev-e new-value l-R/F)
      (match-define (entry dist prev-value prev-lpr tag) prev-e)
      (log-mcmc-info "MH PROPOSED ~.s: ~e, ~e => ~e; log(R/F)=~s" key dist
                     prev-value new-value l-R/F)
      (define new-lpr (dist-pdf dist new-value #t))
      (when (logspace-zero? new-lpr)
        (log-mcmc-info "proposed impossible value: ~e, ~e" dist new-value))
      (define ctx
        (new tracing-stochastic-ctx%
             (prev-db (trace-db prev-trace))
             (delta-db (hash key (entry dist new-value new-lpr tag)))
             (l-R/F l-R/F)))
      (define new-txinfo (vector 'mh key tag))
      (match (send ctx run-top mdl)
        [(list new-result)
         (define new-trace (send ctx make-trace new-result))
         (define l-R/F (send ctx get-l-R/F))
         (define diff-lprs (send ctx get-diff-lprs))
         (define diff-lobs (traces-obs-diff new-trace prev-trace))
         (define diff-nkeys (nkeys-factor new-trace prev-trace))
         (define laccept (+ l-R/F diff-nkeys (* tempfactor (+ diff-lprs diff-lobs))))
         (define u (log (random)))
         (cond [(< u laccept)
                (log-mcmc-info "MH ACCEPT with threshold ~s" (exp laccept))
                (values new-trace new-txinfo)]
               [else
                (log-mcmc-info "MH REJECT with threshold ~s" (exp laccept))
                (values #f new-txinfo)])]
        [#f
         (log-mcmc-info "MH FAIL")
         (values #f new-txinfo)]))

    ;; nkeys-factor : Trace Trace -> Real
    ;; Account for backward and forward likelihood of selecting key.
    (define/private (nkeys-factor new-trace prev-trace)
      (define new-nkeys (db-count* (trace-db new-trace) ok-tag?))
      (define prev-nkeys (db-count* (trace-db prev-trace) ok-tag?))
      (cond [(zero? prev-nkeys)
             +inf.0]
            [else
             ;; Note: assumes we pick uniformly from all choices.
             ;; R = (log (/ 1 new-nkeys))    = (- (log new-nkeys))
             ;; F = (log (/ 1 prev-nkeys))   = (- (log prev-nkeys))
             ;; convert to inexact so (log 0.0) = -inf.0
             (define lR (- (log (fl new-nkeys))))
             (define lF (- (log (fl prev-nkeys))))
             (- lR lF)]))
    ))

;; ============================================================

#;
(define multi-site-transition%
  (class delta-mh-transition-base%
    (init-field ok-tag?         ;; (Tag -> Boolean) or #f
                make-proposal)  ;; (Tag Dist[X] X -> ProposalKernel[X])
    (super-new)

    ;; delta : Trace -> (values DeltaDB Real)
    (define/override (delta prev-trace)
      (define prev-db (trace-db prev-trace))
      (define delta-db
        (for/hash ([(key e) (in-hash prev-db)] #:when (ok-tag? (db-entry-tag key e)))
          (match-define (entry dist value _ tag) e)
          (values key (make-proposal tag dist value))))
      (when (zero? (hash-count delta-db))
        (unless (zero? (hash-count prev-db))
          (error 'multi-site-transition "no suitable keys to change")))
      (values delta-db 0.0))
    ))

;; ============================================================

(define enumerative-gibbs-transition%
  (class* object% (mcmc-transition/single-site<%>)
    (super-new)

    ;; run/key : (Model A) Trace DBKey Entry Real -> (values Trace/#f TxInfo)
    (define/public (run/key mdl prev-trace key prev-e)
      (run/slice mdl prev-trace key prev-e))

    ;; run/slice : (Model A) Trace DBKey Entry Real -> (values Trace/#f TxInfo)
    (define/public (run/slice mdl prev-trace key prev-e)
      (define who 'enumerative-gibbs-transition)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) prev-e)
      (unless (finite-dist? dist)
        (error who "distribution is not finite\n  key: ~e\n  dist: ~e" key dist))
      (define (make-entry new-value)
        (entry dist new-value (dist-pdf dist new-value #t) tag))
      (define eval-slice (make-eval-slice who mdl prev-db (list key)))
      (define conditional-dist
        (log-hash->normalized-discrete-dist
         (for/fold ([lh (hash)]) ([new-value (in-dist-values dist)])
           (define new-trace (eval-slice new-value))
           (if new-trace (hash-set lh new-trace (trace-lj new-trace)) lh))))
      (define new-trace (dist-sample conditional-dist))
      (complete-slice-trace! new-trace prev-db)
      (values new-trace (vector who key tag)))

    ;; run/full : (Model A) Trace -> (values (U Trace #f) TxInfo)
    (define/public (run/full mdl prev-trace key prev-e)
      (define who 'enumerative-gibbs-transition)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) prev-e)
      (unless (finite-dist? dist)
        (error who "distribution is not finite\n  key: ~e\n  dist: ~e" key dist))
      (define (make-entry new-value)
        (entry dist new-value (dist-pdf dist new-value #t tag)))
      (define conditional-dist
        (log-hash->normalized-discrete-dist
         (for/fold ([lh (hash)]) ([new-value (in-dist-values dist)])
           (cond [(equal? new-value prev-value)
                  (hash-set lh prev-trace (trace-lj prev-trace))]
                 [else
                  (define new-entry (make-entry new-value))
                  (define delta-db (hash key new-entry))
                  (define ctx (new tracing-stochastic-ctx%
                                   (prev-db prev-db)
                                   (delta-db delta-db)
                                   (disallow-new/who who)))
                  (match (send ctx run-top mdl)
                    [(list new-result)
                     (define new-trace (send ctx make-trace new-result))
                     (unless (traces-same-structure? prev-trace new-trace #t)
                       (error who "structural change not allowed"))
                     (hash-set lh new-trace (trace-lj new-trace))]
                    [#f lh])]))))
      (define new-trace (dist-sample conditional-dist))
      (values new-trace (vector who)))
    ))

;; ============================================================
;; Slice sampling
;; https://www.cs.toronto.edu/pub/radford/slice-aos.pdf

(define slice-transition%
  (class* object% (mcmc-transition/single-site<%>)
    (init-field [method 'double] ;; (U 'step 'double)
                [Wi 1]           ;; slice search width for integer dists
                [Wr 1.0]         ;; slice search width for real dists
                [M +inf.0]       ;; max # of widths to grow slice by
                [small-dist 10]) ;; limit of small-dist optimization, 0 to disable
    (super-new)

    ;; run/key : (Model A) Trace DBKey Entry -> (values (U Trace #f) TxInfo)
    (define/public (run/key mdl prev-trace key prev-e)
      (define who 'slice-transition)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) prev-e)
      (unless (numeric-dist? dist)
        (error who "distribution does not support slice sampling\n  dist: ~e" dist))
      (define prev-lj (trace-lj prev-trace))
      (define lthreshold (+ (log (random)) prev-lj))
      (log-mcmc-info "Slice threshold = ~s (logspace ~s)" (exp lthreshold) lthreshold)
      (define eval-trace (make-caching-eval-trace who mdl prev-trace key))
      (define (eval-lj new-value) (cond [(eval-trace new-value) => trace-lj] [else -inf.0]))
      ;; --------------------
      (define-values (lo hi) (get-slice-bounds lthreshold dist prev-value eval-lj))
      (define new-trace (select dist prev-value eval-trace eval-lj lo hi lthreshold))
      (complete-slice-trace! new-trace prev-db)
      (values new-trace (vector who key tag)))

    (define/private (make-caching-eval-trace who mdl prev-trace key)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
      (define trace-cache (make-hash)) ;; Hash[Real => Trace/#f]
      (hash-set! trace-cache prev-value prev-trace)
      (define base-eval-trace (make-eval-trace who mdl prev-trace key))
      (define (caching-eval-trace new-value)
        (hash-ref! trace-cache new-value (lambda () (base-eval-trace new-value))))
      caching-eval-trace)

    (define/private (make-eval-trace who mdl prev-trace key)
      (if #t
          (make-eval-trace/slice who mdl prev-trace key)
          (make-eval-trace/full who mdl prev-trace key)))

    (define/private (make-eval-trace/slice who mdl prev-trace key)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
      (define eval-slice (make-eval-slice who mdl prev-db (list key)))
      (define (eval-trace/slice new-value)
        (log-mcmc-info "Eval at ~e" new-value)
        (define new-lpr (dist-pdf dist new-value #t))
        (define new-trace (eval-slice (hash key (entry dist new-value new-lpr tag))))
        (log-mcmc-info "Eval lj ~e" (and new-trace (trace-lj new-trace)))
        new-trace)
      eval-trace/slice)

    (define/private (make-eval-trace/full who mdl prev-trace key)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
      (define (eval-trace/full new-value)
        (define new-lpr (dist-pdf dist new-value #t))
        (cond [(not (logspace-zero? new-lpr))
               (define delta-db
                 (hash key (entry dist new-value new-lpr tag)))
               (define ctx
                 (new tracing-stochastic-ctx%
                      (prev-db prev-db)
                      (delta-db delta-db)
                      (disallow-new/who who)))
               (match (send ctx run-top mdl)
                 [(list sample-value)
                  (define new-trace (send ctx make-trace sample-value))
                  (unless (traces-same-structure? new-trace prev-trace)
                    (error who "structural change not allowed"))
                  new-trace]
                 [#f #f])]
              [else #f]))
      eval-trace/full)

    ;; ----------------------------------------
    ;; Find slice bounds

    (define/private (get-slice-bounds lthreshold dist init-value eval-lj)
      (cond [(small-dist? dist)
             (match (dist-support dist)
               [(integer-range lo hi) (values lo hi)])]
            [else
             (define-values (W u)
               (cond [(integer-dist? dist) (values Wi (random (add1 Wi)))]
                     [else (values Wr (* (random) Wr))]))
             (define lo (- init-value u))
             (define hi (+ lo W))
             (case method
               [(step)
                (define-values (lo-k hi-k) (random-split-M))
                (values (step-out lthreshold lo-k lo (- W) eval-lj)
                        (step-out lthreshold hi-k hi (+ W) eval-lj))]
               [(double)
                (double-out lthreshold lo hi eval-lj)])]))

    (define/private (random-split-M)
      (cond [(= M +inf.0) (values +inf.0 +inf.0)]
            [else (let ([k (random M)]) (- M 1 k))]))

    (define/private (step-out lthreshold k x delta eval-lj)
      (let loop ([k k] [x x] [x-lj (eval-lj x)])
        (cond [(or (zero? k) (<= x-lj lthreshold)) x]
              [else (let ([x* (+ x delta)]) (loop (sub1 k) x* (eval-lj x*)))])))

    (define/private (double-out lthreshold lo hi eval-lj)
      (let loop ([lo lo] [lo-lj (eval-lj lo)] [hi hi] [hi-lj (eval-lj hi)])
        (cond [(and (<= lo-lj lthreshold) (<= hi-lj lthreshold))
               (values lo hi)]
              [(zero? (random 2))
               (let ([lo* (- lo (- hi lo))])
                 (loop lo* (eval-lj lo*) hi hi-lj))]
              [else
               (let ([hi* (+ hi (- hi lo))])
                 (loop lo lo-lj hi* (eval-lj hi*)))])))

    ;; ----------------------------------------
    ;; Select value in slice

    ;; select : ... -> Trace
    (define/private (select dist init-value eval-trace eval-lj lo0 hi0 lthreshold)
      (let loop ([lo lo0] [hi hi0])
        (log-mcmc-info "Slice bounds = [~s,~s]" lo hi)
        (define new-value
          (if (integer-dist? dist)
              (+ lo (random (add1 (- hi lo))))
              (+ lo (* (random) (- hi lo)))))
        (define new-trace (eval-trace new-value))
        (cond [(and new-trace
                    (> (trace-lj new-trace) lthreshold)
                    (acceptable? lo0 hi0 lthreshold init-value new-value eval-lj dist))
               (log-mcmc-info "Selected ~s" new-value)
               new-trace]
              [(integer-dist? dist)
               (if (< new-value init-value)
                   (loop (add1 new-value) hi)
                   (loop lo (sub1 new-value)))]
              [else
               (if (< new-value init-value)
                   (loop new-value hi)
                   (loop lo new-value))])))

    (define/private (acceptable? lo hi lthreshold init-value new-value eval-lj dist)
      (cond [(small-dist? dist) #t]
            [(eq? method 'double)
             (define int? (integer-dist? dist))
             (acceptable?/double lo hi lthreshold init-value new-value eval-lj int?)]
            [else #t]))

    (define/private (acceptable?/double lo hi lthreshold init-value new-value eval-lj int?)
      (define Wlimit (* 1.1 (if int? Wi Wr))) ;; avoid rounding problems
      (define (get-mid lo hi)
        (if int? (round (/ (+ lo hi) 2)) (* 0.5 (+ lo hi))))
      (let loop ([lo lo] [hi hi])
        (or (< (- hi lo) Wlimit)
            (let ([mid (get-mid lo hi)])
              (define lo* (if (< new-value mid) lo mid))
              (define hi* (if (< new-value mid) mid hi))
              (if (and (or (and (<  init-value mid) (>= new-value mid))
                           (and (>= init-value mid) (<  new-value mid)))
                       (<= (eval-lj lo*) lthreshold)
                       (<= (eval-lj hi*) lthreshold))
                  #f ;; not acceptable
                  (loop lo* hi*))))))

    (define/private (small-dist? dist)
      (match (dist-support dist)
        [(integer-range lo hi) (< (- hi lo) small-dist)]
        [_ #f]))
    ))
