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

    ;; run : ModelRunner #f -> (values Trace/#f TxInfo)
    (define/public (run mrun prev-trace)
      (define ctx
        (new initializing-tracing-stochastic-ctx%
             (get-value get-value)))
      (cond [(send mrun eval/ctx ctx)
             => (lambda (new-trace)
                  (values new-trace 'initialize-transition))]
            [else (values #f 'initialize-transition)]))
    ))

;; ============================================================

(define single-site-transition%
  (class* object% (mcmc-transition<%>)
    (init-field ok-tag?             ;; (Tag -> Boolean) or #f
                transition          ;; Transition/SingleSite
                [tempfactor 1.0])   ;; PositiveReal, inverse of temperature (mh)
    (super-new)

    ;; run : ModelRunner Trace -> (values Trace/#f TxInfo)
    (define/public (run mrun prev-trace)
      (define prev-db (trace-db prev-trace))
      (define key (db-random-key (trace-db prev-trace) ok-tag?))
      (cond [key
             (define prev-e (hash-ref prev-db key))
             (log-mcmc-info "Key to change = ~.s; tag ~e; value ~e"
                            key (entry-tag prev-e) (entry-value prev-e))
             (run* mrun prev-trace key prev-e)]
            [else (error 'single-site-transition "no suitable key to change")]))

    ;; run* : ... -> (values Trace/#f TxInfo)
    (define/private (run* mrun prev-trace key prev-e)
      (match-define (entry dist prev-value prev-lpr tag) prev-e)
      (let loop ([transition transition])
        (match transition
          [#f
           (log-mcmc-info "No proposal (#f); resampling")
           (define-values (new-value l-R/F) (propose/resample dist prev-value))
           (mh mrun prev-trace key prev-e new-value l-R/F)]
          [(proposal-value new-value l-R/F)
           (mh mrun prev-trace key prev-e new-value l-R/F)]
          [(proposal-kernel kernel)
           (define-values (new-value l-R/F) (propose/kernel kernel prev-value))
           (mh mrun prev-trace key prev-e new-value l-R/F)]
          [(? procedure? get-transition)
           (loop (get-transition tag dist prev-value))]
          [_ (send transition run/key mrun prev-trace key prev-e)])))

    ;; ----------------------------------------
    ;; Metropolis-Hastings

    ;; mh : ... -> (values Trace/#f TxInfo)
    (define/private (mh mrun prev-trace key prev-e new-value proposal-l-R/F)
      (match-define (entry dist prev-value prev-lpr tag) prev-e)
      (log-mcmc-info "MH PROPOSED ~.s: ~e, ~e => ~e; log(R/F)=~s" key dist
                     prev-value new-value proposal-l-R/F)
      (define new-lpr (dist-pdf dist new-value #t))
      (when (logspace-zero? new-lpr)
        (log-mcmc-info "proposed impossible value: ~e, ~e" dist new-value))
      (define delta-db (hash key (entry dist new-value new-lpr tag)))
      (define new-txinfo (vector 'mh key tag))
      (define-values (new-trace ctx) (send mrun eval/try-reuse delta-db prev-trace))
      (cond [new-trace
             (define l-R/F (+ proposal-l-R/F (send ctx get-l-R/F)))
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
            [else
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
;; Slice sampling
;; https://www.cs.toronto.edu/pub/radford/slice-aos.pdf

(define slice-transition%
  (class* object% (mcmc-transition/single-site<%>)
    (init-field [gibbs? #t]      ;; Boolean, do Gibbs if available
                [method 'double] ;; (U 'step 'double)
                [Wi 1]           ;; slice search width for integer dists
                [Wr 1.0]         ;; slice search width for real dists
                [M +inf.0]       ;; max # of widths to grow slice by
                [small-dist 10]) ;; limit of small-dist optimization, 0 to disable
    (super-new)

    ;; run/key : ModelRunner Trace DBKey Entry -> (values (U Trace #f) TxInfo)
    (define/public (run/key mrun prev-trace key prev-e)
      (define who 'slice-transition)
      (cond [(and gibbs? (send mrun get-slice-posterior who key prev-trace))
             => (lambda (pdist) (run/gibbs mrun prev-trace key prev-e pdist))]
            [else (run/slice mrun prev-trace key prev-e)]))

    (define/private (run/gibbs mrun prev-trace key prev-e pdist)
      (define who 'slice-transition)
      (match-define (entry dist prev-value _ tag) prev-e)
      (define new-value (dist-sample pdist))
      (log-mcmc-info "Gibbs dist = ~e" pdist)
      (define new-lpr (dist-pdf dist new-value #t))
      (define eval-slice (send mrun make-eval-slice who (list key) prev-trace))
      (define delta-db (hash key (entry dist new-value new-lpr tag)))
      (values (eval-slice delta-db #f)
              (vector who key (entry-tag prev-e) 'gibbs)))

    (define/private (run/slice mrun prev-trace key prev-e)
      (define who 'slice-transition)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) prev-e)
      (unless (numeric-dist? dist)
        (error who "distribution does not support slice sampling\n  dist: ~e" dist))
      (define prev-lj (trace-lj prev-trace))
      (define lthreshold (+ (log (random)) prev-lj))
      (log-mcmc-info "Slice threshold = ~s (logspace ~s)" (exp lthreshold) lthreshold)
      (define eval-trace (make-caching-eval-trace who mrun prev-trace key))
      (define (eval-lj new-value) (trace-lj (eval-trace new-value #t)))
      ;; --------------------
      (define-values (lo hi) (get-slice-bounds lthreshold dist prev-value eval-lj))
      (define new-value (select dist prev-value eval-lj lo hi lthreshold))
      (define new-trace (eval-trace new-value #f))
      (values new-trace (vector who key tag)))

    (define/private (make-caching-eval-trace who mrun prev-trace key)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
      (define trace-cache (make-hash)) ;; Hash[Real => Trace/#f]
      (hash-set! trace-cache prev-value prev-trace)
      (define eval-trace (make-eval-trace who mrun prev-trace key))
      (define (caching-eval-trace new-value mini?)
        (if mini?
            (hash-ref! trace-cache new-value (lambda () (eval-trace new-value #t)))
            (eval-trace new-value #f)))
      caching-eval-trace)

    (define/private (make-eval-trace who mrun prev-trace key)
      (define prev-db (trace-db prev-trace))
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
      (define eval-slice (send mrun make-eval-slice who (list key) prev-trace))
      (define (eval-trace new-value mini?)
        (log-mcmc-info "Eval at ~e" new-value)
        (define new-lpr (dist-pdf dist new-value #t))
        (define new-trace (eval-slice (hash key (entry dist new-value new-lpr tag)) mini?))
        (log-mcmc-info "Eval lj ~e" (trace-lj new-trace))
        new-trace)
      eval-trace)

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

    ;; select : Dist[X] X .... -> X
    (define/private (select dist init-value eval-lj lo0 hi0 lthreshold)
      (let loop ([lo lo0] [hi hi0])
        (log-mcmc-info "Slice bounds = [~s,~s]" lo hi)
        (define new-value
          (if (integer-dist? dist)
              (+ lo (random (add1 (- hi lo))))
              (+ lo (* (random) (- hi lo)))))
        (cond [(and (> (eval-lj new-value) lthreshold)
                    (acceptable? lo0 hi0 lthreshold init-value new-value eval-lj dist))
               (log-mcmc-info "Selected ~s" new-value)
               new-value]
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
