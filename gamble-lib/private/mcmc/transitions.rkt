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
    (init-field get-value)  ;; (Tag Dist ProposalValue/#f) -> ProposalValue/#f
    (super-new)

    ;; run : ModelRunner #f -> (values Trace/#f TxInfo)
    (define/public (run mrun prev-trace)
      (define ctx
        (new initializing-tracing-stochastic-ctx%
             (prev-db (trace-db prev-trace))
             (get-value get-value)))
      (cond [(send mrun eval/ctx ctx)
             => (lambda (new-trace)
                  (values new-trace 'initialize))]
            [else (values #f 'initialize)]))
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
                    (values new-trace (vector 'mh key tag #t))]
                   [else
                    (log-mcmc-info "MH REJECT with threshold ~s" (exp laccept))
                    (values #f (vector 'mh key tag #f))])]
            [else
             (log-mcmc-info "MH FAIL")
             (values #f (vector 'mh key tag #f))]))

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
                [W 1.0]          ;; slice search width
                [M +inf.0]       ;; max # of steps to grow slice by w/ step-out
                [SD 5.0])        ;; if prior support < SD wide, use prior support (0 disables)
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
              (vector 'gibbs key tag)))

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
      (define new-value (slice-sample dist prev-value eval-lj lthreshold))
      (define new-trace (eval-trace new-value #f))
      (values new-trace (vector 'slice key tag)))

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
    ;; Slice Sampling (for X in {Real, ExactInteger})

    ;; slice-sample : Dist[X] X (X -> Real) Real -> X
    (define/private (slice-sample dist prev-value eval-lj lthreshold)
      (define-values (lo hi used-doubling?)
        (get-slice-bounds dist prev-value eval-lj lthreshold))
      (define check-accept
        (and used-doubling?
             (make-check-accept dist prev-value eval-lj lthreshold lo hi)))
      (select dist prev-value eval-lj lo hi lthreshold check-accept))

    ;; ----------------------------------------
    ;; Find slice bounds

    ;; get-slice-bounds : Dist[X] X (X -> Real) Real -> (values X X Boolean)
    (define/private (get-slice-bounds dist prev-value eval-lj lthreshold)
      (cond [(dist-support-bounds dist SD)
             => (match-lambda [(cons lo hi) (values lo hi #f)])]
            [(integer-dist? dist)
             (define Wi (exact (ceiling W)))
             (define u (random (add1 Wi)))
             (get-slice-bounds* prev-value eval-lj lthreshold Wi u)]
            [else
             (define u (* (random) W))
             (get-slice-bounds* prev-value eval-lj lthreshold W u)]))

    ;; get-slice-bounds* : X (X -> Real) Real X X -> (values X X Boolean)
    (define/private (get-slice-bounds* prev-value eval-lj lthreshold W u)
      (define lo (- prev-value u))
      (define hi (+ lo W))
      (case method
        [(step)
         (define-values (lo-k hi-k) (random-split-M))
         (values (step-out eval-lj lthreshold lo-k lo (- W))
                 (step-out eval-lj lthreshold hi-k hi (+ W))
                 #f)]
        [(double)
         (double-out eval-lj lthreshold lo hi)]))

    (define/private (random-split-M)
      (cond [(= M +inf.0) (values +inf.0 +inf.0)]
            [else (let ([k (random (add1 M))]) (values k (- M k)))]))

    ;; step-out : (X -> Real) Real Nat X X -> X
    (define/private (step-out eval-lj lthreshold k x delta)
      (let loop ([k k] [x x] [x-lj (eval-lj x)])
        (cond [(or (zero? k) (<= x-lj lthreshold)) x]
              [else (let ([x* (+ x delta)]) (loop (sub1 k) x* (eval-lj x*)))])))

    ;; double-out : (X -> Real) Real X X -> (values X X #t)
    (define/private (double-out eval-lj lthreshold lo hi)
      (let loop ([lo lo] [lo-lj (eval-lj lo)] [hi hi] [hi-lj (eval-lj hi)])
        (cond [(and (<= lo-lj lthreshold) (<= hi-lj lthreshold))
               (values lo hi #t)]
              [(zero? (random 2))
               (let ([lo* (- lo (- hi lo))])
                 (loop lo* (eval-lj lo*) hi hi-lj))]
              [else
               (let ([hi* (+ hi (- hi lo))])
                 (loop lo lo-lj hi* (eval-lj hi*)))])))

    ;; ----------------------------------------
    ;; Select value in slice

    ;; select : Dist[X] X .... -> X
    (define/private (select dist init-value eval-lj lo0 hi0 lthreshold check-accept)
      (let loop ([lo lo0] [hi hi0])
        (log-mcmc-info "Slice bounds = [~s,~s]" lo hi)
        (define new-value
          (if (integer-dist? dist)
              (+ lo (random (add1 (- hi lo))))
              (+ lo (* (random) (- hi lo)))))
        (cond [(and (> (eval-lj new-value) lthreshold)
                    (if check-accept (check-accept new-value) #t))
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

    ;; make-check-accept : Dist[X] X (X -> Real) Real X X -> (X -> Boolean)
    ;; Checks whether the new value could have doubled out to the same interval.
    (define/private (make-check-accept dist init-value eval-lj lthreshold lo hi)
      (define int? (integer-dist? dist))
      (lambda (new-value)
        (define Wlimit (* 1.1 (if int? (exact (ceiling W)) W))) ;; avoid rounding problems
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
                    (loop lo* hi*)))))))
    ))

;; dist-support-bounds : NumericDist -> (cons Real Real) or #f
;; Returns bound on width of prior support if width < SD.
(define (dist-support-bounds dist SD)
  (match (dist-support dist)
    [(integer-range lo hi) (and (< (- hi lo) SD) (cons lo hi))]
    [(real-range lo hi) (and (< (- hi lo) SD) (cons lo hi))]
    [_ #f]))
