;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "base.rkt"
         "../dist.rkt"
         "../util/real.rkt"
         "../util/density.rkt")
(provide (all-defined-out))

;; ============================================================

(define mh-transition-base%
  (class* object% (mcmc-transition<%>)
    (super-new)

    ;; run : (Model A) Trace -> (values Trace/#f TxInfo)
    (define/public (run mdl prev-trace)
      (define-values (laccept new-trace new-txinfo)
        (run* mdl prev-trace))
      (define u (log (random)))
      (cond [(< u laccept)
             (log-mcmc-info "Accepted MH step with threshold ~s" (exp laccept))
             (values new-trace new-txinfo)]
            [else
             (log-mcmc-info "Rejected MH step with threshold ~s" (exp laccept))
             (values #f new-txinfo)]))

    ;; run* : (Model A) Trace -> (values Real Trace/#f TxInfo)
    (abstract run*)
    ))

;; ============================================================

(define initialize-transition%
  (class* object% (mcmc-transition<%>)
    (init-field get-value)  ;; (Addr Dist -> (or/c (list X) #f))
    (super-new)

    ;; run : (Model A) #f -> (values Trace/#f TxInfo)
    (define/public (run mdl prev-trace)
      (define ctx
        (new initializing-tracing-stochastic-ctx%
             (initializer get-value)))
      (match (send ctx run-top mdl)
        [(list new-value)
         (define new-trace (send ctx make-trace new-value))
         (values 0.0 new-trace 'initialize-transition)]
        [#f (values -inf.0 #f 'initialize-transition)]))
    ))

;; ============================================================

(define delta-mh-transition-base%
  (class mh-transition-base%
    (init-field [temperature 1.0])
    (super-new)

    ;; run* : (Model A) Trace -> (values Real Trace/#f TxInfo)
    (define/override (run* mdl prev-trace)
      (define prev-db (trace-db prev-trace))
      (define-values (delta-db delta-ll-R/F) (delta prev-trace))
      (define ctx
        (new tracing-stochastic-ctx%
             (prev-db prev-db)
             (delta-db delta-db)
             (ll-R/F delta-ll-R/F)))
      (match (send ctx run-top mdl)
        [(list new-value)
         (define new-trace (send ctx make-trace new-value))
         (define ll-diff (send ctx get-ll-diff))
         (define ll-R/F (send ctx get-ll-R/F))
         (define threshold (accept-threshold prev-trace ll-R/F new-trace ll-diff))
         (values threshold new-trace (vector 'delta delta-db))]
        [#f (values -inf.0 #f (vector 'delta delta-db))]))

    ;; delta : Trace -> (values DeltaDB Real)
    (abstract delta)

    ;; accept-threshold : Trace Real Trace Real -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/public (accept-threshold prev-trace ll-R/F new-trace ll-diff)
      (define other-factor (accept-threshold* prev-trace new-trace))
      (cond [(or (= other-factor -inf.0) (= other-factor +inf.0))
             other-factor]
            [else
             (define ll-diff-obs (traces-obs-diff new-trace prev-trace))
             (+ ll-R/F (/ (+ ll-diff ll-diff-obs) temperature) other-factor)]))

    ;; accept-threshold* : Trace Trace -> Real
    ;; Computes (log) of additional factors of accept threshold.
    ;; If +/-inf.0, then that is taken as accept factor (to avoid
    ;; possible NaN from arithmetic).
    (define/public (accept-threshold* prev-trace current-trace)
      0.0)
    ))

;; ============================================================

(define single-site-transition%
  (class delta-mh-transition-base%
    (init-field ok-addr?      ;; (Addr -> Boolean) or #f
                proposal)     ;; Proposal
    (super-new)

    ;; delta : Trace -> (values DeltaDB Real)
    (define/override (delta prev-trace)
      (define prev-db (trace-db prev-trace))
      (define addr (hash-random-key (trace-db prev-trace) ok-addr?))
      (cond [addr
             (log-mcmc-info "Addr to change = ~s" addr)
             (match (hash-ref prev-db addr)
               [(entry prev-dist prev-value prev-ll)
                (define-values (new-e ll-R/F)
                  (delta-addr addr prev-dist prev-value))
                (values (hash addr new-e) ll-R/F)])]
            [else
             ;; Allow empty delta if no known variables; eg, for initial trace.
             (unless (zero? (hash-count prev-db))
               (error 'single-site-transition "no suitable addr to change"))
             (values (hash) 0.0)]))

    ;; delta-addr : Address Dist Value -> (values Entry Real)
    (define/public (delta-addr addr dist prev-value)
      (match-define (cons new-value ll-R/F)
        (or (send proposal propose1 addr dist prev-value)
            (begin (log-mcmc-info "Proposal returned #f; resampling")
                   (propose1:resample dist prev-value))))
      (log-mcmc-info "PROPOSED ~s: ~e, ~e => ~e; R/F=~s" addr dist
                     prev-value new-value (exp ll-R/F))
      (define new-ll (dist-pdf dist new-value #t))
      (when (logspace-zero? new-ll)
        (log-mcmc-info "proposed impossible value: ~e, ~e" dist new-value))
      (values (entry dist new-value new-ll) ll-R/F))

    (define/override (accept-threshold* prev-trace new-trace)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      (define new-nchoices (hash-count* (trace-db new-trace) ok-addr?))
      (define prev-nchoices (hash-count* (trace-db prev-trace) ok-addr?))
      (cond [(zero? prev-nchoices)
             +inf.0]
            [else
             ;; Note: assumes we pick uniformly from all choices.
             ;; R = (log (/ 1 new-nchoices))    = (- (log new-nchoices))
             ;; F = (log (/ 1 prev-nchoices))   = (- (log prev-nchoices))
             ;; convert to inexact so (log 0.0) = -inf.0
             (define lR (- (log (fl new-nchoices))))
             (define lF (- (log (fl prev-nchoices))))
             (- lR lF)]))
    ))

(define multi-site-transition%
  (class delta-mh-transition-base%
    (init-field ok-addr?      ;; (Addr -> Boolean) or #f
                proposal)     ;; Proposal
    (super-new)

    ;; delta : Trace -> (values DeltaDB Real)
    (define/override (delta prev-trace)
      (define prev-db (trace-db prev-trace))
      (define delta-db
        (for/hash ([(addr e) (in-hash prev-db)] #:when (ok-addr? addr))
          (values addr proposal)))
      (when (zero? (hash-count delta-db))
        (unless (zero? (hash-count prev-db))
          (error 'multi-site-transition "no suitable addrs to change")))
      (values delta-db 0.0))

    ;; accept-threshold* : Trace Trace -> Real
    (define/override (accept-threshold* prev-trace new-trace)
      (if (zero? (hash-count (trace-db prev-trace))) +inf.0 0.0))
    ))

;; ============================================================

(define enumerative-gibbs-transition%
  (class* object% (mcmc-transition<%>)
    (init-field ok-addr?)     ;; (Addr -> Boolean) or #f
    (super-new)

    ;; run : (Model A) Trace -> (values (U Trace #f) TxInfo)
    (define/public (run mdl prev-trace)
      (define who 'enumerative-gibbs-transition)
      (define prev-db (trace-db prev-trace))
      (define addr (hash-random-key prev-db ok-addr?))
      (unless addr (error who "no suitable addr to change"))
      (log-mcmc-info "Addr to change = ~s" addr)
      (match-define (entry dist prev-value _) (hash-ref prev-db addr))
      (unless (finite-dist? dist)
        (error who "distribution is not finite\n  addr: ~e\n  dist: ~e" addr dist))
      (define (make-entry new-value)
        (entry dist new-value (dist-pdf dist new-value #t)))
      (define conditional-dist
        (log-hash->normalized-discrete-dist
         (for/fold ([lh (hash)]) ([new-value (in-dist-values dist)])
           (cond [(equal? new-value prev-value)
                  (hash-set lh prev-trace (trace-ll prev-trace))]
                 [else
                  (define new-entry (make-entry new-value))
                  (define delta-db (hash addr new-entry))
                  (define ctx (new tracing-stochastic-ctx%
                                   (prev-db prev-db)
                                   (delta-db delta-db)
                                   (disallow-new/who who)))
                  (match (send ctx run-top mdl)
                    [(list new-result)
                     (define new-trace (send ctx make-trace new-result))
                     (unless (traces-same-structure? prev-trace new-trace #t)
                       (error who "structural change not allowed"))
                     (hash-set lh new-trace (trace-ll new-trace))]
                    [#f lh])]))))
      (define new-trace (dist-sample conditional-dist))
      (values new-trace (vector who)))
    ))

;; ============================================================
;; Slice sampling
;; https://www.cs.toronto.edu/pub/radford/slice-aos.pdf

(define slice-transition%
  (class* object% (mcmc-transition<%>)
    (init-field ok-addr?
                [method 'double] ;; (U 'step 'double)
                [Wi 1]           ;; slice search width for integer dists
                [Wr 1.0]         ;; slice search width for real dists
                [M +inf.0]       ;; max # of widths to grow slice by
                [small-dist 10]) ;; limit of small-dist optimization, 0 to disable
    (super-new)

    ;; run : (Model A) Trace -> (cons (U Trace #f) TxInfo)
    (define/public (run mdl prev-trace)
      (define who 'slice-transition)
      (define prev-db (trace-db prev-trace))
      (define addr (hash-random-key prev-db ok-addr?))
      (unless addr (error who "no suitable addr to change"))
      (match-define (entry dist prev-value _) (hash-ref prev-db addr))
      (log-mcmc-info "Addr to change = ~s, ~e" addr prev-value)
      (unless (real-dist? dist)
        (error who "distribution does not support slice sampling\n  dist: ~e" dist))
      (define slice
        (new slice% (method method) (Wi Wi) (Wr Wr) (M M) (small-dist small-dist)
             (mdl mdl) (prev-trace prev-trace) (addr addr)))
      (values (send slice sample) (vector who addr)))
    ))

(define slice%
  (class object%
    (init-field method Wi Wr M small-dist mdl prev-trace addr)
    (super-new)

    (define prev-db (trace-db prev-trace))
    (define prev-ll (trace-ll prev-trace))
    (match-define (entry dist prev-value _) (hash-ref prev-db addr))

    ;; ----------------------------------------

    (define/public (sample)
      (define lthreshold (+ (log (random)) prev-ll))
      (log-mcmc-info "Slice threshold = ~s (logspace ~s)" (exp lthreshold) lthreshold)
      (define-values (lo hi) (get-slice-bounds lthreshold))
      (log-mcmc-info "Slice bounds = [~s,~s]" lo hi)
      (select lo hi lthreshold))

    ;; ----------------------------------------
    ;; Eval trace, ll

    (define trace-cache (make-hash)) ;; Hash[Real => Trace/#f]
    (hash-set! trace-cache prev-value prev-trace)

    (define/private (eval-ll new-value)
      (cond [(eval-trace new-value) => trace-ll]
            [else -inf.0]))

    (define/private (eval-trace new-value)
      (hash-ref! trace-cache new-value (lambda () (eval-trace* new-value))))

    (define/private (eval-trace* new-value)
      (define new-ll (dist-pdf dist new-value #t))
      (cond [(not (logspace-zero? new-ll))
             (define delta-db
               (hash addr (entry dist new-value new-ll)))
             (define ctx
               (new tracing-stochastic-ctx% 
                    (prev-db prev-db)
                    (delta-db delta-db)
                    (disallow-new/who 'slice)))
             (match (send ctx run-top mdl)
               [(list sample-value)
                (define new-trace (send ctx make-trace sample-value))
                (unless (traces-same-structure? new-trace prev-trace)
                  (error 'slice-transition "structural change not allowed"))
                new-trace]
               [#f #f])]
            [else #f]))

    ;; ----------------------------------------
    ;; Find slice bounds

    (define/private (get-slice-bounds lthreshold)
      (cond [(small-dist? dist)
             (match (dist-support dist)
               [(integer-range lo hi) (values lo hi)])]
            [else
             (define-values (W u)
               (cond [(integer-dist? dist) (values Wi (random (add1 Wi)))]
                     [else (values Wr (* (random) Wr))]))
             (define lo (- prev-value u))
             (define hi (+ lo W))
             (case method
               [(step)
                (define-values (lo-k hi-k) (random-split-M))
                (values (step-out lthreshold lo-k lo (- W))
                        (step-out lthreshold hi-k hi (+ W)))]
               [(double)
                (double-out lthreshold lo hi)])]))

    (define/private (random-split-M)
      (cond [(= M +inf.0) (values +inf.0 +inf.0)]
            [else (let ([k (random M)]) (- M 1 k))]))

    (define/private (step-out lthreshold k x delta)
      (let loop ([k k] [x x] [x-ll (eval-ll x)])
        (cond [(or (zero? k) (<= x-ll lthreshold)) x]
              [else (let ([x* (+ x delta)]) (loop (sub1 k) x* (eval-ll x*)))])))

    (define/private (double-out lthreshold lo hi)
      (let loop ([lo lo] [lo-ll (eval-ll lo)] [hi hi] [hi-ll (eval-ll hi)])
        (cond [(and (<= lo-ll lthreshold) (<= hi-ll lthreshold))
               (values lo hi)]
              [(zero? (random 2))
               (let ([lo* (- lo (- hi lo))])
                 (loop lo* (eval-ll lo*) hi hi-ll))]
              [else
               (let ([hi* (+ hi (- hi lo))])
                 (loop lo lo-ll hi* (eval-ll hi*)))])))

    ;; ----------------------------------------
    ;; Select value in slice

    ;; select : Real Real Real -> Trace
    (define/private (select lo0 hi0 lthreshold)
      (let loop ([lo lo0] [hi hi0])
        (define new-value
          (if (integer-dist? dist)
              (+ lo (random (add1 (- hi lo))))
              (+ lo (* (random) (- hi lo)))))
        (define new-trace (eval-trace new-value))
        (cond [(and new-trace
                    (> (trace-ll new-trace) lthreshold)
                    (acceptable? new-value lo0 hi0 lthreshold))
               new-trace]
              [(integer-dist? dist)
               (if (< new-value prev-value)
                   (loop (add1 new-value) hi)
                   (loop lo (sub1 new-value)))]
              [else
               (if (< new-value prev-value)
                   (loop new-value hi)
                   (loop lo new-value))])))

    (define/private (acceptable? new-value lo hi lthreshold)
      (cond [(small-dist? dist) #t]
            [(eq? method 'double)
             (acceptable?/double new-value lo hi lthreshold (integer-dist? dist))]
            [else #t]))

    (define/private (acceptable?/double new-value lo hi lthreshold int?)
      (define Wlimit (* 1.1 (if int? Wi Wr))) ;; avoid rounding problems
      (define (get-mid lo hi)
        (if int? (round (/ (+ lo hi) 2)) (* 0.5 (+ lo hi))))
      (let loop ([lo lo] [hi hi])
        (or (< (- hi lo) Wlimit)
            (let ([mid (get-mid lo hi)])
              (define lo* (if (< new-value mid) lo mid))
              (define hi* (if (< new-value mid) mid hi))
              (if (and (or (and (<  prev-value mid) (>= new-value mid))
                           (and (>= prev-value mid) (<  new-value mid)))
                       (<= (eval-ll lo*) lthreshold)
                       (<= (eval-ll hi*) lthreshold))
                  #f ;; not acceptable
                  (loop lo* hi*))))))

    (define/private (small-dist? dist)
      (match (dist-support dist)
        [(integer-range lo hi) (< (- hi lo) small-dist)]
        [_ #f]))
    ))
