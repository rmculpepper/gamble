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
      (define-values (delta-db delta-l-R/F) (delta prev-trace))
      (define ctx
        (new tracing-stochastic-ctx%
             (prev-db prev-db)
             (delta-db delta-db)
             (l-R/F delta-l-R/F)))
      (match (send ctx run-top mdl)
        [(list new-value)
         (define new-trace (send ctx make-trace new-value))
         (define diff-lprs (send ctx get-diff-lprs))
         (define l-R/F (send ctx get-l-R/F))
         (define threshold (accept-threshold prev-trace l-R/F new-trace diff-lprs))
         (values threshold new-trace (vector 'delta delta-db))]
        [#f (values -inf.0 #f (vector 'delta delta-db))]))

    ;; delta : Trace -> (values DeltaDB Real)
    (abstract delta)

    ;; accept-threshold : Trace Real Trace Real -> Real
    ;; Computes (log) accept threshold for current trace.
    (define/public (accept-threshold prev-trace l-R/F new-trace diff-lprs)
      (define other-factor (accept-threshold* prev-trace new-trace))
      (cond [(or (= other-factor -inf.0) (= other-factor +inf.0))
             other-factor]
            [else
             (define diff-lobs (traces-obs-diff new-trace prev-trace))
             (+ l-R/F (/ (+ diff-lprs diff-lobs) temperature) other-factor)]))

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
    (init-field ok-tag?       ;; (Tag -> Boolean) or #f
                proposal)     ;; Proposal
    (super-new)

    ;; delta : Trace -> (values DeltaDB Real)
    (define/override (delta prev-trace)
      (define prev-db (trace-db prev-trace))
      (define key (db-random-key (trace-db prev-trace) ok-tag?))
      (cond [key
             (log-mcmc-info "Key to change = ~s" key)
             (define-values (new-e l-R/F)
               (delta-key key (hash-ref prev-db key)))
             (values (hash key new-e) l-R/F)]
            [else
             ;; Allow empty delta if no known variables; eg, for initial trace.
             (unless (zero? (hash-count prev-db))
               (error 'single-site-transition "no suitable key to change"))
             (values (hash) 0.0)]))

    ;; delta-key : DBKey Entry -> (values Entry Real)
    (define/public (delta-key key prev-e)
      (match-define (entry dist prev-value prev-lpr tag) prev-e)
      (match-define (cons new-value l-R/F)
        (or (send proposal propose1 tag dist prev-value)
            (begin (log-mcmc-info "Proposal returned #f; resampling")
                   (propose1:resample dist prev-value))))
      (log-mcmc-info "PROPOSED ~s: ~e, ~e => ~e; R/F=~s" key dist
                     prev-value new-value (exp l-R/F))
      (define new-lpr (dist-pdf dist new-value #t))
      (when (logspace-zero? new-lpr)
        (log-mcmc-info "proposed impossible value: ~e, ~e" dist new-value))
      (values (entry dist new-value new-lpr tag) l-R/F))

    (define/override (accept-threshold* prev-trace new-trace)
      ;; Account for backward and forward likelihood of picking
      ;; the random choice to perturb that we picked.
      (define new-nchoices (db-count* (trace-db new-trace) ok-tag?))
      (define prev-nchoices (db-count* (trace-db prev-trace) ok-tag?))
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
    (init-field ok-tag?       ;; (Tag -> Boolean) or #f
                proposal)     ;; Proposal
    (super-new)

    ;; delta : Trace -> (values DeltaDB Real)
    (define/override (delta prev-trace)
      (define prev-db (trace-db prev-trace))
      (define delta-db
        (for/hash ([(key e) (in-hash prev-db)] #:when (ok-tag? (db-entry-tag key e)))
          (values key proposal)))
      (when (zero? (hash-count delta-db))
        (unless (zero? (hash-count prev-db))
          (error 'multi-site-transition "no suitable keys to change")))
      (values delta-db 0.0))

    ;; accept-threshold* : Trace Trace -> Real
    (define/override (accept-threshold* prev-trace new-trace)
      (if (zero? (hash-count (trace-db prev-trace))) +inf.0 0.0))
    ))

;; ============================================================

(define enumerative-gibbs-transition%
  (class* object% (mcmc-transition<%>)
    (init-field ok-tag?)      ;; (Tag -> Boolean) or #f
    (super-new)

    ;; run : (Model A) Trace -> (values (U Trace #f) TxInfo)
    (define/public (run mdl prev-trace)
      (run/slice mdl prev-trace))

    ;; run/slice : (Model A) Trace -> (values (U Trace #f) TxInfo)
    (define/public (run/slice mdl prev-trace)
      (define who 'enumerative-gibbs-transition)
      (define prev-db (trace-db prev-trace))
      (define key (db-random-key prev-db ok-tag?))
      (unless key (error who "no suitable key to change"))
      (log-mcmc-info "Key to change = ~s" key)
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
      (unless (finite-dist? dist)
        (error who "distribution is not finite\n  key: ~e\n  dist: ~e" key dist))
      (define (make-entry new-value)
        (entry dist new-value (dist-pdf dist new-value #t tag)))
      (define eval-slice (make-eval-slice who mdl prev-db (list key)))
      (define conditional-dist
        (log-hash->normalized-discrete-dist
         (for/fold ([lh (hash)]) ([new-value (in-dist-values dist)])
           (define new-trace (eval-slice new-value))
           (if new-trace (hash-set lh new-trace (trace-lj new-trace)) lh))))
      (define new-trace (dist-sample conditional-dist))
      (complete-slice-trace! new-trace prev-db)
      (values new-trace (vector who key)))

    ;; run/full : (Model A) Trace -> (values (U Trace #f) TxInfo)
    (define/public (run/full mdl prev-trace)
      (define who 'enumerative-gibbs-transition)
      (define prev-db (trace-db prev-trace))
      (define key (db-random-key prev-db ok-tag?))
      (unless key (error who "no suitable key to change"))
      (log-mcmc-info "Key to change = ~s" key)
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
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
  (class* object% (mcmc-transition<%>)
    (init-field ok-tag?
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
      (define key (db-random-key prev-db ok-tag?))
      (unless key (error who "no suitable key to change"))
      (match-define (entry dist prev-value _ tag) (hash-ref prev-db key))
      (unless (numeric-dist? dist)
        (error who "distribution does not support slice sampling\n  dist: ~e" dist))
      (log-mcmc-info "Key to change = ~s, ~e" key prev-value)
      (define prev-lj (trace-lj prev-trace))
      (define lthreshold (+ (log (random)) prev-lj))
      (log-mcmc-info "Slice threshold = ~s (logspace ~s)" (exp lthreshold) lthreshold)
      (define eval-trace (make-caching-eval-trace who mdl prev-trace key))
      (define (eval-lj new-value) (cond [(eval-trace new-value) => trace-lj] [else -inf.0]))
      ;; --------------------
      (define-values (lo hi) (get-slice-bounds lthreshold dist prev-value eval-lj))
      (define new-trace (select dist prev-value eval-trace eval-lj lo hi lthreshold))
      (complete-slice-trace! new-trace prev-db)
      (values new-trace (vector who key)))

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
