;; Copyright (c) 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "interfaces.rkt"
         "mcmc/base.rkt"
         "mcmc/transitions.rkt"
         "util/real.rkt")
(provide (all-from-out "mcmc/base.rkt")
         (all-defined-out))

;; ============================================================
;; Transitions

(define (initialize-transition [get-value (lambda (addr dist) #f)]
                               #:hash [value-hash #f])
  (define (get-value* addr dist)
    (cond [(hash-has-key? value-hash addr)
           (list (hash-ref value-hash addr))]
          [else (get-value addr dist)]))
  (new initialize-transition% (get-value get-value*)))

(define (single-site-transition #:proposal proposal
                                #:any [ok-addr? #f])
  (new single-site-transition% (ok-addr? ok-addr?) (proposal proposal)))

(define (multi-site-transition #:proposal proposal
                               #:all [ok-addr? #f])
  (new multi-site-transition% (ok-addr? ok-addr?) (proposal proposal)))

(define (enumerative-gibbs-transition #:any [ok-addr? #f])
  (new enumerative-gibbs-transition% (ok-addr? ok-addr?)))

(define (slice-transition #:method [method 'double]
                          #:W [Wr 1.0]
                          #:Wi [Wi (exact (ceiling Wr))]
                          #:M [M +inf.0]
                          #:small-dist-limit [small-dist 10]
                          #:any [ok-addr? #f])
  (new slice-transition% (ok-addr? ok-addr?) (method method)
       (Wr Wr) (Wi Wi) (M M) (small-dist small-dist)))

;; ============================================================

(define INIT-ITERS 10)

(define mcmc%
  (class object%
    (init-field thunk
                [last-trace init-trace])
    (super-new)

    ;; step : Transition -> (values Boolean Trace TxInfo)
    (define/public (step transition)
      (define-values (new-trace new-txinfo)
        (send transition run thunk last-trace))
      (cond [new-trace
             (set! last-trace new-trace)
             (values #t new-trace new-txinfo)]
            [else
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

    #|
    ;; Note: {MAP,MLE}-estimate is argmax over *all* unconditioned variables.
    ;; FIXME: figure out how to do subsets.
    (define/public (MAP-estimate iters)
      (*estimate iters trace-ll))
    (define/public (MLE-estimate iters)
      (*estimate iters trace-ll-obs))

    (define/private (*estimate iters get-trace-ll)
      (void (sample))
      (define best-trace
        (for/fold ([best-trace last-trace]) ([n (in-range iters)])
          (void (sample))
          (if (trace>? last-trace best-trace) last-trace best-trace)))
      (trace-value best-trace))
    |#
    ))

(define mcmc-sampler%
  (class sampler-base%
    (init-field thunk
                transition)
    (super-new)

    (define mcmc (new mcmc% (thunk thunk)))

    (define/override (sample)
      (define-values (accepted? trace txinfo)
        (send mcmc step transition))
      (trace-value trace))
    ))

(define (mcmc-sampler thunk
                      [transition (single-site-transition #:proposal (resample-proposal))])
  (new mcmc-sampler% (thunk thunk) (transition transition)))

;; ============================================================
