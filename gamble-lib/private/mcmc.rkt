;; Copyright (c) 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "base.rkt"
         "mcmc/base.rkt"
         "mcmc/transitions.rkt"
         "util/real.rkt")
(provide (all-from-out "mcmc/base.rkt")
         (all-defined-out))

;; ============================================================
;; Transitions

(define (initialize-transition [get-value (lambda (tag dist) #f)])
  (new initialize-transition% (get-value get-value)))

(define (single-site-transition [transition #f]
                                #:any [ok-tag? #f])
  (new single-site-transition% (ok-tag? ok-tag?) (transition transition)))

(define (slice-transition #:method [method 'double]
                          #:W [Wr 1.0]
                          #:Wi [Wi (exact (ceiling Wr))]
                          #:M [M +inf.0]
                          #:small-dist-limit [small-dist 10])
  (new slice-transition% (method method)
       (Wr Wr) (Wi Wi) (M M) (small-dist small-dist)))

;; ============================================================

(define mcmc%
  (class object%
    (init-field mdl
                [last-trace init-trace])
    (super-new)

    ;; step : Transition -> (values Boolean Trace TxInfo)
    (define/public (step transition)
      (log-mcmc-info "START transition ~e" transition)
      (define-values (new-trace new-txinfo)
        (send transition run mdl last-trace))
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

    (define/public (initialize transition)
      (when (eq? last-trace init-trace)
        (define-values (accepted? trace txinfo)
          (step transition))
        (if accepted? (void) (initialize transition))))
    ))

(define mcmc-sampler%
  (class sampler-base%
    (init-field mdl
                transition)
    (super-new)

    (define mcmc (new mcmc% (mdl mdl)))
    (define/public (get-mcmc) mcmc)

    (define/override (sample)
      (define-values (accepted? trace txinfo)
        (send mcmc step transition))
      (trace-value trace))

    (define/public (initialize transition)
      (send mcmc initialize transition))
    ))

(define (mcmc-sampler mdl
                      #:initialize [initialize (initialize-transition)]
                      #:transition [transition (single-site-transition)])
  (let ([transition
         (cond [(mcmc-transition? transition) transition]
               [else (single-site-transition transition)])])
    (define s (new mcmc-sampler% (mdl mdl) (transition transition)))
    (when initialize
      (send s initialize initialize))
    s))
