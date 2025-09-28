;; Copyright (c) 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         racket/flonum
         racket/vector
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
                          #:gibbs? [gibbs? #t]
                          #:W [W 1.0]
                          #:M [M +inf.0]
                          #:SD [SD 5.0])
  (new slice-transition% (method method) (gibbs? gibbs?) (W W) (M M) (SD SD)))

;; ============================================================

(define mcmc%
  (class object%
    (init-field mdl
                [retries 10]
                [last-trace init-trace])
    (super-new)

    (field [mrun (new model-runner% (mdl mdl))])

    (define/public (show)
      (send mrun show))

    ;; step : Transition -> (values Trace TxInfo)
    (define/public (step transition)
      (log-mcmc-info "START transition ~e" transition)
      (define-values (new-trace new-txinfo)
        (send transition run mrun last-trace))
      (cond [new-trace
             (set! last-trace new-trace)
             (values new-trace new-txinfo)]
            [else
             (values last-trace new-txinfo)]))

    ;; steps : Nat Transition (Listof Symbol) -> (Hasheq Symbol Any)
    (define/public (steps n transition fields #:lag [lag 0])
      ;; Vector-valued fields
      ;; - 'trace       => 'value 'log-joint 'log-prior 'log-score
      ;; - 'transition
      (define tracev (make-vector n))
      (define txinfov (and (memq 'transition fields) (make-vector n)))
      (for ([i (in-range n)])
        (for ([j (in-range lag)])
          (step transition))
        (define-values (trace txinfo) (step transition))
        (vector-set! tracev i trace)
        (when txinfov (vector-set! txinfov i txinfo)))
      (define (vector-fl-map f v) ;; (X -> Real) (Vectorof X) -> FlVector
        (define flv (make-flvector (vector-length v)))
        (for ([x (in-vector v)] [i (in-naturals)])
          (flvector-set! flv i (fl (f x))))
        flv)
      (define (trace-fl-value trace)
        (define value (trace-value trace))
        (if (real? value) value +nan.0))
      (define (get-field-value field)
        (case field
          [(trace) tracev]
          [(value) (vector-map trace-value tracev)]
          [(log-joint) (vector-map trace-lj tracev)]
          [(log-prior) (vector-map trace-lprs tracev)]
          [(log-score) (vector-map trace-lobs tracev)]
          [(fl-value) (vector-fl-map trace-fl-value tracev)]
          [(fl-log-joint) (vector-fl-map trace-lj tracev)]
          [(fl-log-prior) (vector-fl-map trace-lprs tracev)]
          [(fl-log-score) (vector-fl-map trace-lobs tracev)]
          [(transition) txinfov]
          [else (error 'steps "unknown result name: ~e" field)]))
      (for/fold ([h (hasheq)]) ([field (in-list fields)])
        (hash-set h field (get-field-value field))))

    (define/public (initialize transition)
      (let loop ([n 0])
        (when (eq? last-trace init-trace)
          (unless (< n retries)
            (error 'initialize-transition
                   "initialization failed after ~s attempts" retries))
          (step transition)
          (loop (add1 n)))))
    ))

(define mcmc-sampler%
  (class sampler-base%
    (init-field mdl
                transition)
    (super-new)

    (define mcmc (new mcmc% (mdl mdl)))
    (define/public (get-mcmc) mcmc)

    (define/public (show)
      (send mcmc show))

    (define/override (sample)
      (define-values (trace txinfo)
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
