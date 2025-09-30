;; Copyright (c) 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         racket/flonum
         racket/vector
         "addr.rkt"
         "base.rkt"
         "mcmc/base.rkt"
         "mcmc/transitions.rkt"
         "util/real.rkt")
(provide (all-from-out "mcmc/base.rkt")
         (all-defined-out))

;; ============================================================
;; Transitions

(define (initialize-transition [get-value (lambda (tag dist prev) #f)])
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
                init-addr
                [retries 10]
                [last-trace init-trace])
    (super-new)

    (define mrun (new model-runner% (mdl mdl) (init-addr init-addr)))

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
                init-addr
                transition)
    (super-new)

    (define mcmc (new mcmc% (mdl mdl) (init-addr init-addr)))
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
                      #:transition [transition (single-site-transition)]
                      #:address-seed [addr-seed init-hash-addr])
  (let ([transition
         (cond [(mcmc-transition? transition) transition]
               [else (single-site-transition transition)])]
        [init-addr (if (fixnum? addr-seed) addr-seed '(0))])
    (define s (new mcmc-sampler% (mdl mdl) (transition transition) (init-addr init-addr)))
    (when initialize
      (send s initialize initialize))
    s))

;; ============================================================

;; model-slice : (Model A) GetValue
;;            -> (Listof Tag) Dist[X]/#f (X ... -> Real) (X ... -> (values A Real))
;; where GetValue = (Tag Dist[X] (U #f (ProposeValue X)) -> (U #f (ProposeValue X))
(define (model-slice mdl
                     [get-value (lambda (tag dist prev) #f)]
                     #:address-seed [addr-seed init-hash-addr]
                     #:debug? [debug? #f])
  (define init-ctx
    (new initializing-tracing-stochastic-ctx%
         (prev-db (hash)) (new-keys null) (get-value get-value)))
  (define init-addr (if (fixnum? addr-seed) addr-seed '(0)))
  (define mrun (new model-runner% (mdl mdl) (init-addr init-addr)))
  (define init-trace (send mrun eval/ctx init-ctx))
  (define keys (send init-ctx get-new-keys))
  (define pdist
    (match keys
      [(list key) (send mrun get-slice-posterior 'model-slice key init-trace)]
      [_ #f]))
  (define eval-slice (send mrun make-eval-slice 'model-slice keys init-trace))
  (define-values (tags dists)
    (let ([init-db (trace-db init-trace)])
      (for/lists (tags dists) ([key (in-list keys)])
        (let ([e (hash-ref init-db key)]) (values (entry-tag e) (entry-dist e))))))
  (when debug?
    (printf "Slice addresses to tags and priors:\n")
    (for ([key (in-list keys)] [tag (in-list tags)] [dist (in-list dists)])
      (printf "  ~e : ~e, ~e\n" key tag dist))
    (send mrun show))
  (define (eval-slice* xs mini?)
    (define delta-db (for/hash ([key (in-list keys)] [x (in-list xs)])
                       (values key (proposal-value x 0.0))))
    (eval-slice delta-db mini?))
  (values tags
          pdist
          (procedure-reduce-arity
           (lambda xs (trace-lj (eval-slice* xs #t)))
           (length keys) 'model-logjoint)
          (procedure-reduce-arity
           (lambda xs
             (define tr (eval-slice* xs #f))
             (values (and tr (trace-value tr)) (trace-lj tr)))
           (length keys) 'model-eval+logjoint)))
