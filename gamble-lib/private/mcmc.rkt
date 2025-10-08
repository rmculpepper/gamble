;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/class
         racket/match
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

(define (gibbs-transition [fallback (slice-transition)])
  (new gibbs-transition% (fallback fallback)))

(define (slice-transition #:method [method 'double]
                          #:W [W 1.0]
                          #:M [M +inf.0]
                          #:SD [SD 5.0])
  (new slice-transition% (method method) (W W) (M M) (SD SD)))

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

    ;; step1 : Transition -> (values Trace TxInfo)
    (define/public (step1 transition)
      (log-mcmc-info "START transition ~e" transition)
      (define-values (new-trace new-txinfo)
        (send transition run mrun last-trace))
      (cond [new-trace
             (set! last-trace new-trace)
             (values new-trace new-txinfo)]
            [else
             (values last-trace new-txinfo)]))

    ;; step : Symbol Nat Nat Transition (Listof Symbol)/#f -> (Hasheq Symbol Any)/#f
    (define/public (step who n thin transition fields)
      (define tracev (and fields (make-vector n)))
      (define txinfov (and fields (memq 'transition fields) (make-vector n)))
      (call-with-continuation-barrier
       (lambda ()
         (for ([i (in-range n)])
           (for ([j (in-range thin)])
             (step1 transition))
           (define-values (trace txinfo) (step1 transition))
           (when tracev (vector-set! tracev i trace))
           (when txinfov (vector-set! txinfov i txinfo)))))
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
          [(transition) txinfov]
          [else (error who "unknown field name: ~e" field)]))
      (and fields
           (for/fold ([h (hash)]) ([field (in-list fields)])
             (hash-set h field (get-field-value field)))))

    (define/public (initialize transition)
      (call-with-continuation-barrier
       (lambda ()
         (let loop ([n 0])
           (when (eq? last-trace init-trace)
             (unless (< n retries)
               (error 'initialize-transition
                      "initialization failed after ~s attempts" retries))
             (step1 transition)
             (loop (add1 n)))))))
    ))

(define mcmc-sampler%
  (class* object% (sampler<%>)
    (init-field mdl
                init-addr
                transition)
    (super-new)

    (define mcmc (new mcmc% (mdl mdl) (init-addr init-addr)))
    (define/public (get-mcmc) mcmc)

    (define/public (show)
      (send mcmc show))

    (define/public (sample)
      (define-values (trace txinfo)
        (send mcmc step1 transition))
      (trace-value trace))

    (define/public (burn n)
      (send mcmc step 'burn n 0 transition #f)
      (void))

    (define/public (generate-samples n thin)
      (send mcmc step 'generate-samples n thin transition '(value)))

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
