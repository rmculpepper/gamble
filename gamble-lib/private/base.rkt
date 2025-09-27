;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse racket/list)
         racket/class
         racket/match
         racket/stxparam
         "util/density.rkt"
         (only-in "dist/base.rkt" dist? dist-sample dist-density)
         (only-in "dist/discrete.rkt" for/discrete-dist))
(provide (all-defined-out))

;; Defines interfaces, base classes, and parameters.

;; ============================================================

(struct exn:fail:gamble exn:fail (info))
(struct exn:fail:gamble:structural exn:fail:gamble ())

;; ============================================================
;; Stochastic models

;; (Model X) = (model (StochasticCtx Addr -> X))

(struct model (proc))
(struct model/tracing model (gproc csbase))

(define slicer<%>
  (interface ()
    eval-top        ;; (Model X) -> X
    get-slice-eval  ;; [#:keys (Listof DBKeys)] -> (values ReEval Real Real)
    ;;                  where ReEval = (StochasticCtx -> X)
    ))

;; ============================================================
;; Samplers

(define weighted-sampler<%>
  (interface ()
    sample/weight  ;; -> (values A PosReal)

    burn                        ;; Nat -> Void
    generate-discrete-dist      ;; Nat [#:normalize? Boolean] -> DiscreteDist
    generate-weighted-samples   ;; Nat -> (values (Vectorof A) (Vectorof PosReal))
    ))

(define sampler<%>
  (interface (weighted-sampler<%>)
    sample  ;; -> A

    generate-samples            ;; Nat -> (Vectorof A)
    ))

(define (weighted-sampler? x) (is-a? x weighted-sampler<%>))
(define (sampler? x) (is-a? x sampler<%>))

(define weighted-sampler-base%
  (class* object% (weighted-sampler<%>)
    (super-new)

    (abstract sample/weight)

    (define/public (burn n)
      (for ([i (in-range n)])
        (sample/weight))
      (void))

    (define/public (generate-discrete-dist n #:normalize? [normalize? #t])
      (for/discrete-dist #:normalize? normalize? ([i (in-range n)])
        (sample/weight)))

    (define/public (generate-weighted-samples n)
      (define vs (make-vector n))
      (define ws (make-vector n))
      (for ([i (in-range n)])
        (define-values (v w) (sample/weight))
        (vector-set! vs i v)
        (vector-set! ws i w))
      (values vs ws))
    ))

(define sampler-base%
  (class* weighted-sampler-base% (sampler<%>)
    (super-new)

    (define/override (sample/weight) (values (sample) 1))
    (abstract sample)

    ;; ----

    (define/public (generate-samples n)
      (define vs (make-vector n))
      (for ([i (in-range n)])
        (vector-set! vs i (sample)))
      vs)
    ))

(define (sampler->discrete-dist s n #:burn [nburn 0] #:normalize? [normalize? #t])
  (send s burn nburn)
  (send s generate-discrete-dist n #:normalize? normalize?))
(define (generate-samples s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-samples n))
(define (generate-weighted-samples s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-weighted-samples n))

;; ============================================================
;; Stochastic contexts

;; Tag = Any, chosen by user

(define stochastic-ctx<%>
  (interface ()
    get-functions

    sample      ;; (Dist A) Tag Addr/#f -> A
    observe     ;; Dist[X] X -> Void
    dscore      ;; Density -> Void
    lscore      ;; LogReal Nat -> Void
    fail        ;; -> escapes
    mem         ;; (X ... -> Y) Addr/#f -> (X ... -> Y)
    run-model   ;; (Model A ...) Addr/#f -> (values A ...)

    ;; run-top  ;; varies, but often: (-> (values A ...)) -> (U (list A ...) #f)
    ))

(define base-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (field [escape-prompt (make-continuation-prompt-tag)])
    (super-new)

    (define/public (get-functions)
      (define (ctx-sample dist [tag #f]) (sample dist tag #f))
      (define (ctx-dscore dn) (dscore dn))
      (define (ctx-lscore ll) (lscore ll))
      (define (ctx-observe d v) (observe d v))
      (define (ctx-fail [reason #f]) (fail reason))
      (define (ctx-mem f) (mem f #f))
      (define (ctx-run-model m) (run-model m #f))
      (define (ctx-sample/addr dist tag addr) (sample dist tag addr))
      (values ctx-sample ctx-dscore ctx-lscore ctx-observe ctx-fail ctx-mem ctx-run-model
              ctx-sample/addr))

    (define/public (-unsupported who)
      (error who "called outside of sampling context"))

    (define/public (sample dist tag addr)
      (unless (dist? dist) (raise-argument-error 'sample "dist?" dist))
      (-sample dist tag addr))

    (define/public (-sample dist tag addr)
      (dist-sample dist))

    (define/public (-dscore who dn)
      (-unsupported who))

    (define/public (dscore dn)
      (unless (density? dn) (raise-argument-error 'dscore "density?" dn))
      (-dscore 'dscore dn))
    (define/public (lscore ll)
      (unless (real? ll) (raise-argument-error 'lscore "real?" ll))
      (-dscore 'lscore (density #t ll)))
    (define/public (observe dist value)
      (unless (dist? dist) (raise-argument-error 'observe "dist?" dist))
      (-dscore 'observe (dist-density dist value)))

    (define/public (fail reason)
      (unless (continuation-prompt-available? escape-prompt)
        (-unsupported 'fail))
      (abort-current-continuation escape-prompt (lambda () #f)))

    (define/public (mem f addr)
      (define memo-table (make-hash))
      (define (mf . args)
        (hash-ref! memo-table args (lambda () (apply f args))))
      (procedure-reduce-arity mf (procedure-arity f) 'memoized-function))

    (define/public (run-model m addr)
      (unless (model? m) (raise-argument-error 'run-model "model?" m))
      ((model-proc m) this addr))

    (define/public (run-top top)
      (match top
        [(? procedure? proc)
         (call-with-continuation-prompt
          (lambda () (call-with-values proc list))
          escape-prompt)]
        [(? model? m)
         (run-top (lambda () (run-model m #f)))]))
    ))

(define scoring-stochastic-ctx%
  (class base-stochastic-ctx%
    (inherit fail)
    (field [obs-dn one-density])
    (super-new)

    (define/public (get-observation-density) obs-dn)

    (define/override (-dscore who dn)
      (set! obs-dn (density* obs-dn dn))
      (when (density-zero? obs-dn) (fail who)))
    ))

(define (top-level-run-model m)
  (define subctx (new scoring-stochastic-ctx%))
  (define result (send subctx run-top m))
  (cond [(list? result)
         (printf "[run-model] log likelihood = ~s\n"
                 (density->real (send subctx get-observation-density) #t))
         (apply values result)]
        [else
         (printf "[run-model] log likelihood = ~s (failed)\n" -inf.0)
         (void)]))

;; ============================================================
;; Primitive operations

(begin-for-syntax
  (define (out-of-context stx)
    (raise-syntax-error #f "used out of model context" stx)))

(define-syntax-parameter sample out-of-context)
(define-syntax-parameter mem out-of-context)
(define-syntax-parameter dscore out-of-context)
(define-syntax-parameter lscore out-of-context)
(define-syntax-parameter observe out-of-context)
(define-syntax-parameter fail out-of-context)
(define-syntax-parameter run-model
  (make-rename-transformer (quote-syntax top-level-run-model)))

(define (ctx-get-functions ctx)
  (send ctx get-functions))

(define-syntax-rule (with-ctx ctx body ...)
  (let-values ([(ctx-sample ctx-dscore ctx-lscore ctx-observe ctx-fail ctx-mem ctx-run-model
                            ctx-sample/addr)
                (ctx-get-functions ctx)])
    (syntax-parameterize ([sample (make-rename-transformer (quote-syntax ctx-sample))]
                          [mem (make-rename-transformer (quote-syntax ctx-mem))]
                          [run-model (make-rename-transformer (quote-syntax ctx-run-model))]
                          [dscore (make-rename-transformer (quote-syntax ctx-dscore))]
                          [lscore (make-rename-transformer (quote-syntax ctx-lscore))]
                          [observe (make-rename-transformer (quote-syntax ctx-observe))]
                          [fail (make-rename-transformer (quote-syntax ctx-fail))])
      body ...)))

;; ============================================================

;; structural : X ... -> (values X ...)
;; In graph-tracing mode, asserts that arguments are "structural", so the
;; results are considered "path-constant". Otherwise, equivalent to `values`.
;; Could be parameterized with tags (structural wrt these RVs but not those),
;; but probably not worth the complexity.
(define (structural . vs) (apply values vs))
