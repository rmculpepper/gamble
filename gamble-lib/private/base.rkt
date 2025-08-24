;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base)
         racket/class
         racket/match
         racket/stxparam
         "util/density.rkt"
         (only-in "dist/base.rkt" dist-sample dist-density)
         (only-in "dist/discrete.rkt" for/discrete-dist))
(provide (all-defined-out))

;; Defines interfaces, base classes, and parameters.

;; ============================================================
;; Stochastic models

;; A (Model X) is one of
;; - (model (StochasticCtx -> X))
;; - (-> X)

;; Addr is passed by dynamic protocol (continuation mark).

(struct model (proc))

;; ============================================================
;; Samplers

(define weighted-sampler<%>
  (interface ()
    sample/weight  ;; -> (values A PosReal)

    burn                        ;; Nat -> Void
    generate-discrete-dist      ;; Nat -> DiscreteDist
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

    (define/public (generate-discrete-dist n)
      (for/discrete-dist ([i (in-range n)])
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

(define (sampler->discrete-dist s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-discrete-dist n))
(define (generate-samples s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-samples n))
(define (generate-weighted-samples s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-weighted-samples n))

;; ============================================================
;; Stochastic contexts

;; A Label is one of
;; - (auto-label Addr)  -- managed by model/instrument
;; - Any (not false)    -- chosen by user

(define stochastic-ctx<%>
  (interface ()
    get-functions

    sample      ;; (Dist A) Label/#f -> A
    observe     ;; Dist[X] X -> Void
    dscore      ;; Density -> Void
    lscore      ;; LogReal Nat -> Void
    fail        ;; -> escapes
    mem         ;; (X ... -> Y) -> (X ... -> Y)
    run-model   ;; (Model A ...) Boolean -> (values A ...)

    run-top     ;; (Model A ...) -> (U (list A ...) #f)
    ))

(define plain-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (field [escape-prompt (make-continuation-prompt-tag)]
           [model-only? #f]) ;; mutated
    (super-new)

    (define/public (get-functions)
      (define (ctx-sample dist [label #f]) (sample dist label))
      (define (ctx-dscore dn) (dscore dn))
      (define (ctx-lscore ll) (lscore ll))
      (define (ctx-observe d v) (observe d v))
      (define (ctx-fail [reason #f]) (fail reason))
      (define (ctx-mem f) (mem f))
      (define (ctx-run-model m) (run-model m #f))
      (values ctx-sample ctx-dscore ctx-lscore ctx-observe ctx-fail ctx-mem ctx-run-model))

    (define/public (-unsupported who)
      (error who "called outside of sampling context"))

    (define/public (sample dist label)
      (dist-sample dist))

    (define/public (-dscore who dn)
      (-unsupported who))

    (define/public (dscore dn)
      (-dscore 'dscore dn))
    (define/public (lscore ll)
      (-dscore 'lscore (density #t ll)))
    (define/public (observe d v)
      (-dscore 'observe (dist-density d v)))

    (define/public (fail reason)
      (unless (continuation-prompt-available? escape-prompt)
        (-unsupported 'fail))
      (abort-current-continuation escape-prompt (lambda () #f)))

    (define/public (mem f)
      (define memo-table (make-hash))
      (define (mf . args)
        (hash-ref! memo-table args
                   (if model-only?
                       (lambda () (apply f args))
                       (lambda () (parameterize ((current-stochastic-ctx this))
                                    (apply f args))))))
      (procedure-reduce-arity mf (procedure-arity f) 'memoized-function))

    (define/public (run-model m top?)
      (match m
        [(model proc)
         (proc this)]
        [(? procedure? proc)
         (when model-only?
           (error 'run-model "cannot run dynamic model within static model"))
         (parameterize ((current-stochastic-ctx this))
           (proc))]))

    (define/public (run-top m)
      (when (model? m) (set! model-only? #t))
      (call-with-continuation-prompt
       (lambda ()
         (call-with-values
          (lambda () (run-model m #t))
          list))
       escape-prompt))
    ))

(define initial-stochastic-ctx%
  (class plain-stochastic-ctx%
    (inherit -unsupported)
    (super-new)

    (define/override (sample dist label)
      (-unsupported 'sample))

    (define/override (run-model m top?)
      (send (new plain-stochastic-ctx%) run-top m))
    ))

(define current-stochastic-ctx
  (make-parameter (new initial-stochastic-ctx%)))

(define (ctx-get-functions ctx)
  (send ctx get-functions))

;; ============================================================
;; Primitive operations

(define (dynamic-sample dist [label #f])
  (send (current-stochastic-ctx) sample dist label))

(define (dynamic-dscore dn) (send (current-stochastic-ctx) dscore dn))
(define (dynamic-lscore ll) (send (current-stochastic-ctx) lscore ll))
(define (dynamic-observe dist val) (send (current-stochastic-ctx) observe dist val))
(define (dynamic-fail [reason #f]) (send (current-stochastic-ctx) fail reason))

(define (dynamic-mem f) (send (current-stochastic-ctx) mem f))
(define (dynamic-run-model m) (send (current-stochastic-ctx) run-model m #f))

(define-syntax-parameter sample
  (make-rename-transformer (quote-syntax dynamic-sample)))
(define-syntax-parameter dscore
  (make-rename-transformer (quote-syntax dynamic-dscore)))
(define-syntax-parameter lscore
  (make-rename-transformer (quote-syntax dynamic-lscore)))
(define-syntax-parameter observe
  (make-rename-transformer (quote-syntax dynamic-observe)))
(define-syntax-parameter fail
  (make-rename-transformer (quote-syntax dynamic-fail)))
(define-syntax-parameter mem
  (make-rename-transformer (quote-syntax dynamic-mem)))
(define-syntax-parameter run-model
  (make-rename-transformer (quote-syntax dynamic-run-model)))

(define-syntax-rule (with-ctx ctx body ...)
  (let-values ([(ctx-sample ctx-dscore ctx-lscore ctx-observe ctx-fail ctx-mem ctx-run-model)
                (ctx-get-functions ctx)])
    (syntax-parameterize ([sample (make-rename-transformer (quote-syntax ctx-sample))]
                          [dscore (make-rename-transformer (quote-syntax ctx-dscore))]
                          [lscore (make-rename-transformer (quote-syntax ctx-lscore))]
                          [observe (make-rename-transformer (quote-syntax ctx-observe))]
                          [fail (make-rename-transformer (quote-syntax ctx-fail))]
                          [mem (make-rename-transformer (quote-syntax ctx-mem))]
                          [run-model (make-rename-transformer (quote-syntax ctx-run-model))])
      body ...)))
