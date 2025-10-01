;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse racket/list)
         racket/class
         racket/match
         racket/stxparam
         "util/dnum.rkt"
         "dist.rkt")
(provide (all-defined-out))

;; Defines interfaces, base classes, and parameters.

;; ============================================================

(struct exn:fail:gamble exn:fail (info))
(struct exn:fail:gamble:structural exn:fail:gamble ())

;; error-structural : Symbol Symbol String -> (escapes)
(define (error-structural who-op who-disallowed what)
  (let/ec escape
    (define cms (continuation-marks escape))
    (define msg (format "~a: ~a\n  change: ~a\n  disallowed by: ~a"
                        who-op "structural change not allowed" what who-disallowed))
    (define info (hasheq 'error 'structural-change
                         'operation who-op
                         'disallowed-by who-disallowed
                         'change what))
    (raise (exn:fail:gamble:structural msg cms info))))

;; ============================================================
;; Stochastic models

;; (Model X) = (model (StochasticCtx Addr -> X))
(struct model (proc gproc csbase))

;; ============================================================
;; SampleFrame

;; A SampleFrame is (Hasheq s:Symbol FieldValue(s))
;; with 'value      : (Vectorof A)      -- always
;;      'log-weight : (Vectorof Real)   -- only importance sampler
;; and other fields determined by sampler (see mcmc-sampler).

(define (samples->discrete-dist sf #:normalize? [normalize? #t])
  (define vs (hash-ref sf 'value))
  (define lws (hash-ref sf 'log-weight #f))
  (make-discrete-dist vs lws #:log-weight? #t #:normalize? normalize?))

;; ============================================================
;; Samplers

(define sampler<%>
  (interface ()
    burn                        ;; Nat -> Void
    generate-samples            ;; Nat Nat -> SampleFrame
    ))

(define (sampler? x) (is-a? x sampler<%>))

(define (sampler->discrete-dist s n
                                #:burn [nburn 0]
                                #:thin [thin 0]
                                #:normalize? [normalize? #t])
  (send s burn nburn)
  (define sf (send s generate-samples n thin))
  (samples->discrete-dist sf #:normalize? normalize?))

(define (generate-samples s n
                          #:burn [nburn 0]
                          #:thin [thin 0])
  (send s burn nburn)
  (send s generate-samples n thin))

;; ============================================================
;; Stochastic contexts

;; Tag = Any, chosen by user

(define stochastic-ctx<%>
  (interface ()
    get-functions

    sample      ;; (Dist A) Tag Addr/#f -> A
    observe     ;; Dist[X] X -> Void
    observe*    ;; Dist[X] (Vectorof X) -> Void
    score       ;; (U Real Dnum) -> Void
    fail        ;; -> escapes
    mem         ;; (X ... -> Y) Addr/#f -> (X ... -> Y)
    run-model   ;; (Model A) Addr/#f -> (values A)

    ;; run-top  ;; varies, but often: (-> A) Addr -> (U (list A) #f)
    ))

;; Failure reasons
;; - `(gamble zero-score ,who)               -- score became zero
;;       where who = 'dscore | 'lscore | 'observe | 'sample-rescore

(define base-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (field [escape-prompt (make-continuation-prompt-tag)])
    (super-new)

    (define/public (get-functions)
      (define (ctx-sample dist [tag #f]) (sample dist tag #f))
      (define (ctx-score dn) (score dn))
      (define (ctx-observe d v) (observe d v))
      (define (ctx-observe* d vs) (observe* d vs))
      (define (ctx-fail [reason #f]) (fail reason))
      (define (ctx-mem f) (mem f #f))
      (define (ctx-run-model m) (run-model m #f))
      (define (ctx-sample/addr dist tag addr) (sample dist tag addr))
      (values ctx-sample ctx-score ctx-observe ctx-observe*
              ctx-fail ctx-mem ctx-run-model ctx-sample/addr))

    (define/public (-unsupported who)
      (error who "called outside of sampling context"))

    (define/public (sample dist tag addr)
      (unless (dist? dist) (raise-argument-error 'sample "dist?" dist))
      (-sample dist tag addr))

    (define/public (-sample dist tag addr)
      (dist-sample dist))

    (define/public (-dscore who dn)
      (-unsupported who))

    (define/public (score dn)
      (cond [(dnum? dn) (-dscore 'score dn)]
            [(real? dn) (-dscore 'score (logspace-dnum dn))]
            [else (raise-argument-error 'score "(or/c real? dnum?)" dn)]))

    (define/public (observe dist value)
      (unless (dist? dist) (raise-argument-error 'observe "dist?" dist))
      (-dscore 'observe (dist-density dist value)))

    (define/public (observe* dist vs)
      (unless (dist? dist) (raise-argument-error 'observe* "dist?" dist))
      (unless (vector? vs) (raise-argument-error 'observe* "vector?" vs))
      (for ([v (in-vector vs)]) (-dscore 'observe* (dist-density dist v))))

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

    (define/public (run-top top [addr #f])
      (match top
        [(? procedure? proc)
         (call-with-continuation-prompt
          (lambda () (list (proc)))
          escape-prompt)]
        [(? model? m)
         (run-top (lambda () (run-model m addr)))]))
    ))

(define scoring-stochastic-ctx%
  (class base-stochastic-ctx%
    (init-field [score-dnum (linear-dnum 1.0)])
    (inherit fail)
    (super-new)

    (define/public (get-score-dnum) score-dnum)

    (define/override (-dscore who dn)
      (set! score-dnum (dnum* score-dnum dn))
      (when (dnum-zero? score-dnum) (fail `(gamble zero-score ,who))))
    ))

(define (top-level-run-model m)
  (define subctx (new scoring-stochastic-ctx%))
  (match (send subctx run-top m)
    [(list v)
     (printf "[run-model] log likelihood = ~s\n"
             (dnum->logspace-real (send subctx get-score-dnum)))
     v]
    [#f
     (printf "[run-model] log likelihood = ~s (failed)\n" -inf.0)
     (void)]))

;; ============================================================
;; Primitive operations

(begin-for-syntax
  (define (out-of-context stx)
    (raise-syntax-error #f "used out of model context" stx)))

(define-syntax-parameter sample out-of-context)
(define-syntax-parameter mem out-of-context)
(define-syntax-parameter score out-of-context)
(define-syntax-parameter observe out-of-context)
(define-syntax-parameter observe* out-of-context)
(define-syntax-parameter fail out-of-context)
(define-syntax-parameter run-model
  (make-rename-transformer (quote-syntax top-level-run-model)))

(define (ctx-get-functions ctx)
  (send ctx get-functions))

(define-syntax-rule (with-ctx ctx body ...)
  (let-values ([(-sample -score -observe -observe* -fail -mem -run-model -sample/addr)
                (ctx-get-functions ctx)])
    (syntax-parameterize ([sample   (make-rename-transformer (quote-syntax -sample))]
                          [mem      (make-rename-transformer (quote-syntax -mem))]
                          [score    (make-rename-transformer (quote-syntax -score))]
                          [observe  (make-rename-transformer (quote-syntax -observe))]
                          [observe* (make-rename-transformer (quote-syntax -observe*))]
                          [fail     (make-rename-transformer (quote-syntax -fail))]
                          [run-model (make-rename-transformer (quote-syntax -run-model))])
      body ...)))

;; ============================================================

;; structural : X ... -> (values X ...)
;; In graph-tracing mode, asserts that arguments are "structural", so the
;; results are considered "path-constant". Otherwise, equivalent to `values`.
;; Could be parameterized with tags (structural wrt these RVs but not those),
;; but probably not worth the complexity.
(define (structural . vs) (apply values vs))
