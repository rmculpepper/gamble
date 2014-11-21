;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; A language for probabilistic programming

#lang racket/base
(require (for-syntax racket/base
                     "private/instrument-analysis.rkt")
         (rename-in typed/racket/base
                    [#%module-begin typed-module-begin]
                    [#%top-interaction typed-top-interaction])
         "private/instrument.rkt")
(provide (all-from-out typed/racket/base)
         (rename-out [typed-instrumenting-module-begin #%module-begin]
                     [typed-instrumenting-top-interaction #%top-interaction]))

(define-syntax (typed-instrumenting-module-begin stx)
  (syntax-case stx ()
    [(ti-module-begin form ...)
     (with-syntax ([e-module-body
                    (analyze
                     (local-expand #'(#%plain-module-begin form ...) 'module-begin null))])
       (with-syntax ([(pmb instr-form ...)
                      (local-expand #'(instrument e-module-body #:un) 'module-begin null)])
         #'(typed-module-begin instr-form ...)))]))

(define-syntax (typed-instrumenting-top-interaction stx)
  (syntax-case stx ()
    [(ti-top-interaction . e)
     (let ([estx (local-expand #'(typed-top-interaction . e) 'top-level #f)])
       (syntax-case estx (begin)
         [(begin form ...)
          #'(begin (instrumenting-top-interaction . form) ...)]
         [form
          (with-syntax ([e-form (analyze (local-expand #'form 'top-level null))])
            #'(instrument e-form #:un))]))]))

;; ============================================================

(module instrumentation-type-env typed-racket/base-env/extra-env-lang
  (require "private/context.rkt"
           "private/instrument.rkt"
           (for-syntax typed-racket/rep/type-rep))
  (type-environment
   [the-context (-Param Univ Univ)]
   [obs-mark (make-Continuation-Mark-Keyof Univ)]
   [observing? (-Param Univ Univ)]
   [next-counter (-poly (a b) (-> a b -Integer))]))

(require 'instrumentation-type-env)
(provide (all-from-out 'instrumentation-type-env))

;; ============================================================

(require "matrix.rkt")
(provide (all-from-out "matrix.rkt"))

(module prob-types typed/racket/base
  (define-type (Sampler a)
    (-> a))
  (provide Sampler))
(require 'prob-types)
(provide (all-from-out 'prob-types))

(module prob-type-env typed-racket/base-env/extra-env-lang
  (require (for-syntax typed-racket/rep/type-rep))

  (require "dist.rkt") ;; bleh, have contracts
  (provide (all-from-out "dist.rkt"))

  ;; bleh, don't know how to create new polymorphic datatypes
  (begin-for-syntax
    ;; FIXME: currently, only one dist type; should be polymorphic
    (define -DistTop
      (make-Base 'Dist #'dist? (lambda _ #f) #f))
    (define (-Sampler a)
      (-> a)))

  (require "private/interfaces.rkt")
  (require (rename-in "private/prob-util.rkt"
                      [sampler->discrete-dist sampler->discrete-dist*]))
  (require "private/stat.rkt")
  (require "private/prob-mh.rkt")
  (require "private/prob-syntax.rkt")
  (require "private/context.rkt")
  (provide observe
           rejection-sampler
           importance-sampler
           mh-sampler
           enumerate
           enum-importance-sampler
           pdelay
           deflazy
           defmem
           table)
  (require "private/prob-enum.rkt")

  ;; ------------------------------------------------------------
  ;; Replacements and simplifications for difficult-to-type functions

  (define (sampler->discrete-dist s n [f values])
    (sampler->discrete-dist* s n f))

  ;; ------------------------------------------------------------
  ;; Type environment

  (type-environment
   [verbose? (-Param -Boolean -Boolean)]
   ;; [weighted-sampler? (make-pred-ty -WeightedSampler)]
   ;; [sampler? (make-pred-ty -Sampler)]
   [generate-samples (-poly (a) (-> (-Sampler a) -Integer (-vec a)))] ;; FIXME
   ;; [generate-weighted-samples (-> -WeightedSampler -Integer (-vec (-pair Univ -Real)))] ;; FIXME

   ;; ----------------------------------------
   ;; mem: FIXME: polydots?
   [mem (-poly (a b) (-> (-> a b) (-> a b)))]
   ;; FIXME: [sample (-poly (a) (-> (-Dist a) a))]
   [sample (-> -DistTop Univ)]
   ;; FIXME: [observe-at (-poly (a) (-> (-Dist a) -a -a))]
   [observe-at (-> -DistTop Univ Univ)]
   [fail (-> (Un))]
   [sampler->discrete-dist ;; NOTE: different version; FIXME: optional function like sort :(
    (-poly (a) (-> (-Sampler a) -Integer -DistTop))]
   [indicator/value
    (-> Univ (-> Univ -Real))]
   [indicator/predicate
    (-> (-> Univ -Boolean) (-> Univ -Real))]
   ;; discrete-dist-error
   [repeat
    (-poly (a) (-> (-> a) -Integer (-lst a)))]
   ;; resample
   ;; probability?
   [flip
    (cl->* (-> -Boolean)
           (-> -Real -Boolean))]
   [bernoulli
    (cl->* (-> (Un -Zero -One))
           (-> -Real (Un -Zero -One)))]
   [categorical
    (-> (-vec -Real) -Integer)]
   [discrete
    (-poly (a) (-> (-lst (-pair a -Real)) a))]
   [discrete*
    (-poly (a)
           (cl->* (-> (-vec a) a)
                  (-> (-vec a) (-vec -Real) a)))]
   [discrete-uniform
    (-> -Integer -Integer)]
   [geometric
    (cl->* (-> -Integer)
           (-> -Real -Integer))]
   [poisson
    (-> -Real -Real)]
   [binomial
    (-> -Integer -Real -Integer)]
   [beta
    (-> -Real -Real -Real)]
   [cauchy
    (cl->* (-> -Real)
           (-> -Real -Real))]
   [exponential
    (cl->* (-> -Real)
           (-> -Real -Real))]
   [gamma
    (cl->* (-> -Real)
           (-> -Real -Real)
           (-> -Real -Real -Real))]
   [inverse-gamma
    (cl->* (-> -Real)
           (-> -Real -Real)
           (-> -Real -Real -Real))]
   [logistic
    (cl->* (-> -Real)
           (-> -Real -Real)
           (-> -Real -Real -Real))]
   [normal
    (cl->* (-> -Real)
           (-> -Real -Real)
           (-> -Real -Real -Real))]
   [pareto
    (cl->* (-> -Real)
           (-> -Real -Real)
           (-> -Real -Real -Real))]
   [uniform
    (cl->* (-> -Real)
           (-> -Real -Real)
           (-> -Real -Real -Real))]
   [dirichlet
    (-> (-vec -Real) (-vec -Real))]
   ;; multi-normal
   ;; wishart
   ;; inverse-wishart
   [factor (-> -Real -Void)]

   ;; ----------------------------------------
   [sampler->mean+variance ;; FIXME
    (-> (-Sampler -Real) -Integer (-values (list -Real -Real)))]
   ;; struct statistics
   ;; sampler->statistics
   ;; samples-statistics
   ;; sampler->KS
   ;; samples->KS

   ;; ----------------------------------------
   [observe*
    (-poly (a) (-> (-> a) a a))]

   ;; ----------------------------------------
   [rejection-sampler*
    (-poly (a) (-> (-> a) (-Sampler a)))]
   ;; importance-sampler
   [mh-sampler* ;; FIXME
    (-poly (a) (-> (-> a) (-Sampler a)))]
   [enumerate* ;; FIXME
    (-poly (a) (-> (-> a) (Un -False -Real) -Boolean -DistTop))]
   ;; enum-importance-sampler*
   ;; pdelay
   ;; table
   ;; ppromise?
   ;; pforce (-> ppromise? any)
   ;; table?
   )

#|
(provide mh-transition?
         (contract-out
          [cycle
           (->* [] [] #:rest (listof mh-transition?) mh-transition?)]
          [sequence
           (->* [] [] #:rest (listof mh-transition?) mh-transition?)]
          [single-site
           (->* [] [#:zone any/c #:record-obs? any/c] mh-transition?)]
          [multi-site
           (->* [] [#:zone any/c #:record-obs? any/c] mh-transition?)]
          [hmc
           (->* [] [(>/c 0) exact-positive-integer? #:zone any/c] mh-transition?)]
          [slice
           (->* [] [#:scale (>/c 0) #:zone any/c] mh-transition?)]
          [enumerative-gibbs
           (->* [] [#:zone any/c #:record-obs? any/c] mh-transition?)]))

(require "private/ho-functions.rkt")
(provide (all-from-out "private/ho-functions.rkt"))

(require "private/particle-filter.rkt")
(provide particles?
         (contract-out
          [make-particles
           (->* [exact-nonnegative-integer?]
                [any/c]
                particles?)]
          [make-parallel-particles
           (->* [exact-nonnegative-integer?]
                [any/c]
                particles?)]
          [particles-count
           (-> particles? exact-nonnegative-integer?)]
          [particles-update
           (->* [particles? procedure?]
                [exact-nonnegative-integer?]
                particles?)]
          [particles-score
           (->* [particles? procedure?]
                [exact-nonnegative-integer?]
                particles?)]
          [particles-resample
           (->* [particles?]
                [exact-nonnegative-integer? #:alg (or/c #f 'multinomial 'residual)]
                particles?)]
          [particles-effective-count
           (-> particles? real?)]
          [particles-effective-ratio
           (-> particles? real?)]
          [particles-weighted-states
           (-> particles? vector?)]
          [particles-states
           (-> particles? vector?)]
          [in-particles
           (-> particles? sequence?)]))

(require "private/serializable-lambda.rkt")
(provide lambda/s
         define/s)
|#
  )

(require 'prob-type-env)
(provide (all-from-out 'prob-type-env))

;; FIXME: use typed reader instead
(module reader syntax/module-reader
  prob/typed)
