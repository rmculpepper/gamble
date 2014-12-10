;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; A language for probabilistic programming

#lang racket/base
(require racket/contract
         racket/dict)

(module reader syntax/module-reader
  prob)

;; See private/instrument.rkt for the call-site instrumenter.
;; See private/context.rkt for discussion of Address representation.
(require "private/instrument.rkt")
(provide (except-out (all-from-out racket/base)
                     #%module-begin #%top-interaction)
         (rename-out [instrumenting-module-begin #%module-begin]
                     [instrumenting-top-interaction #%top-interaction])
         describe-all-call-sites
         describe-call-site)

;; ------------------------------------------------------------

(require "dist.rkt")
(provide (all-from-out "dist.rkt"))

(require "private/dist-define.rkt")
(provide define-dist-type)

(require "matrix.rkt")
(provide (all-from-out "matrix.rkt"))

(require "private/interfaces.rkt")
(provide verbose?
         weighted-sampler?
         sampler?)

(require "private/prob-util.rkt")
(provide (contract-out
          [mem (-> procedure? procedure?)]
          [sample (-> dist? any)]
          [observe-at (-> dist? any/c any)]
          [fail (->* [] [any/c] any)]
          ;; ----
          [sampler->discrete-dist
           (->* [weighted-sampler? exact-nonnegative-integer?]
                [procedure? #:burn exact-nonnegative-integer? #:thin exact-nonnegative-integer?]
                discrete-dist?)]
          [indicator/value
           (-> any/c procedure?)]
          [indicator/predicate
           (-> procedure? procedure?)]
          ;; ----
          [discrete-dist-error
           (-> discrete-dist? discrete-dist?
               (>=/c 0))]
          [repeat
           (-> (-> any) exact-nonnegative-integer? 
               list?)]
          [generate-samples
           (->* [weighted-sampler? exact-nonnegative-integer?]
                [#:burn exact-nonnegative-integer? #:thin exact-nonnegative-integer?]
                any)]
          [generate-weighted-samples
           (->* [weighted-sampler? exact-nonnegative-integer?]
                [#:burn exact-nonnegative-integer? #:thin exact-nonnegative-integer?]
                any)]
          [resample
           (->* [vector? vector?] 
                [exact-nonnegative-integer? #:alg (or/c #f 'multinomial 'residual)]
                vector?)])
         probability?
         ;; The following stochastic procedures have codomain contract of any
         ;; so that their internal call to sample is in tail position (no
         ;; result check frame). (Except for flip, FIXME.)
         (contract-out
          [flip
           (->* [] [probability?] any)]
          [bernoulli
           (->* [] [probability?] any)]
          [categorical
           (-> (vectorof (>=/c 0)) 
               any)]
          [discrete
           (-> dict? any)]
          [discrete*
           (->* [(or/c list? vector?)]
                [(or/c (listof (>=/c 0)) (vectorof (>=/c 0)))]
                any)]
          [discrete-uniform
           (-> exact-positive-integer? any)]
          [geometric
           (->* [] [probability?] any)]
          [poisson
           (-> (>/c 0) any)]
          [binomial
           (-> exact-nonnegative-integer? probability? any)]
          [beta
           (-> (>/c 0) (>/c 0) any)]
          [cauchy
           (->* [] [real? (>/c 0)] any)]
          [exponential
           (->* [] [(>/c 0)] any)]
          [gamma
           (->* [] [(>/c 0) (>/c 0)] any)]
          [inverse-gamma
           (->* [] [(>/c 0) (>/c 0)] any)]
          [logistic
           (->* [] [real? (>/c 0)] any)]
          [normal
           (->* [] [real? (>/c 0)] any)]
          [pareto
           (-> (>/c 0) (>/c 0) any)]
          [uniform
           (->* [] [real? real?] any)]
          [dirichlet
           (-> (vectorof (>/c 0)) any)]
          [multi-normal
           (-> col-matrix? square-matrix? any)]
          [wishart
           (-> real? square-matrix? any)]
          [inverse-wishart
           (-> real? square-matrix? any)]
          [factor
           (-> real? any)]))

(require "private/stat.rkt")
(provide (contract-out
          [struct statistics
            ([dim exact-positive-integer?]
             [n exact-positive-integer?]
             [mean vector?]
             [cov vector?])]
          [sampler->statistics
           (->* [(or/c sampler? procedure?) exact-positive-integer?]
                [procedure? #:burn exact-nonnegative-integer? #:thin exact-nonnegative-integer?]
                statistics?)]
          [samples->statistics
           (-> vector? statistics?)]
          [sampler->KS
           (-> procedure? exact-positive-integer? dist?
               real?)]
          [samples->KS
           (-> vector? dist?
               real?)]
          [sampler->mean
           (->* [(or/c weighted-sampler? procedure?) exact-positive-integer?]
                [procedure? #:burn exact-nonnegative-integer? #:thin exact-nonnegative-integer?]
                any)]
          [sampler->mean+variance
           (->* [(or/c sampler? procedure?) exact-positive-integer?]
                [procedure? #:burn exact-nonnegative-integer? #:thin exact-nonnegative-integer?]
                any)]))

(require "private/prob-syntax.rkt")
(provide observe
         check-observe
         with-zone
         rejection-sampler
         importance-sampler
         mh-sampler
         hmc-sampler
         enumerate
         label
         with-zone
         derivative
         ppromise?
         pdelay
         (contract-out
          [pforce (-> ppromise? any)])
         deflazy
         defmem
         table
         table?)

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

(define proposal/c (or/c proposal? (-> proposal?)))

(require "private/prob-mh.rkt")
(provide mh-transition?
         proposal?
         proposal:resample
         proposal:drift
         (contract-out
          [default-proposal
           (parameter/c proposal/c)]
          [cycle
           (->* [] [] #:rest (listof mh-transition?) mh-transition?)]
          [sequence
           (->* [] [] #:rest (listof mh-transition?) mh-transition?)]
          [single-site
           (->* [] [proposal/c #:zone any/c #:record-obs? any/c] mh-transition?)]
          [multi-site
           (->* [] [proposal/c #:zone any/c #:record-obs? any/c] mh-transition?)]
          [hmc
           (->* [] [(>/c 0) exact-positive-integer? #:zone any/c] mh-transition?)]
          [slice
           (->* []
                [#:method (or/c 'double 'step)
                 #:w (>/c 0)
                 #:m exact-positive-integer?
                 #:zone any/c]
                mh-transition?)]
          [enumerative-gibbs
           (->* [] [#:zone any/c #:record-obs? any/c] mh-transition?)]
          [mixture
           (->* [(vectorof mh-transition?)] [(vectorof (>=/c 0))] mh-transition?)]
          [rerun
           (-> mh-transition?)]))

(require "private/serializable-lambda.rkt")
(provide lambda/s
         define/s)
