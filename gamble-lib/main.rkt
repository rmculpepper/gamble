;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         scramble/contract
         "dist.rkt"
         "private/base.rkt"
         "private/addr.rkt"
         "private/model.rkt"
         "private/importance.rkt"
         "private/mcmc.rkt"
         "private/enumerate.rkt"
         "private/samples.rkt"
         "private/util/real.rkt")
(provide (all-from-out "dist.rkt")

         sampler?
         model?
         sample-frame/c

         (contract-out
          [sampler->discrete-dist
           (->* [sampler? exact-nonnegative-integer?]
                [#:burn exact-nonnegative-integer?
                 #:thin exact-nonnegative-integer?
                 #:normalize? boolean?]
                any)]
          [generate-samples
           (->* [sampler? exact-nonnegative-integer?]
                [#:burn exact-nonnegative-integer?
                 #:thin exact-nonnegative-integer?]
                any)])

         sample
         score
         observe
         observe*
         fail
         mem
         run-model
         structural

         (rename-out [model* model])
         begin-model-definitions

         (contract-out
          [importance-sampler
           (->* [model?]
                [#:propose (or/c #f (-> any/c dist? (or/c #f dist?)))]
                any)]
          [mcmc-sampler
           (->* [model?]
                [#:initialize mcmc-transition?
                 #:transition (or/c mcmc-transition? mcmc-transition/single-site/c)]
                any)]
          [enumerate
           (->* [model?]
                [#:stop (>=/c 0)
                 #:discretize (or/c #f (-> any/c real-dist? (or/c #f enumerable-dist?)))
                 #:normalize? boolean?]
                any)])

         (contract-out
          [model-slice
           (->* [model?]
                [(-> any/c dist? (or/c #f proposal-value?)
                     (or/c #f proposal-value?))
                 #:debug? boolean?]
                any)])

         mcmc-transition?
         mcmc-transition/single-site?
         mcmc-transition/single-site/c
         (contract-out
          [initialize-transition
           (->* [] [(-> any/c dist? (or/c #f (list/c any/c)))] any)]
          [single-site-transition
           (->* []
                [mcmc-transition/single-site/c
                 #:any (or/c #f (-> any/c dist? any))]
                any)]
          [gibbs-transition
           (->* [] [(or/c #f mcmc-transition/single-site?)]
                any)]
          [slice-transition
           (->* []
                [#:method (or/c 'double 'step)
                 #:W (>/c 0.0)
                 #:M (or/c exact-nonnegative-integer? +inf.0)
                 #:SD (>=/c 0.0)]
                any)]
          [struct proposal-value
            ([value any/c]
             [l-R/F real?])]
          [struct proposal-kernel
            ([kernel (-> any/c dist?)])])

         (contract-out
          [samples-count
           (-> any-sample-frame/c any)]
          [samples-fmap
           (-> any-sample-frame/c (-> any/c any/c)
               any-sample-frame/c)]
          [samples-resample
           (->* [any-sample-frame/c exact-positive-integer?]
                [#:mode (or/c 'multinomial 'stratified 'systematic)]
                any-sample-frame/c)]
          [samples->discrete-dist
           (->* [any-sample-frame/c]
                [#:normalize? boolean?]
                discrete-dist?)]
          [samples->empirical-cdf
           (->* [real-sample-frame/c]
                [#:normalize? boolean?]
                (-> real? real?))]
          [samples->kde
           (->* [real-sample-frame/c]
                [#:normalize? boolean?]
                (->* [real?] [real? real?] real?))]
          [samples-KS-statistic
           (-> real-sample-frame/c
               (or/c dist? (-> real? real?) real-sample-frame/c)
               real?)]
          [samples-KS-test
           (->* [real-sample-frame/c
                 (or/c dist? (-> real? real?) real-sample-frame/c)]
                [probability?]
                boolean?)]
          [samples-G-statistic
           (-> any-sample-frame/c finite-dist?
               real?)]
          [samples-G-test
           (->* [any-sample-frame/c finite-dist?]
                [(or/c #f exact-positive-integer?) probability?]
                boolean?)]))

(define mcmc-transition/single-site/c
  (or/c #f
        proposal-value?
        proposal-kernel?
        mcmc-transition/single-site?
        (-> any/c dist? any/c
            (recursive-contract mcmc-transition/single-site/c))))

(define (sample-frame/c value/c)
  (define value-ctc (coerce-contract 'sample-frame/c value/c))
  (hash-record/ic
   (hasheq 'value (cond [(eq? value/c any/c) any-vector/ic]
                        [(eq? value/c real?) real-vector/ic]
                        [(eq? value/c flonum?) flonum-vector/ic]
                        [else (vectorof/ic value-ctc)])
           'log-weight flonum-vector/ic
           'log-joint  flonum-vector/ic
           'log-prior  flonum-vector/ic
           'log-score  flonum-vector/ic
           'trace      trace-vector/ic
           'transition any-vector/ic)
   #:name `(sample-frame/c ,(contract-name value-ctc))
   #:other (lambda (key) (if (symbol? key) none/c any/c))
   #:required '(value)))

(define any-vector/ic (vectorof/ic any/c))
(define real-vector/ic (vectorof/ic real?))
(define flonum-vector/ic (vectorof/ic flonum?))
(define trace-vector/ic (vectorof/ic trace?))

(define real-sample-frame/c (sample-frame/c real?))
(define any-sample-frame/c (sample-frame/c any/c))
