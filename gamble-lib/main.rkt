#lang racket/base
(require racket/contract
         "dist.rkt"
         "private/base.rkt"
         "private/addr.rkt"
         "private/model.rkt"
         "private/rejection.rkt"
         "private/importance.rkt"
         "private/mcmc.rkt"
         "private/enumerate.rkt"
         "private/samples.rkt"
         "private/util/real.rkt")
(provide (all-from-out "dist.rkt")

         sampler?
         model?

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
          [rejection-sampler
           (-> model? any)]
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
          [slice-transition
           (->* []
                [#:gibbs? boolean?
                 #:method (or/c 'double 'step)
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
          [samples->empirical-cdf
           (->* [sample-frame/c]
                [#:normalize? boolean?]
                (-> real? real?))]
          [samples-KS-statistic
           (-> sample-frame/c (or/c dist? (-> real? real?) sample-frame/c)
               real?)]
          [samples-KS-test
           (->* [sample-frame/c (or/c dist? (-> real? real?) sample-frame/c)]
                [probability?]
                boolean?)]
          [samples-G-statistic
           (-> sample-frame/c finite-dist?
               real?)]
          [samples-G-test
           (->* [sample-frame/c finite-dist?]
                [(or/c #f exact-positive-integer?) probability?]
                boolean?)]))

(define mcmc-transition/single-site/c
  (or/c #f
        proposal-value?
        proposal-kernel?
        mcmc-transition/single-site?
        (-> any/c dist? any/c
            (recursive-contract mcmc-transition/single-site/c))))

(define sample-frame/c hash?) ;; FIXME
