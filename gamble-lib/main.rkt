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
         "private/samples.rkt")
(provide (all-from-out "dist.rkt")

         weighted-sampler<%>
         sampler<%>
         weighted-sampler?
         sampler?
         model?

         (contract-out
          [sampler->discrete-dist
           (->* [weighted-sampler? exact-nonnegative-integer?]
                [#:burn exact-nonnegative-integer?
                 #:normalize? boolean?]
                any)]
          [generate-samples
           (->* [sampler? exact-nonnegative-integer?]
                [#:burn exact-nonnegative-integer?]
                any)]
          [generate-weighted-samples
           (->* [weighted-sampler? exact-nonnegative-integer?]
                [#:burn exact-nonnegative-integer?]
                any)])

         sample
         dscore
         lscore
         observe
         fail
         mem
         run-model
         structural

         current-init-addr

         (rename-out [model* model])

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
                 #:transition mcmc-transition?]
                any)]
          [enumerate
           (->* [model?]
                [#:stop (>=/c 0)
                 #:normalize? boolean?]
                any)])

         proposal?
         (contract-out
          [proposal
           (->* []
                [#:propose1 (or/c #f propose1/c)
                 #:propose2 (or/c #f propose2/c)
                 #:propose-dist (or/c #f propose-dist/c)]
                any)]
          [resample-proposal
           (-> any)]
          [drift-proposal
           (->* []
                [#:params? boolean?
                 #:scale (or/c (>/c 0) (-> any/c dist? (>/c 0)))]
                any)])

         mcmc-transition?
         (contract-out
          [initialize-transition
           (->* [] [(-> any/c dist? (or/c #f (list/c any/c)))] any)]
          [single-site-transition
           (->* []
                [#:proposal proposal?
                 #:any (or/c #f (-> any/c dist? any))]
                any)]
          [multi-site-transition
           (->* []
                [#:proposal proposal?
                 #:all (or/c #f (-> any/c dist? any))]
                any)]
          [enumerative-gibbs-transition
           (->* []
                [#:any (or/c #f (-> any/c dist? any))]
                any)]
          [slice-transition
           (->* []
                [#:method (or/c 'double 'step)
                 #:W (>/c 0)
                 #:Wi exact-positive-integer?
                 #:M exact-positive-integer?
                 #:small-dist-limit exact-nonnegative-integer?
                 #:any (or/c #f (-> any/c dist? any))]
                any)]))

(define propose1/c
  (-> any/c dist? any/c
      (or/c #f (cons/c any/c real?) proposal?)))

(define propose2/c
  (-> any/c dist? dist? any/c
      (or/c #f (cons/c any/c real?) proposal?)))

(define propose-dist/c
  (-> any/c dist? any/c
      (or/c #f dist?)))
