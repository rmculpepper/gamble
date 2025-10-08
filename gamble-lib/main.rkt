#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         racket/flonum
         scramble/contract
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

(define sample-frame/c
  (let ()
    (define vectorof-flonum-proj (get/build-late-neg-projection (vectorof/ic flonum?)))
    (define vectorof-trace-proj (get/build-late-neg-projection (vectorof/ic trace?)))
    (lambda (value/c)
      (define value-ctc (coerce-contract 'sample-frame/c value/c))
      (define vectorof-value-proj (get/build-late-neg-projection (vectorof/ic value-ctc)))
      (define (sample-frame/first-order? v)
        (and (hash? v) (immutable? v)))
      (define (sample-frame-late-neg-proj b)
        (define vectorof-value-proj* (vectorof-value-proj b))
        (define vectorof-flonum-proj* (vectorof-flonum-proj b))
        (define vectorof-trace-proj* (vectorof-trace-proj b))
        (lambda (v missing-party)
          (define (bad . info) (apply raise-blame-error b #:missing-party missing-party v info))
          (unless (and (hash? v) (immutable? v))
            (bad '(expected: "(and/c hash? immutable?)")))
          (unless (hash-has-key? v 'value)
            (bad '("hash missing 'value key")))
          (define vvalue (hash-ref v 'value))
          (define n (and (vector? vvalue) (vector-length vvalue)))
          (for/fold ([wh v]) ([(key value) (in-hash v)] #:when (symbol? key))
            (define b* (blame-add-context b (format "the ~e field of" key)))
            (define (bad* . info) (apply raise-blame-error b* #:missing-party missing-party v info))
            (define (check-vec-len value)
              (unless (and n (vector? value) (= (vector-length value) n))
                (bad* '(expected: "vector of length ~s" given: "~e") n value)))
            (define (update new-value) (if (eq? new-value value) wh (hash-set wh key new-value)))
            (case key
              [(value)
               (when n (unless (> n 0) (bad* '(expected: "non-empty vector" given: "~e") value)))
               (update (vectorof-value-proj* value missing-party))]
              [(log-weight log-joint log-prior log-score)
               (check-vec-len value)
               (update (vectorof-flonum-proj* value missing-party))]
              [(fl-log-joint fl-log-prior fl-log-score)
               (unless (and (flvector? value) (= (flvector-length value) n))
                 (bad* '(expected: "flvector of length ~s" given: "~e") n value))
               wh]
              [(trace)
               (check-vec-len value)
               (update (vectorof-trace-proj* value missing-party))]
              [(transition)
               (check-vec-len value)
               wh]
              [else
               (raise-blame-error b #:missing-party missing-party v
                                  "hash has unexpected symbol key\n  key: ~e\n  value: ~e"
                                  key value)]))))
      (make-contract
       #:name `(sample-frame/c ,(contract-name value-ctc))
       #:first-order sample-frame/first-order?
       #:late-neg-projection sample-frame-late-neg-proj
       #:list-contract? #f))))
