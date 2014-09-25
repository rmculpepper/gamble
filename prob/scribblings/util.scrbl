;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     racket/class
                     prob
                     prob/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require prob (only-in prob/viz [hist-pict hist])))

@title[#:tag "util"]{Probabilistic Utilities}

@defproc[(probability? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is a real number between 0 and 1
(inclusive), @racket[#f] otherwise.
}

@defparam[verbose? v? boolean?]{

Parameter that controls whether informative messages are printed by
solvers and ERPs.
}

@defproc[(discrete-dist-error [dist1 discrete-dist?]
                              [dist2 discrete-dist?])
         (>=/c 0)]{

Returns a measure of the difference between two discrete
distributions. The result is the probability mass that would need to
be reassigned in order to transform @racket[dist1] into
@racket[dist2].

@examples[#:eval the-eval
(discrete-dist-error
 (discrete-dist ['A 3/5] ['B 2/5])
 (discrete-dist ['A 1/2] ['B 1/2]))
]

In the example above, @racket[1/10] of the probability mass of
@racket['A] in the first distribution would have to be shifted to
@racket['B] to transform the first distribution into the second.
}

@defproc[(sampler->KS [sampler sampler?]
                      [iterations exact-positive-integer?]
                      [dist dist?])
         (>=/c 0)]{

Gets @racket[iterations] samples from @racket[sampler] and calculates
the @hyperlink["http://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov
statistic} with @racket[dist]. The result is a measure of the goodness
of fit of the samples to the distribution.

@examples[#:eval the-eval
(sampler->KS (rejection-sampler (uniform 0 1))
             1000
             (uniform-dist 0 1))
(sampler->KS (rejection-sampler (normal 0 1))
             1000
             (uniform-dist 0 1))
(sampler->KS (rejection-sampler (for/sum ([i 3]) (uniform -1 1)))
             100
             (normal-dist 0 1))
]
}


@section[#:tag "sample-utils"]{Sampler Utilities}

@defproc[(repeat [thunk (-> any/c)]
                 [n exact-nonnegative-integer?])
         list?]{

Calls @racket[thunk] @racket[n] times, accumulating the results in a
list.

@examples[#:eval the-eval
(repeat flip 10)
]
}

@defproc[(sampler->discrete-dist [sampler sampler?]
                                 [n exact-positive-integer?]
                                 [f (-> any/c any/c) (lambda (x) x)])
         discrete-dist?]{

Generates @racket[n] samples using @racket[(f (sampler))], and
produces a list of the results with probability weights.

@examples[#:eval the-eval
(sampler->discrete-dist (rejection-sampler (flip 1/2)) 100)
]
}

@defproc[(sampler->mean+variance [sampler sampler?]
                                 [f (-> _A real?) (lambda (x) x)])
         (values real? real?)]{

Generates @racket[n] samples using @racket[(f (sampler))], and returns
the mean and variance. If @racket[sampler] does not return real-valued
samples, then @racket[f] must be given and must convert samples into
real values.

@examples[#:eval the-eval
(sampler->mean+variance (rejection-sampler (flip 1/2))
                        100
                        (indicator/value #t))
]
}

@deftogether[[
@defproc[(indicator/value [v any/c])
         (-> any/c (or/c 1 0))]
@defproc[(indicator/predicate [pred (-> any/c boolean?)])
         (-> any/c (or/c 1 0))]
]]{

Produces an indicator function for the value @racket[v] or the set of
values accepted by the predicate @racket[pred], respectively.

@examples[#:eval the-eval
(define z (indicator/value 0))
(z 0)
(z 2.74)
(z 'apple)
]
}


@(close-eval the-eval)
