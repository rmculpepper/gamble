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

@title[#:tag "util"]{Utilities}

@; ============================================================
@section[#:tag "stat-util"]{Statistical Utilities}

@defstruct*[statistics
            ([dim exact-positive-integer?]
             [n exact-positive-integer?]
             [mean vector?]
             [cov vector?])]{

Represents some basic statistics of a sample sequence of real vectors
of compatible shapes. The @racket[dim] field represents the dimension
of the sample vectors; @racket[n] is the number of samples;
@racket[mean] is the mean of the samples; and @racket[cov] is the
covariance matrix.
}

@deftogether[[
@defproc[(sampler->statistics [s sampler?]
                              [n exact-positive-integer?]
                              [f (-> any/c (vectorof real?))])
         statistics?]
@defproc[(samples->statistics [samples (vectorof (vectorof real?))])
         statistics?]
]]

@defproc[(sampler->mean+variance [sampler sampler?]
                                 [n exact-positive-integer?]
                                 [f (-> any/c real?) (lambda (x) x)])
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


@; ============================================================
@section[#:tag "test-util"]{Utilities for Testing and Comparing Distributions}

@deftogether[[
@defproc[(sampler->KS [sampler sampler?]
                      [iterations exact-positive-integer?]
                      [dist dist?])
         (>=/c 0)]
@defproc[(samples->KS [samples (vectorof real?)]
                      [dist dist?])
         real?]
]]{

Calculates the
@hyperlink["http://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov
statistic} of a sample set with @racket[dist]. The result is a measure
of the goodness of fit of the samples to the distribution.

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















@; ============================================================
@section[#:tag "misc-util"]{Miscellaneous Utilities}

@defproc[(probability? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is a real number between 0 and 1
(inclusive), @racket[#f] otherwise.
}

@defparam[verbose? v? boolean?]{

Parameter that controls whether informative messages are printed by
solvers and ERPs.
}

@defproc[(repeat [thunk (-> any/c)]
                 [n exact-nonnegative-integer?])
         list?]{

Calls @racket[thunk] @racket[n] times, accumulating the results in a
list.

@examples[#:eval the-eval
(repeat flip 10)
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
