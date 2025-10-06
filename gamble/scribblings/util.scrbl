;; Copyright (c) 2014-2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/class
                     racket/match
                     gamble gamble/util/dnum))

@(define (wiki suffix . content)
   (apply hyperlink (format "https://en.wikipedia.org/wiki/~a" suffix) content))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble gamble/util/dnum racket/match))
@(the-eval '(random-seed 1))

@title[#:tag "util"]{Utilities}

@; ============================================================
@section[#:tag "samples-util"]{Sample Frames}

@defproc[(samples-count [samples sample-frame/c])
         exact-positive-integer?]{

Returns the number of samples in @racket[samples]. The sample weights, if
present, are not taken into account.
}

@defproc[(samples-fmap [samples sample-frame/c]
                       [f (-> any/c any/c)])
         sample-frame/c]{

Produces a new sample frame by applying @racket[f] to each sample in
@racket[samples]. If @racket[samples] has any fields other than @racket['value],
those fields are retained in the new sample frame.
}

@defproc[(samples-resample [samples sample-frame/c]
                           [n exact-positive-integer?]
                           [#:mode mode (or/c 'multinomial 'residual 'stratified 'systematic)
                                   'systematic])
         sample-frame/c]{

Produces a new same frame by resampling @racket[n] samples from @racket[samples]
using the technique specified by @racket[mode]. The resulting sample frame has
only a @racket['values] field; no fields from @racket[samples] are retained. In
particular, the resulting samples are unweighted.
}

@defproc[(samples->empirical-cdf [samples sample-frame/c]
                                 [#:normalize? normalize? #t])
         (-> real? real?)]{

Calculates the @wiki["Empirical_distribution_function"]{empirical CDF} of
@racket[samples], which must be real-valued.
}

@defproc[(samples-KS-statistic [samples sample-frame/c]
                               [ref (or/c dist? sample-frame/c (-> real? real?))])
         real?]{

Calculates the @wiki["Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov
statistic} of @racket[samples] with respect to the reference distribution, CDF,
or samples @racket[ref].  The result is a measure of the goodness of fit of the
samples to the distribution.

@examples[#:eval the-eval
(samples-KS-statistic
 (generate-samples (importance-sampler (model (sample (uniform-dist 0 1)))) 100)
 (uniform-dist 0 1))
(let ([s (importance-sampler (model (sample (uniform-dist 0 1))))])
  (samples-KS-statistic (generate-samples s 100)
                        (generate-samples s 100)))
(samples-KS-statistic
 (generate-samples (importance-sampler (model (sample (normal-dist 0 1)))) 100)
 (normal-dist 0.1 1))
(samples-KS-statistic
 (generate-samples (importance-sampler (model (sample (uniform-dist -1 1)))) 100)
 (normal-dist 0 1))
]}

@defproc[(samples-KS-test [samples sample-frame/c]
                          [ref (or/c dist? (-> real? real?) sample-frame/c)]
                          [alpha probability? 0.05])
         real?]{

Performs a @wiki["Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov test}
for the goodness-of-fit of @racket[samples] with respect to the reference
distribution, CDF, or sample frame @racket[ref].

If @racket[ref] is a distribution or CDF, the KS statistic measures the
discrepancy between @racket[samples] and @racket[ref]. The 1-sample KS test
returns @racket[#f] if the probability that @racket[ref] produces a sample set
with the given level of discrepancy or greater is less than @racket[alpha].

If @racket[ref] is a sample frame, a 2-sample KS test is performed, which
evaluates whether the two sets of samples correspond to the same underlying
continuous distribution.

@examples[#:eval the-eval
(samples-KS-test
 (generate-samples (importance-sampler (model (sample (uniform-dist 0 1)))) 100)
 (uniform-dist 0 1))
(let ([s (importance-sampler (model (sample (uniform-dist 0 1))))])
  (samples-KS-test (generate-samples s 100)
                   (generate-samples s 100)))
(samples-KS-test
 (generate-samples (importance-sampler (model (sample (normal-dist 0 1)))) 100)
 (normal-dist 0.1 1))
(samples-KS-test
 (generate-samples (importance-sampler (model (sample (uniform-dist -1 1)))) 100)
 (normal-dist 0 1))
]}

@defproc[(samples-G-statistic [samples sample-frame/c]
                              [ref-dist finite-dist?])
         real?]{

Computes the @wiki["G-test"]{G-test statistic} of @racket[samples] with respect
to the reference distribution @racket[ref-dist]. The reference distribution must
be finite; typically it is a discrete or categorical distribution.

@examples[#:eval the-eval
(define fair-dice (categorical-dist (make-vector 6 1/6)))
(define unfair-dice (categorical-dist (vector 0.15 0.15 0.15 0.15 0.15 0.25)))
(samples-G-statistic
 (generate-samples (importance-sampler (model (sample fair-dice))) 1000)
 fair-dice)
(samples-G-statistic
 (generate-samples (importance-sampler (model (sample unfair-dice))) 1000)
 fair-dice)
]}

@defproc[(samples-G-test [samples sample-frame/c]
                         [ref-dist finite-dist?]
                         [df (or/c #f exact-positive-integer?) #f]
                         [alpha probability? 0.05])
         real?]{

Performs a @wiki["G-test"]{G-test} for the goodness-of-fit of
@racket[samples] with respect to the reference distribution
@racket[ref-dist]. The @racket[df] argument represents the @emph{degrees of
freedom} of the test, which should be equal to
@racket[(- _ncategories 1 _nparameters-estimated-from-data)],
where @racket[_ncategories] is the number of distinct categories in
@racket[ref-dist] and @racket[_nparameters-estimated-from-data] is the number of
parameters of @racket[ref-dist] that were estimated using @racket[samples]. If
@racket[df] is @racket[#f], then it is computed from the support of
@racket[ref-dist], assuming that no parameters were estimated.

The G statistic measures the discrepancy between @racket[samples] and
@racket[ref-dist]. The test returns @racket[#f] if the probability that
@racket[ref-dist] produces a sample set with the given level of discrepancy or
greater is less than @racket[alpha].

@examples[#:eval the-eval
(samples-G-test
 (generate-samples (importance-sampler (model (sample fair-dice))) 1000)
 fair-dice)
(samples-G-test
 (generate-samples (importance-sampler (model (sample unfair-dice))) 1000)
 fair-dice)
]}

@;{
@defproc[(samples-PC2 [samples sample-frame/c]
                      [ref-dist finite-dist?])
         real?]{

Computes the @wiki["Pearson's_chi-squared_test"]{Pearson χ² statistic} of
@racket[samples] with respect to the reference distribution
@racket[ref-dist]. The reference distribution must be finite; typically it is a
discrete or categorical distribution.
}

@defproc[(samples-PC2-test [samples sample-frame/c]
                           [ref-dist finite-dist?]
                           [df (or/c #f exact-positive-integer?) #f]
                           [alpha probability? 0.05])
         boolean?]{

Like @racket[samples-G-test],
but uses @wiki["Pearson's_chi-squared_test"]{Pearson's χ² test}.
}
}


@; ============================================================
@section[#:tag "dnum"]{Dimorphic Numbers}

@defmodule[gamble/util/dnum]

A dimorphic number (@racket[dnum?]) represents a nonnegative real. It has two variants:
@itemlist[

@item{@racket[(linear-dnum _x)], the @emph{linear space} representation,
represents the number @racket[_x], which can be either exact or inexact}

@item{@racket[(logspace-dnum _lx)], the @emph{logspace} representation,
represents the number @racket[(exp _lx)]; the number @racket[_lx] must be a flonum}

]
Operations on dnums produce logspace results if any argument is in logspace.

@defproc[(dnum? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a dnum, @racket[#f] otherwise.
}

@defproc[(linear-dnum [x (>=/c 0)]) dnum?]{

Produces a @racket[dnum] representing @racket[x] using the linear space variant.
}

@defproc[(logspace-dnum [lx flonum?]) dnum?]{

Produces a @racket[dnum] representing @racket[(exp lx)] using the logspace variant.
}

@deftogether[[
@defproc[(dnum-logspace? [dn dnum?]) boolean?]
@defproc[(dnum-linear? [dn dnum?]) boolean?]
]]{

Checks whether @racket[dn] is represented using the logspace or linear space
variant, respectively.
}

@deftogether[[
@defproc[(dnum->linear-real [dn dnum?]) real?]
@defproc[(dnum->logspace-real [dn dnum?]) real?]
]]{

Converts @racket[dn] to a real number in linear space or logspace, respectively.

@examples[#:eval the-eval
(dnum->linear-real (linear-dnum 2))
(dnum->logspace-real (linear-dnum 2))
(dnum->linear-real (logspace-dnum -1.0))
(dnum->logspace-real (logspace-dnum -1.0))
]}

@defproc[(dnum-zero? [dn dnum?]) boolean?]{

Returns @racket[#t] if @racket[dn] represents zero, @racket[#f] otherwise.

@examples[#:eval the-eval
(dnum-zero? (linear-dnum 2))
(dnum-zero? (linear-dnum 0))
(dnum-zero? (logspace-dnum -1.0))
(dnum-zero? (logspace-dnum -inf.0))
]}

@deftogether[[
@defproc[(dnum+ [dn1 dnum?] [dn2 dnum?]) dnum?]
@defproc[(dnum- [dn1 dnum?] [dn2 dnum?]) dnum?]
@defproc[(dnum* [dn1 dnum?] [dn2 dnum?]) dnum?]
@defproc[(dnum/ [dn1 dnum?] [dn2 dnum?]) dnum?]
]]{

Computes the sum, difference, product, or ratio of two dnums, respectively.

The @racket[dnum-] operation signals an error if the result would be negative.
The @racket[dnum/] operation signals an error on division by exact zero or if
the result would be @racket[+nan.0].
}

@deftogether[[
@defproc[(dnum-sum [dns (listof dnum?)]) dnum?]
@defproc[(dnum-product [dns (listof dnum?)]) dnum?]
]]{

Computes the sum or product of a list of dnums, respectively.
}

@deftogether[[
@defproc[(dnum=? [dn1 dnum?] [dn2 dnum?]) boolean?]
@defproc[(dnum<? [dn1 dnum?] [dn2 dnum?]) boolean?]
@defproc[(dnum<=? [dn1 dnum?] [dn2 dnum?]) boolean?]
]]{

Compares two dnums.
}

@; ============================================================
@(close-eval the-eval)
