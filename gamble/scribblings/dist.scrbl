;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base racket/contract gamble gamble/util/dnum gamble/pict))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble gamble/pict))
@(the-eval '(random-seed 1))

@(define (wiki url-suffix . pre-content)
   (apply hyperlink (string-append "http://en.wikipedia.org/wiki/" url-suffix)
   	  pre-content))

@title[#:tag "dist"]{Probability Distributions}

This section describes the distribution types and operations supported by this
library. This library builds upon the distribution support of
@racketmodname[math/distributions] (see @secref["dist" #:doc '(lib
"math/scribblings/math.scrbl")]), and follows its conventions for distribution
parameters.

@; ------------------------------------------------------------
@section[#:tag "dist-kinds"]{Kinds of Distributions}

@defproc[(dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object, @racket[#f]
otherwise.
}

@defproc[(numeric-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose support is
limited to the real numbers or a subset thereof, @racket[#f]
otherwise. Numeric distributions support additional operations, such as
@racket[dist-cdf].

If @racket[(numeric-dist? v)] is true, then @racket[v] is either an integer
distribution (@racket[integer-dist?]) or a continuous real distribution
(@racket[real-dist?]).
}

@defproc[(integer-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose support is
intrinsically integer-valued, @racket[#f] otherwise.

The distribution types for which @racket[integer-dist?] returns true consist of
exactly the ones listed in @secref["integer-dists"]. A discrete distribution
whose values happen to be integers is not considered an integer distribution.

If @racket[(integer-dist? v)] is true, then @racket[(numeric-dist? v)] is also true.
}

@defproc[(real-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose support is a
real interval and whose CDF is continuous, @racket[#f] otherwise.

The distribution types for which @racket[real-dist?] returns true consist of
exactly the ones listed in @secref["real-dists"] and @secref["dist-transformers"].

If @racket[(real-dist? v)] is true, then @racket[(numeric-dist? v)] is also true.
}

@defproc[(enumerable-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose support is
finite or countably infinite, @racket[#f] otherwise. See also @racket[in-dist].
}

@defproc[(finite-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose
support is finite, @racket[#f] otherwise.

If @racket[(finite-dist? v)] is true, then @racket[(enumerable-dist? v)] is
also true.
}

@; ------------------------------------------------------------
@section[#:tag "dist-ops"]{Distribution Operations}

@defproc[(dist-sample [d dist?]) any/c]{

Produces a sample distributed according to @racket[d].

@emph{Do not use @racket[dist-sample] within a model; use @racket[sample] instead.}

@examples[#:eval the-eval
(for/list ([i 10]) (dist-sample (bernoulli-dist 1/3)))
]}

@defproc[(dist-density [d dist?] [v any/c])
         dnum?]{

Returns the probability density of the value @racket[v] in the distribution
@racket[d], represented as a dnum (@racket[dnum?]). Numeric distributions
generally represent the density in logspace, and discrete distributions
generally represent the density in linear space.

@examples[#:eval the-eval
(dist-density (boolean-dist 1/3) #f)
(dist-density (bernoulli-dist 1/3) 0)
(dist-density (uniform-dist 0 10) 3)
]}

@defproc[(dist-pdf [d dist?] [v any/c] [log? any/c #f]) 
         real?]{

Returns the probability density of the value @racket[v] in distribution
@racket[d]. If @racket[log?] is true, the log density is returned instead.

@examples[#:eval the-eval
(dist-pdf (boolean-dist 1/3) #f)
(dist-pdf (bernoulli-dist 1/3) 0)
(dist-pdf (uniform-dist 0 10) 3)
(dist-pdf (uniform-dist 0 10) 3 #t)
]}

@defproc[(dist-cdf [d numeric-dist?] [v real?] [log? any/c #f] [1-p? any/c #f])
         real?]{

Returns the cumulative probability of the value @racket[v] in distribution
@racket[d]---that is, the probability that a random variable @racket[_X]
distributed according to @racket[d] satisfies @racket[(<= _X v)].  If
@racket[1-p?] is true, then the probability of @racket[(> _X v)] is returned
instead.  If @racket[log?] is true, then the log probability is returned instead
of the probability.

@examples[#:eval the-eval
(dist-cdf (uniform-dist 0 10) 3)
]}

@defproc[(dist-inv-cdf [d numeric-dist?] [p real?] [log? any/c #f] [1-p? any/c #f])
         any/c]{

Returns the inverse of the CDF of @racket[d] at @racket[p].
If @racket[log?] is true, then the inverse at @racket[(exp p)] is used instead.
If @racket[1-p?] is true, then the inverse at @racket[(- 1 p)] is used instead.

@examples[#:eval the-eval
(dist-inv-cdf (uniform-dist 0 10) 0.7)
(dist-inv-cdf (normal-dist 0 1) 0.1)
]}

@defproc[(in-dist [d enumerable-dist?]) sequence?]{

Returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence}
where each element consists of two values: a value from the support of the
distribution and its probability density (@racket[real?]).

@examples[#:eval the-eval
(for ([(v p) (in-dist (bernoulli-dist 1/3))])
  (printf "Result ~s has probability ~s.\n" v p))
]}

@defproc[(in-dist-values [d enumerable-dist?]) sequence?]{

Returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence}
where each element consists of a value from the support of the distribution.
}

@defproc[(dist-total-measure [d dist?]) (>=/c 0)]{

Returns the total probability weight of @racket[d]. For standard integer and
real distributions, this always returns @racket[1], but discrete distributions
and mixture distributions, it can produce other nonnegative values.

@examples[#:eval the-eval
(dist-total-measure (bernoulli-dist 1/2))
(dist-total-measure (hash->discrete-dist (hash)))
]}

@deftogether[[
@defproc[(dist-mean [d numeric-dist?]) (or/c #f real?)]
@defproc[(dist-median [d numeric-dist?]) (or/c #f real?)]
@defproc[(dist-modes [d numeric-dist?]) (or/c #f (listof real?))]
@defproc[(dist-variance [d numeric-dist?]) (or/c #f real?)]
]]{

Returns the mean, median, modes, and variance of the distribution @racket[d],
respectively. If the statistic is undefined, or if this library does not
implement its computation, @racket[#f] is returned.
}

@defproc[(dist-discretize/quantile [d real-dist?]
                                   [n exact-positive-integer?])
         discrete-dist?]{

Returns a discrete distribution of @racket[n] values from the support of
@racket[d]. Each value in the discrete distribution has the same weight,
@racket[(/ n)].

The values are obtained by dividing the unit interval into @racket[n] equal-size
segments and calling @racket[dist-inv-cdf] on the @emph{midpoints} of those
segments.

@examples[#:eval the-eval
(dist-discretize/quantile (uniform-dist 0 8) 4)
]}


@; ------------------------------------------------------------
@section[#:tag "numeric-dist"]{Numeric Distribution Types}

Unless otherwise noted, the distribution types documented in this section
automatically convert their real-valued parameters to @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{flonum} values, and they report
probability densities and cumulative probabilities as flonums, except that the
density of a value that is out of the support type (that is, not a real number)
may be represented as exact @racket[0].

@; ----------------------------------------
@subsection[#:tag "integer-dists"]{Integer Distribution Types}

For integer distributions, the results of @racket[dist-sample] and
@racket[dist-inv-cdf] are exact integers.

@defstruct*[bernoulli-dist
            ([p probability?])]{

Represents a @wiki["Bernoulli_distribution"]{Bernoulli distribution} with
success probability @racket[p]. The distribution's support consists of
@racket[0] and @racket[1]. See also @racket[boolean-dist].

@examples[#:eval the-eval
(dist->pict (bernoulli-dist 1/4))
]}

@defstruct*[binomial-dist
            ([n exact-positive-integer?]
             [p probability?])]{

Represents a @wiki["Binomial_distribution"]{binomial distribution}: the number
of successes given @racket[n] trials each with success probability @racket[p].

@examples[#:eval the-eval
(dist->pict (binomial-dist 10 1/4))
]}

@defstruct*[categorical-dist
            ([weights (vectorof (>=/c 0))])]{

Represents a @emph{zero-based} @wiki["Categorical_distribution"]{categorical
distribution} (sometimes called a discrete distribution, multinomial
distribution, or multinoulli distribution).

The distribution's support consists of the exact integers {@racket[0], ...,
@racket[(sub1 _n)]}, where @racket[_n] is the length of @racket[weights].

@examples[#:eval the-eval
(dist->pict (categorical-dist (vector 0.2 0.3 0.1 0.25 0.15)))
]}

@defstruct*[geometric-dist
            ([p probability?])]{

Represents a @wiki["Geometric_distribution"]{geometric distribution}: the number
of failures before the first success, where each trial has success probability
@racket[p].

@examples[#:eval the-eval
(dist->pict (geometric-dist 1/3))
]}

@defstruct*[negative-binomial-dist
            ([r exact-positive-integer?]
             [p probability?])]{

Represents a @wiki["Negative_binomial_distribution"]{negative binomial
distribution}: the number of failures before @racket[r] successes, where each
trial has success probability @racket[p].

@examples[#:eval the-eval
(dist->pict (negative-binomial-dist 5 0.6))
]}

@defstruct*[poisson-dist
            ([mean (>/c 0)])]{

Represents a @wiki["Poisson_distribution"]{Poisson distribution} with parameter
@racket[mean] (aka @italic{λ}).

@examples[#:eval the-eval
(dist->pict (poisson-dist 5))
]}

@; ----------------------------------------
@subsection[#:tag "real-dists"]{Real Distribution Types}

For real distributions, the results of @racket[dist-sample] and
@racket[dist-inv-cdf] are flonums.

@defstruct*[beta-dist
            ([a (>/c 0)]
             [b (>/c 0)])]{

Represents a @wiki["Beta_distribution"]{beta distribution} with shape parameters
@racket[a] (aka @italic{α}) and @racket[b] (aka @italic{β}).

@examples[#:eval the-eval
(dist->pict (beta-dist 4 3))
]}

@defstruct*[cauchy-dist
            ([mode real?]
             [scale (>/c 0)])]{

Represents a @wiki["Cauchy_distribution"]{Cauchy distribution} with parameters
@racket[mode] (aka @italic{x₀}) and @racket[scale] (aka @italic{γ}).

@examples[#:eval the-eval
(dist->pict (cauchy-dist 2 3))
]}

@defstruct*[exponential-dist
            ([mean (>/c 0)])]{

Represents an @wiki["Exponential_distribution"]{exponential
distribution} with parameter @racket[mean].

Note: A common alternative parameterization uses the rate, @italic{λ} =
@racket[(/ mean)].

@examples[#:eval the-eval
(dist->pict (exponential-dist 4))
]}

@defstruct*[gamma-dist
            ([shape (>/c 0)]
             [scale (>/c 0)])]{

Represents a @wiki["Gamma_distribution"]{gamma distribution} with parameters
@racket[shape] (aka @italic{a}) and @racket[scale] (aka @italic{θ}).

Note: A common alternative parameterization uses the rate, @italic{λ} =
@racket[(/ scale)].

@examples[#:eval the-eval
(dist->pict (gamma-dist 2 1))
]}

@defstruct*[logistic-dist
            ([mean real?]
             [scale (>/c 0)])]{

Represents a @wiki["Logistic_distribution"]{logistic distribution}
with parameters @racket[mean] (aka @italic{μ}) and @racket[scale] (aka @italic{s}).

@examples[#:eval the-eval
(dist->pict (logistic-dist 5 2))
]}

@defstruct*[normal-dist
            ([mean real?]
             [stddev (>/c 0)])]{

Represents a @wiki["Normal_distribution"]{normal (Gaussian) distribution} with
parameters @racket[mean] (aka @italic{μ}) and @racket[stddev]
(standard deviation, aka @italic{σ}, scale).

Note: A common alternative parameterization uses the variance
@italic{σ@superscript{2}}.

@examples[#:eval the-eval
(dist->pict (normal-dist 2 3))
]}

@defstruct*[pareto-dist
            ([scale (>/c 0)]
             [shape (>/c 0)])]{

Represents a @wiki["Pareto_distribution"]{Pareto distribution} with parameters
@racket[scale] (aka @italic{x}@subscript{m}) and @racket[shape] (aka
@italic{α}).

@examples[#:eval the-eval
(dist->pict (pareto-dist 1 2))
]}

@defstruct*[student-t-dist
            ([degrees (>/c 0)]
             [mode real?]
             [scale (>/c 0)])]{

Represents a @wiki["Student's_t-distribution"]{Student's t distribution} with
parameters @racket[degrees] (aka @italic{ν}), @racket[mode] (aka @italic{μ}),
and @racket[scale] (aka @italic{τ}).

@examples[#:eval the-eval
(dist->pict (student-t-dist 2 2 1))
]}

@defstruct*[triangle-dist
            ([lo real?]
             [hi real?]
             [mode real?])]{

Represents a @wiki["Triangle_distribution"]{triangle distribution} with lower
bound @racket[lo], upper bound @racket[hi], and mode at @racket[mode].

Must satisfy @racket[(< lo hi)] and @racket[(<= lo mode hi)].

@examples[#:eval the-eval
(dist->pict (triangle-dist 0 5 4))
]}

@defstruct*[uniform-dist
            ([lo real?]
             [hi real?])]{

Represents a @wiki["Uniform_distribution"]{uniform distribution} with
lower bound @racket[lo] and upper bound @racket[hi].

Must satisfy @racket[(< lo hi)].

@examples[#:eval the-eval
(dist->pict (uniform-dist 0 3))
]}

@; ----------------------------------------
@subsection[#:tag "dist-transformers"]{Real to Real Distribution Transformers}

The following constructors produce real-valued distributions.

@defstruct*[affine-distx
            ([dist real-dist?]
             [a real?]
             [b real?])]{

Represents the distribution of @racket[(+ (* a _X) b)] where
@racket[_X] is distributed according to @racket[dist].

@emph{Note:} The affine transformation is applied to generated samples; its
inverse is applied when evaluating the density, CDF, etc.

@examples[#:eval the-eval
(dist->pict (affine-distx (triangle-dist 0 5 4) 2 1))
]}

@defstruct*[clip-distx
            ([dist real-dist?]
             [a real?]
             [b real?])]{

Clips @racket[dist] to the closed interval [@racket[a], @racket[b]].

If the interval is small, the clipped dist is sampled
using the @racket[dist-inv-cdf] method of @racket[dist]; otherwise,
rejection sampling is used.

@examples[#:eval the-eval
(dist->pict (clip-distx (normal-dist 0 1) -1 1))
]}

@defstruct*[exp-distx
            ([dist real-dist?])]{

Represents the distribution of @racket[(exp _X)] where @racket[_X] is
distributed according to @racket[dist].

@emph{Note:} The @racket[exp] in the name refers to the transformation applied
when sampling. This differs from standard terminology---for example, variables
distributed according to @racket[(exp-distx (normal-dist 0 1))] are customarily
called ``Lognormal'' random variables.

@examples[#:eval the-eval
(dist->pict (exp-distx (normal-dist 0 1)))
]}

@; ------------------------------------------------------------
@subsection[#:tag "discrete-dist"]{Discrete Distribution Type}

A discrete distribution is a distribution whose support is a finite collection
of arbitrary Racket values. The elements of a discrete distribution are
distinguished using @racket[equal?]. The constructors for discrete distributions
detect and coalesce duplicates.

A discrete distribution is not required to be @emph{normalized}. That is, its
probabilities may not sum to one; the sum may also be greater or lesser. In
particular, the empty discrete distribution has zero total probability.

@defproc[(discrete-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a discrete distribution, @racket[#f]
otherwise. All discrete distributions are finite (@racket[finite-dist?]) and
thus enumerable (@racket[enumerable-dist?]).
}

@defform[(discrete-dist [value-expr weight-expr] ...)
         #:contracts ([weight-expr (>=/c 0)])]{

Produces a discrete distribution whose values are the @racket[value-expr]s and
whose probability masses are the corresponding @racket[weight-expr]s.

The total mass of the distribution is @emph{not normalized}. Use
@racket[dist-rescore] to normalize if necessary.

@examples[#:eval the-eval
(discrete-dist ['apple 1/2] ['orange 1/3] ['pear 1/6])
]}

@defproc[(hash->discrete-dist [h (hash/c any/c (>=/c 0))]
                              [#:normalize? normalize? boolean? #t])
         discrete-dist?]{

Produces a discrete distribution, using @racket[h]'s keys as the distrubution's
values and @racket[h]'s values as the distribution's weights. If any value's
weight is zero, that value is dropped from the distribution. If any weight is
inexact, then all weights are converted to inexact numbers.

If @racket[normalize?] is true, then the distribution's probability weights are
normalized to sum to one (or to zero if the distribution is empty). In practice,
if the weights are inexact, their sum might not be exactly @racket[1.0] even
after normalization.

@examples[#:eval the-eval
(hash->discrete-dist (hash 'apple 1/2 'orange 1/3 'pear 1/6))
(hash->discrete-dist (hash 'apple 0.5 'orange 1/3 'pear 1/6))
(hash->discrete-dist (hash))
]}

@defproc[(make-discrete-dist [values vector?]
                             [weights (or/c #f (vectorof (>=/c 0))) #f]
                             [#:log-weight? log-weight? boolean? #f]
                             [#:normalize? normalize? boolean? #t])
         discrete-dist?]{

Produces a discrete distribution on the elements of @racket[values]. If
@racket[weights] is a vector, it must be the same length as @racket[values], and
each element gives the probability of the corresponding value (or the
log-probability, if @racket[log-weight?] is true). If @racket[weights] is
@racket[#f], then the weight of each value is treated as @racket[1] before
normalization. If any weight is inexact, then all weights are converted to
inexact numbers. If @racket[log-weight?] is true, then the weights must be
flonums.

If @racket[normalize?] is true, then the distribution's probability weights are
normalized to sum to one (or to zero if the distribution is empty). In practice,
if the weights are inexact, their sum might not be exactly @racket[1.0] even
after normalization.

@examples[#:eval the-eval
(make-discrete-dist (vector 'apple 'orange 'pear) (vector 1/2 1/3 1/6))
(make-discrete-dist (vector 'apple 'orange 'pear) (vector 0.5 1/3 1/6))
(make-discrete-dist (vector 'red 'blue 'green 'blue) #:normalize? #t)
(make-discrete-dist (vector 'red 'blue 'green 'blue) #:normalize? #f)
]}

@deftogether[[
@defproc[(discrete-dist-values [d discrete-dist?])
         vector?]
@defproc[(discrete-dist-weights [d discrete-dist?])
         vector?]
]]{

Returns the values and weights of @racket[d], respectively.

@examples[#:eval the-eval
(define dd (hash->discrete-dist (hash 'apple 1/2 'orange 1/3 'pear 1/6)))
(discrete-dist-values dd)
(discrete-dist-weights dd)
]}

@defproc[(discrete-distof [predicate predicate/c])
         predicate/c]{

Produces a predicate that returns @racket[#t] if applied to a discrete
distribution whose values all satisfy @racket[predicate], @racket[#f] otherwise.
}


@; ------------------------------------------------------------
@section[#:tag "other-dists"]{Other Distribution Types}

@defstruct*[boolean-dist
            ([p probability?])]{

Like a Bernoulli distribution (@racket[bernoulli-dist]), but the support
consists of the values @racket[#t] and @racket[#f]. Unlike
@racket[bernoulli-dist], this distribution type supports exact success
probabilities @racket[p].

@examples[#:eval the-eval
(dist-pdf (boolean-dist 1/3) #f)
(dist->pict (boolean-dist 0.3))
]}

@defstruct*[dirichlet-dist
            ([alpha (vectorof (>/c 0))])]{

Represents a @wiki["Dirichlet_distribution"]{Dirichlet distribution}.
The support consists of vectors of the same length as @racket[alpha]
whose elements are nonnegative reals summing to @racket[1.0].

@examples[#:eval the-eval
(dist-sample (dirichlet-dist (vector 2 5 1)))
]}

@defstruct*[multinomial-dist
            ([n exact-nonnegative-integer?]
             [weights (vectorof (>=/c 0))])]{

Represents a @wiki["Multinomial_distribution"]{multinomial
distribution}. The support consists of vectors of the same length as
@racket[weights] representing counts of @racket[n] iterated samples
from the corresponding categorical distribution with @racket[weights]
for weights.

@examples[#:eval the-eval
(dist-sample (multinomial-dist 100 (vector 1/2 1/3 1/6)))
]}


@; ------------------------------------------------------------
@section[#:tag "dist-monad"]{Finite Distributions as a Monad}

The following operations do not apply normalization to their results.

@defproc[(dist-unit [v any/c]) discrete-dist?]{

Returns a distribution with all probability mass concentrated on
@racket[v].

@examples[#:eval the-eval
(dist-unit 'apple)
]}

@defproc[(dist-bind [d finite-dist?]
                    [f (-> any/c finite-dist?)])
         discrete-dist?]{

Given a distribution @racket[d] for random variable @italic{A} and a
probability kernel @racket[f] for @italic{B given A}, forms the joint
probability for @italic{(A,B)}, then marginalizes out @italic{A},
returning the marginal distribution for @italic{B}.

@examples[#:eval the-eval
(code:comment "probability of Raining")
(define raining-dist (boolean-dist 1/4))
(code:comment "conditional probability of Ground Wet given Raining")
(define (ground-wet raining)
  (case raining
    [(#t) (boolean-dist 19/20)]
    [(#f) (boolean-dist 2/10)]))
(code:comment "marginal probability of Ground Wet")
(dist-bind raining-dist ground-wet)
]}

@defproc[(dist-bindx [d finite-dist?]
                     [f (-> any/c finite-dist?)])
         discrete-dist?]{

Like @racket[dist-bind], but omits the marginalization step, returning
the joint distribution.

Equivalent to
@racket[(dist-bind d (λ (_v1) (dist-fmap (f _v1) (λ (_v2) (list _v1 _v2)))))].

@examples[#:eval the-eval
(code:comment "joint distribution of (Raining, Ground Wet)")
(dist-bindx raining-dist ground-wet)
]}

@defproc[(dist-fmap [d finite-dist?]
                    [f (-> any/c any/c)])
         discrete-dist?]{

Equivalent to @racket[(dist-bind d (λ (_v) (dist-unit (f _v))))].
}

@defproc[(dist-rescore [d finite-dist?]
                       [scale (-> any/c (>=/c 0))
                              (lambda (_v) (/ (dist-total-measure d)))])
         discrete-dist?]{

Adjusts the weight of every value @racket[_v] in @racket[d], multiplying it by
@racket[(scale _v)]. If @racket[scale] is not given, it defaults to a function
that divides by the the total probability mass of @racket[d], producing a
normalized distribution.
}

@defproc[(dist-join [d (discrete-distof finite-dist?)])
         discrete-dist?]{

Equivalent to @racket[(dist-bind d dist-unit)].

@examples[#:eval the-eval
(code:comment "Ground Wet as a mixture of distributions")
(define groundwet-mixture
  (discrete-dist
   [(boolean-dist 0.95) 0.25]
   [(boolean-dist 0.2) 0.75]))
(dist-join groundwet-mixture)
]}


@; ------------------------------------------------------------
@(close-eval the-eval)
