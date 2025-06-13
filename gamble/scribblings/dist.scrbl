;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base racket/contract gamble))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble))
@(the-eval '(random-seed 1))

@(define (wiki url-suffix . pre-content)
   (apply hyperlink (string-append "http://en.wikipedia.org/wiki/" url-suffix)
   	  pre-content))

@title[#:tag "dist"]{Probability Distributions}

This section describes the distribution types and operations supported
by @racketmodname[gamble].

@; ------------------------------------------------------------
@section[#:tag "dist-kinds"]{Kinds of Distributions}

@defproc[(dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object, @racket[#f]
otherwise.
}

@defproc[(real-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose support is
limited to the real numbers or a subset thereof, @racket[#f]
otherwise. Real-valued dists support additional operations, such as
@racket[dist-cdf].

If @racket[(real-dist? v)] is true, then @racket[v] is either an integer
distribution (@racket[integer-dist?]) or a continuous distribution
(@racket[continuous-dist?]).
}

@defproc[(integer-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose support is
intrinsically integer-valued, @racket[#f] otherwise.

The distribution types for which @racket[integer-dist?] returns true consist of
exactly the ones listed in @secref["integer-dists"]. A discrete distribution
whose values happen to be integers is not considered an integer distribution.

If @racket[(continuous-dist? v)] is true, then @racket[(real-dist? v)] is also true.
}

@defproc[(continuous-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose support is a
real interval and whose CDF is continuous, @racket[#f] otherwise.

The distribution types for which @racket[continuous-dist?] returns true consist
of exactly the ones listed in @secref["continuous-dists"].

If @racket[(continuous-dist? v)] is true, then @racket[(real-dist? v)] is also true.
}

@defproc[(enumerable-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose theoretical
support is finite or countably infinite, @racket[#f] otherwise. See also
@racket[in-dist].
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

@emph{Do not use @racket[dist-sample] within a sampler/solver; use
@racket[sample] instead.}

@examples[#:eval the-eval
(for/list ([i 10]) (dist-sample (bernoulli-dist 1/3)))
]}

@defproc[(dist-density [d dist?] [v any/c] [log? any/c #f]) 
         density?]{

Returns the probability density of the value @racket[v] in the distribution
@racket[d]. If @racket[log?] is true, the density is represented in logspace.

@examples[#:eval the-eval
(dist-density (bernoulli-dist 1/3) 0)
(dist-density (uniform-dist 0 10) 3)
(dist-density (uniform-dist 0 10) 3 #t)
]}

@defproc[(dist-pdf [d dist?] [v any/c] [log? any/c #f]) 
         real?]{

Returns the probability density (or mass, as appropriate) of the value
@racket[v] in distribution @racket[d]. If @racket[log?] is true, the
log density (or log mass) is returned instead.

@examples[#:eval the-eval
(dist-pdf (bernoulli-dist 1/3) 0)
(dist-pdf (uniform-dist 0 10) 3)
(dist-pdf (uniform-dist 0 10) 3 #t)
]}

@defproc[(dist-cdf [d real-dist?] [v real?] [log? any/c #f] [1-p? any/c #f])
         real?]{

Returns the cumulative probability density (or mass, as appropriate) of the
value @racket[v] in distribution @racket[d]---that is, the probability that a
random variable @racket[_X] distributed according to @racket[d] satisfies
@racket[(<= _X v)].  If @racket[1-p?] is true, then the probability of
@racket[(> _X v)] is returned instead.  If @racket[log?] is true, then the log
probability is returned instead of the probability.

@examples[#:eval the-eval
(dist-cdf (uniform-dist 0 10) 3)
]}

@defproc[(dist-inv-cdf [d dist?] [p real?] [log? any/c #f] [1-p? any/c #f])
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

@; ------------------------------------------------------------
@section[#:tag "integer-dists"]{Integer Distribution Types}

@defstruct*[bernoulli-dist
            ([p probability?])]{

Represents a @wiki["Bernoulli_distribution"]{Bernoulli distribution} with
success probability @racket[p]. The distribution's support consists of
@racket[0] and @racket[1]. See also @racket[boolean-dist].
}

@defstruct*[binomial-dist
            ([n exact-positive-integer?]
             [p probability?])]{

Represents a @wiki["Binomial_distribution"]{binomial distribution} of
@racket[n] trials each with success probability @racket[p].
}

@defstruct*[categorical-dist
            ([weights (vectorof (>=/c 0))])]{

Represents a @wiki["Categorical_distribution"]{categorical
distribution} (sometimes called a discrete distribution, multinomial
distribution, or multinoulli distribution).

The distribution's support consists of the exact integers {@racket[1], ...,
@racket[_n]}, where @racket[_n] is the length of @racket[weights].
}

@defstruct*[geometric-dist
            ([p probability?])]{

Represents a @wiki["Geometric_distribution"]{geometric distribution}.
}

@defstruct*[negative-binomial-dist
            ([r exact-positive-integer?]
             [p probability?])]{

Represents a @wiki["Negative_Binomial_distribution"]{negative binomial
distribution} of the number of failures before @racket[r] successes, with
success probability @racket[p].
}

@defstruct*[poisson-dist
            ([mean (>/c 0)])]{

Represents a @wiki["Poisson_distribution"]{Poisson distribution} with
mean @racket[mean].
}


@; ------------------------------------------------------------
@section[#:tag "continuous-dists"]{Continuous Distribution Types}

@defstruct*[beta-dist
            ([a (>=/c 0)]
             [b (>=/c 0)])]{

Represents a @wiki["Beta_distribution"]{beta distribution} with shape
@racket[a] and scale @racket[b].}

@defstruct*[cauchy-dist
            ([mode real?]
             [scale (>/c 0)])]{

Represents a @wiki["Cauchy_distribution"]{Cauchy distribution} with
mode @racket[mode] and scale @racket[scale].}

@defstruct*[exponential-dist
            ([mean (>/c 0)])]{

Represents an @wiki["Exponential_distribution"]{exponential
distribution} with mean @racket[mean].

Note: A common alternative parameterization uses the rate @italic{λ} =
@racket[(/ mean)].}

@defstruct*[gamma-dist
            ([shape (>/c 0)]
             [scale (>/c 0)])]{

Represents a @wiki["Gamma_distribution"]{gamma distribution} with
shape (@italic{k}) @racket[shape] and scale (@italic{θ})
@racket[scale].

Note: A common alternative parameterization uses @italic{α}
= @racket[shape] and rate @italic{β} = @racket[(/ scale)].}

@defstruct*[logistic-dist
            ([mean real?]
             [scale (>/c 0)])]{

Represents a @wiki["Logistic_distribution"]{logistic distribution}
with mean @racket[mean] and scale @racket[scale].}

@defstruct*[normal-dist
            ([mean real?]
             [stddev (>/c 0)])]{

Represents a @wiki["Normal_distribution"]{normal (Gaussian)
distribution} with mean (@italic{μ}) @racket[mean] and standard
deviation (@italic{σ}) @racket[stddev].

Note: A common alternative parameterization uses the variance
@italic{σ@superscript{2}}.}

@defstruct*[pareto-dist
            ([scale (>/c 0)]
             [shape (>/c 0)])]{

Represents a @wiki["Pareto_distribution"]{Pareto distribution}.}

@defstruct*[student-t-dist
            ([degrees (>/c 0)]
             [mode real?]
             [scale (>/c 0)])]{

Represents a @wiki["Student's_t-distribution"]{Student's t distribution}.
}

@defstruct*[triangle-dist
            ([lo real?]
             [hi real?]
             [mode real?])]{

Represents a @wiki["Triangle_distribution"]{triangle distribution} with lower
bound @racket[lo], upper bound @racket[hi], and mode at @racket[mode].

Must satisfy @racket[(< lo hi)] and @racket[(<= lo mode hi)].
}

@defstruct*[uniform-dist
            ([lo real?]
             [hi real?])]{

Represents a @wiki["Uniform_distribution"]{uniform distribution} with
lower bound @racket[lo] and upper bound @racket[hi].

Must satisfy @racket[(< lo hi)].
}

@; ------------------------------------------------------------
@section[#:tag "dist-transformers"]{Real Distribution Transformers}

The following constructors take and produce real-valued distributions.

@defstruct*[affine-distx
            ([dist continuous-dist?]
             [a real?]
             [b real?])]{

Represents the distribution of @racket[(+ (* a _t) b)] where
@racket[_t] is distributed according to @racket[dist].

@emph{Note:} The affine transformation is applied to generated samples; its
inverse is applied when evaluating the density, CDF, etc.
}

@defstruct*[clip-distx
            ([dist continuous-dist?]
             [a real?]
             [b real?])]{

Clips @racket[dist] to the closed interval [@racket[a],
@racket[b]]. 

If the interval is small, the clipped dist is sampled
using the @racket[dist-inv-cdf] method of @racket[dist]; otherwise,
rejection sampling is used.
}

@defstruct*[exp-distx
            ([dist continuous-dist?])]{

Represents the distribution of @racket[(exp _t)] where @racket[_t] is
distributed according to @racket[dist].

@emph{Note:} The @racket[exp] in the name refers to the transformation applied
when sampling. This differs from standard terminology---for example, variables
distributed according to @racket[(exp-distx (normal-dist 0 1))] are customarily
called ``Lognormal'' variables.
}

@defstruct*[discretize/floor-distx
            ([dist continuous-dist?])]{

Produces an integer-valued distribution whose sampling process consists of
@racket[(inexact->exact (floor (dist-sample dist)))].
}

@defstruct*[discretize/round-distx
            ([dist continuous-dist?])]{

Produces an integer-valued distribution whose sampling process consists of
@racket[(inexact->exact (round (dist-sample dist)))].
}

@; ------------------------------------------------------------
@section[#:tag "other-dists"]{Other Distribution Types}

@defstruct*[boolean-dist
            ([p probability?])]{

Like a Bernoulli distribution (@racket[bernoulli-dist]), but the support
consists of the values @racket[#t] and @racket[#f].
}

@defstruct*[dirichlet-dist
            ([alpha (vectorof (>/c 0))])]{

Represents a @wiki["Dirichlet_distribution"]{Dirichlet distribution}.
The support consists of vectors of the same length as @racket[alpha]
whose elements are nonnegative reals summing to @racket[1.0].
}

@defstruct*[multinomial-dist
            ([n exact-nonnegative-integer?]
             [weights (vectorof (>=/c 0))])]{

Represents a @wiki["Multinomial_distribution"]{multinomial
distribution}. The support consists of vectors of the same length as
@racket[weights] representing counts of @racket[n] iterated samples
from the corresponding categorical distribution with @racket[weights]
for weights.
}

@; ------------------------------------------------------------
@section[#:tag "discrete-dist"]{Discrete Distribution Type}

A discrete distribution is a distribution whose support is a finite collection
of arbitrary Racket values. The elements of a discrete distribution are
distinguished using @racket[equal?]. The constructors for discrete distributions
detect and coalesce duplicates.

A discrete distribution is not required to be @emph{normalized}. That is, its
probabilities may not sum to one; the sum may also be greater or lesser. In
particular, the empty discrete distribution has zero total probability.

@defproc[(discrete-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a discrete distribution,
@racket[#f] otherwise.
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

@defproc[(hash->discrete-dist [h (hash/c any/c (>=/c 0))])
         discrete-dist?]{

Produces a discrete distribution, using @racket[h]'s keys as the distrubution's
values and @racket[h]'s values as the distribution's weights.

The total mass of the distribution is @emph{not normalized}. Use
@racket[dist-rescore] to normalize if necessary.
}

@defproc[(make-discrete-dist [values vector?]
                             [weights (or/c #f (vectorof (>=/c 0)))])
         discrete-dist?]{

Produces a discrete distribution on the elements of @racket[values]. If
@racket[weights] is a vector, it must be the same length as @racket[values], and
each element gives the probability of the corresponding value. If
@racket[weights] is @racket[#f], then every value is equally likely.

If @racket[weights] are given, the total mass of the distribution is @emph{not
normalized}. Use @racket[dist-rescore] to normalize if necessary.

@examples[#:eval the-eval
(make-discrete-dist (vector 'apple 'orange 'pear) (vector 1/2 1/3 1/6))
]}

@deftogether[[
@defproc[(discrete-dist-values [d discrete-dist?])
         vector?]
@defproc[(discrete-dist-weights [d discrete-dist?])
         vector?]
]]{

Returns the values and weights of @racket[d], respectively.
}

@; ------------------------------------------------------------
@section[#:tag "dist-monad"]{Finite Distributions as a Monad}

The following operations, despite the @litchar{dist-} in the names,
may produce @emph{unnormalized} discrete distributions.

@defproc[(dist-unit [v any/c]) discrete-dist?]{

Returns a distribution with all probability mass concentrated on
@racket[v].
}

@defproc[(dist-bind [d finite-dist?]
                    [f (-> any/c finite-dist?)])
         discrete-dist?]{

Given a distribution @racket[d] for random variable @italic{A} and a
probability kernel @racket[f] for @italic{B given A}, forms the joint
probability for @italic{(A,B)}, then marginalizes out @italic{A},
returning the marginal distribution for @italic{B}.

@examples[#:eval the-eval
(define (ground-wet raining)
  (case raining
    [(0) (bernoulli-dist 9/10)]
    [(1) (bernoulli-dist 2/10)]))
(define raining-dist (bernoulli-dist 2/10))
(code:comment "marginal probability of Ground Wet")
(dist-bind raining-dist ground-wet)
]}

@defproc[(dist-bindx [d finite-dist?]
                     [f (-> any/c finite-dist?)])
         discrete-dist?]{

Like @racket[dist-bind], but omits the marginalization step, returning
the joint distribution.

Equivalent to @racket[(dist-bind d (λ (v1) (dist-fmap (f v1) (λ (v2) (list v1 v2)))))].

@examples[#:eval the-eval
(code:comment "joint distribution of (Raining, Ground Wet)")
(dist-bindx raining-dist ground-wet)
]}

@defproc[(dist-fmap [d finite-dist?]
                    [f (-> any/c any/c)])
         discrete-dist?]{

Equivalent to @racket[(dist-bind d (compose dist-unit f))].
}

@;{
@defproc[(dist-filter [d finite-dist?]
                      [pred (-> any/c boolean?)])
         discrete-dist?]{

Returns a distribution like @racket[d] but whose support is narrowed
to values accepted by the predicate @racket[pred].

@examples[#:eval the-eval
(dist-filter (binomial-dist 10 1/2) even?)
(dist-filter (binomial-dist 10 1/2) negative?)
]}
}

@defproc[(dist-rescore [d finite-dist?]
                       [scale (>=/c 0) (/ (dist-total-measure d))])
         discrete-dist?]{

Adjusts the weight of every value in @racket[d], multiplying it by
@racket[scale]. If @racket[scale] is not given, it defaults to the total
probability mass of @racket[d], producing a normalized distribution.
}

@defproc[(dist-join [d (discrete-distof finite-dist?)])
         discrete-dist?]{

Equivalent to @racket[(dist-bind d dist-unit)].
}


@; ------------------------------------------------------------
@(close-eval the-eval)
