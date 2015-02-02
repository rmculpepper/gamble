;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     (except-in gamble max min)))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble (only-in gamble/viz [hist-pict hist])))

@(define (wiki url-suffix . pre-content)
   (apply hyperlink (string-append "http://en.wikipedia.org/wiki/" url-suffix)
   	  pre-content))

@title[#:tag "dist"]{Probability Distributions}

This section describes the distribution types and operations supported
by @racketmodname[gamble].

@section[#:tag "dist-ops"]{Distribution Operations}

@defproc[(dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object,
@racket[#f] otherwise.
}

@defproc[(integer-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose
support is intrinsically integer-valued, @racket[#f] otherwise.

The distribution types for which @racket[integer-dist?] returns true
consist of exactly the ones listed in @secref["integer-dists"]. A
discrete distribution whose values happen to be integers is not
considered an integer distribution.
}

@defproc[(finite-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose
support is finite, @racket[#f] otherwise.

The distribution types for which @racket[finite-dist?] returns true
consist of exactly the ones listed in @secref["integer-dists"] and
@secref["discrete-dist"].
}

@defproc[(real-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose
support is a real interval, @racket[#f] otherwise.

The distribution types for which @racket[real-dist?] returns true
consist of exactly the ones listed in @secref["real-dists"].  A
discrete distribution whose values happen to be real numbers is
@emph{not} considered a real distribution. A distribution whose values
consist of real vectors, such as a Dirichlet distribution, is
@emph{not} considered real-valued.
}

@defproc[(dist-pdf [d dist?] [v any/c] [log? any/c #f]) 
         real?]{

Returns the probability density (or mass, as appropriate) of the value
@racket[v] in distribution @racket[d]. If @racket[log?] is true, the
log density (or log mass) is returned instead.
}

@defproc[(dist-cdf [d dist?] [v any/c] [log? any/c #f] [1-p? any/c #f]) 
         real?]{

Returns the cumulative probability density (or mass, as appropriate)
of the value @racket[v] in distribution @racket[d]---that is, the
probability that a random variable @racket[_X] distributed as
@racket[d] satisfies @racket[(<= _X v)]. If @racket[1-p?] is true,
then the probability of @racket[(> _X v)] is returned instead.

If @racket[log?] is true, then the log probability is returned
instead of the probability.

This function applies only to integer-valued (@racket[integer-dist?])
and real-valued (@racket[real-dist?]) distributions.
}

@defproc[(dist-inv-cdf [d dist?] [p (real-in 0 1)] [log? any/c #f] [1-p? any/c #f])
         any/c]{

Returns the inverse of the CDF of @racket[d] at @racket[p]. If
@racket[log?] is true, then the inverse at @racket[(exp p)] is used
instead. If @racket[1-p?] is true, then the inverse at @racket[(- 1
p)] is used instead.
}

@defproc[(dist-sample [d dist?]) any/c]{

Produces a sample distributed according to @racket[d]. This is an
@emph{unmanaged} stochastic effect, like calling Racket's
@racket[random] function.

@emph{Do not use @racket[dist-sample] within a sampler/solver; use
@racket[sample] instead.}
}

@;{
;; FIXME
@defproc[(dist-enum [d dist?])
         ...]
@defproc[(dist-support [d dist?])
         ...]
}

@defproc[(in-dist [d finite-dist?]) sequence?]{

Returns a @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{sequence} where each element
consists of two values: a value from the support of the distribution
and its probability mass.

@examples[#:eval the-eval
(for ([(v p) (in-dist (bernoulli-dist 1/3))])
  (printf "Result ~s has probability ~s.\n" v p))
]
}

@deftogether[[
@defproc[(dist-mean [d dist?])
         (or/c any/c +nan.0 #f)]
@defproc[(dist-median [d dist?])
         (or/c any/c +nan.0 #f)]
@defproc[(dist-variance [d dist?])
         (or/c any/c +nan.0 #f)]
]]{

Returns the mean, median, or variance of the distribution @racket[d],
respectively.

If the distribution is integer-valued or real-valued, the statistic is
a real number. Other kinds of distribution may have other types for
these statistics.

A return value of @racket[+nan.0] indicates that the statistic is
known to be undefined.

A return value of @racket[#f] indicates that the statistic is unknown;
it may not be defined, it may be infinite, or the calculation might
not be implemented.
}

@defproc[(dist-modes [d dist?])
         (or/c list? #f)]{

Returns the modes of the distribution @racket[d].

A return value of @racket['()] indicates that the distribution has no
mode. A return value of @racket[#f] indicates that the statistic is
unknown; it may not be defined, it may be infinite, or the calculation
might not be implemented.
}

@defproc[(dist-energy [d dist?] [x any/c]) real?]{

Returns the value of the ``energy'' function of @racket[d] evaluated
at @racket[x]. Minimizing energy is equivalent to maximizing likelihood.

Equivalent to @racket[(- (log (dist-pdf d x)))].

@examples[#:eval the-eval
(define N (normal-dist 0 1))
(dist-energy N 5)
(dist-energy N 1)
(dist-energy N 0)
(dist-energy N -1)
]
}

@defproc[(dist-Denergy [d dist?] 
                       [x real?]
                       [dx/dt real? 1]
                       [dparam/dt real? 0] ...)
         real?]{

Returns the value at @racket[x] of the derivative of @racket[d]'s
energy function. 

@examples[#:eval the-eval
(define N (normal-dist 0 1))
(dist-Denergy N 5)
(dist-Denergy N 1)
(dist-Denergy N 0)
(dist-Denergy N -1)
]

The parameters of @racket[d] and the position @racket[x] are
considered functions of a hypothetical variable @racket[_t], and the
derivative is taken with respect to @racket[_t]. Thus by varying
@racket[dx/dt] and the @racket[dparam/dt]s, mixtures of the partial
derivatives of energy with respect to the distribution's parameters
can be recovered.

@examples[#:eval the-eval
(define N (normal-dist 0 1))
(code:line (dist-Denergy N 5 1 0 0) (code:comment "∂energy/∂x"))
(code:line (dist-Denergy N 5 0 1 0) (code:comment "∂energy/∂μ; μ is 1st param of normal-dist"))
(code:line (dist-Denergy N 5 0 0 1) (code:comment "∂energy/∂σ; σ is 2nd param of normal-dist"))
]

If the derivative is not defined (such as for non-continuous
distributions) or not implemented for distribution @racket[d], an
exception is raised.
}

@defproc[(dist-update-prior [prior dist?] [dist-pattern any/c] [data vector?])
         (or/c dist? #f)]{

Returns a distribution representing a closed-form solution to Bayes'
Law applied to a distribution matching @racket[dist-pattern] whose
parameter is distributed according to @racket[prior] and incorporating
@racket[data] as evidence. The result is a distribution of the same
type as @racket[prior]. If @racket[prior] is not a (known, implemented)
conjugate prior for @racket[dist-pattern], @racket[#f] is returned.

The @racket[dist-pattern] is an S-expression consisting of a
distribution type name (a symbol) and the distribution parameters,
where the parameter distributed according to @racket[prior] is
indicated by the symbol @racket['_].

For example, if the prior of a Bernoulli distribution's success
probability is @racket[(beta-dist 1 1)], and three successes are
observed, the posterior can be calculated with:
@interaction[#:eval the-eval
(dist-update-prior (beta-dist 1 1) '(bernoulli-dist _) (vector 1 1 1))
]
If one success and two failures were observed, the posterior is
@interaction[#:eval the-eval
(dist-update-prior (beta-dist 1 1) '(bernoulli-dist _) (vector 1 0 0))
]
Here is another example where the mean of a normal distribution is
also normally distributed, but the standard deviation is fixed.
@interaction[#:eval the-eval
(dist-update-prior (normal-dist 10 1) '(normal-dist _ 1) (vector 9))
(dist-update-prior (normal-dist 10 1) '(normal-dist _ 0.5) (vector 9))
]
}


@section[#:tag "integer-dists"]{Integer Distribution Types}

@defstruct*[bernoulli-dist
            ([p (real-in 0 1)])]{

Represents a @wiki["Bernoulli_distribution"]{Bernoulli distribution}
with success probability @racket[p].}

@defstruct*[binomial-dist
            ([n exact-positive-integer?]
             [p (real-in 0 1)])]{

Represents a @wiki["Binomial_distribution"]{binomial distribution} of
@racket[n] trials each with success probability @racket[p].}

@defstruct*[categorical-dist
            ([weights (vectorof (>=/c 0))])]{

Represents a @wiki["Categorical_distribution"]{categorical
distribution} (sometimes called a discrete distribution, multinomial
distribution, or multinoulli distribution).}

@defstruct*[geometric-dist
            ([p (real-in 0 1)])]{

Represents a @wiki["Geometric_distribution"]{geometric distribution}.}

@defstruct*[poisson-dist
            ([mean (>/c 0)])]{

Represents a @wiki["Poisson_distribution"]{Poisson distribution} with
mean @racket[mean].}


@section[#:tag "real-dists"]{Real Distribution Types}

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

@defstruct*[inverse-gamma-dist
            ([shape (>/c 0)]
             [scale (>/c 0)])]{

Represents an @wiki["Inverse_gamma_distribution"]{inverse-gamma
distribution}, the reciprocals of whose values are distributed
according to @racket[(gamma-dist shape scale)].}

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

@defstruct*[uniform-dist
            ([min real?]
             [max real?])]{

Represents a @wiki["Uniform_distribution"]{uniform distribution} with
lower bound @racket[min] and upper bound @racket[max].}


@section[#:tag "vector-dists"]{Vector Distribution Types}

@defstruct*[dirichlet-dist
            ([alpha (vectorof (>/c 0))])]{

Represents a @wiki["Dirichlet_distribution"]{Dirichlet distribution}.
The support consists of vectors of the same length as @racket[alpha]
whose elements are nonnegative reals summing to @racket[1].
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

@defstruct*[permutation-dist
            ([n exact-nonnegative-integer?])]{

Returns a uniform distribution over permutations of @racket[n]
elements, where a permutation is represented by a vector containing
each of the integers from @racket[0] to @racket[(sub1 n)] exactly
once.
}


@section[#:tag "multi-dists"]{Multivariate Distribution Types}

@defstruct*[multi-normal-dist
            ([mean col-matrix?]
             [cov square-matrix?])]{

Represents a @wiki["Multivariate_normal_distribution"]{multi-variate
normal (Gaussian) distribution}. The covariance matrix @racket[cov]
must be a square, symmetric, positive-definite matrix with as many
rows as @racket[mean].

The support consists of column matrices having the same shape as
@racket[mean].
}

@defstruct*[wishart-dist
            ([n real?]
             [V square-matrix?])]{

Represents a @wiki["Wishart_distribution"]{Wishart distribution} with
@racket[n] degrees of freedom and scale matrix @racket[_V]. The scale
matrix @racket[V] must be a square, symmetric, positive-definite
matrix.

The support consists of square, symmetric, positive-definite matrices
having the same shape as @racket[V].
}

@defstruct*[inverse-wishart-dist
            ([n real?]
             [Vinv square-matrix?])]{

Represents a @wiki["Inverse-Wishart_distribution"]{Inverse-Wishart
distribution} with @racket[n] degrees of freedom and scale matrix
@racket[Vinv]. The scale matrix @racket[Vinv] must be a square,
symmetric, positive-definite matrix.

If @racket[X] is distributed according to @racket[(wishart-dist n V)],
then @racket[(matrix-inverse X)] is distributed according to
@racket[(inverse-wishart-dist n (matrix-inverse V))].

The support consists of square, symmetric, positive-definite matrices
having the same shape as @racket[Vinv].
}


@section[#:tag "discrete-dist"]{Discrete Distribution Type}

A discrete distribution is a distribution whose support is a finite
collection of arbitrary Racket values. Note: this library calls
@racket[categorical-dist] a distribution whose support consists of the
integers {0, 1, ..., @italic{N}}.

The elements of a discrete distribution are distinguished using
@racket[equal?]. The constructors for discrete distributions detect
and coalesce duplicates.

@defproc[(discrete-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a discrete distribution,
@racket[#f] otherwise.
}

@defform[(discrete-dist maybe-normalize
           [value-expr weight-expr] ...)
         #:grammar ([maybe-normalize (code:line)
	 	   		     (code:line #:normalize? normalize?-expr)])
         #:contracts ([weight-expr (>=/c 0)])]{

Produces a discrete distribution whose values are the
@racket[value-expr]s and whose probability masses are the
corresponding @racket[weight-expr]s.

Normalization affects printing and @racket[discrete-dist-weights], but
not @racket[dist-pdf].

@examples[#:eval the-eval
(discrete-dist ['apple 1/2] ['orange 1/3] ['pear 1/6])
]
}

@defproc[(make-discrete-dist [weighted-values dict?]
                             [#:normalize? normalize? any/c #t])
         discrete-dist?]{

Produces a discrete distribution from the dictionary
@racket[weighted-values] that maps values to weights.

@examples[#:eval the-eval
(make-discrete-dist '((apple . 1/2) (orange . 1/3) (pear . 1/6)))
]
}

@defproc[(make-discrete-dist* [values vector?]
                              [weights (vectorof (>=/c 0)) (vector 1 ...)]
                              [#:normalize? normalize? any/c #t])
         discrete-dist?]{

Produces a discrete distribution with the values of @racket[values]
and weights of @racket[weights]. The two vectors must have equal
lengths.

@examples[#:eval the-eval
(make-discrete-dist* (vector 'apple 'orange 'pear)
                     (vector 1/2 1/3 1/6))
]
}

@defproc[(normalize-discrete-dist [d discrete-dist?])
         discrete-dist?]{

Normalizes a discrete distribution. If @racket[d] is already
normalized, the function may return @racket[d].
}

@deftogether[[
@defproc[(discrete-dist-values [d discrete-dist?])
         vector?]
@defproc[(discrete-dist-weights [d discrete-dist?])
         vector?]
]]{

Returns the values and weights of @racket[d], respectively.
}


@section[#:tag "dist-monad"]{Finite Distributions as a Monad}

@defproc[(dist-unit [v any/c]) finite-dist?]{

Returns a distribution with all probability mass concentrated on
@racket[v].
}

@defproc[(dist-bind [d finite-dist?]
                    [f (-> any/c finite-dist?)])
         finite-dist?]{

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
]
}

@defproc[(dist-bindx [d finite-dist?]
                     [f (-> any/c finite-dist?)])
         finite-dist?]{

Like @racket[dist-bind], but omits the marginalization step, returning
the joint distribution.

Equivalent to @racket[(dist-bind d (λ (v1) (dist-fmap (f v1) (λ (v2) (list v1 v2)))))].

@examples[#:eval the-eval
(code:comment "joint distribution of (Raining, Ground Wet)")
(dist-bindx raining-dist ground-wet)
]
}

@defproc[(dist-fmap [d finite-dist?]
                    [f (-> any/c any/c)])
         finite-dist?]{

Equivalent to @racket[(dist-bind d (compose dist-unit f))].
}

@defproc[(dist-filter [d finite-dist?]
                      [pred (-> any/c boolean?)]
                      [empty any/c (lambda () (error ....))])
         finite-dist?]{

Returns a distribution like @racket[d] but whose support is narrowed
to values accepted by the predicate @racket[pred].

If @racket[pred] does not accept any values in the support of
@racket[d], @racket[empty] is called, if it is a procedure, or
returned otherwise.

@examples[#:eval the-eval
(dist-filter (binomial-dist 10 1/2) even?)
(dist-filter (binomial-dist 10 1/2) negative?)
(dist-filter (binomial-dist 10 1/2) negative? #f)
]
}


@section[#:tag "dist-define"]{Defining New Probability Distributions}

@defform[(define-dist-type dist-name ([field-id field-ctc] ...)
           #:pdf pdf-fun
           #:sample sample-fun
           dist-option ...)
         #:grammar
         ([dist-option (code:line #:cdf cdf-fun)
                       (code:line #:inv-cdf inv-cdf-fun)
                       (code:line #:guard guard-fun)
                       (code:line #:extra [struct-option ...])
                       (code:line #:no-provide)])
         #:contracts
         ([field-ctc contract?]
          [pdf-fun (field-ctc ... any/c boolean? . -> . real?)]
          [cdf-fun (field-ctc ... any/c boolean? boolean? . -> . real?)]
          [inv-cdf-fun (field-ctc ... real? boolean? boolean? . -> . any/c)]
          [sample-fun (field-ctc ... . -> . any/c)])]{

Defines @racket[dist-name] as a new distribution type. In particular,
@racket[dist-name] is a struct implementing the internal generic
interface representing distributions.

At a minimum, the new distribution type must supply a probability
density/mass function (@racket[pdf-fun]) and a sampling function
(@racket[sample-fun]). The type may also supply a cumulative
probability function (@racket[cdf-fun]) and inverse
(@racket[inv-cdf-fun]). The signatures of the functions are like
@racket[dist-pdf], @racket[dist-sample], etc, but instead of the
distribution itself, they accept the fields of the distribution as the
initial arguments.

The @racket[guard-fun] is a struct guard function; see
@racket[make-struct-type] for details. Other struct options may be
passed via the @racket[#:extra] option. Distribution types are
automatically transparent.

By default, @racket[dist-name], @racket[_dist-name?], and the
synthesized accessors are provided with the @racket[field-ctc]
contracts. The @racket[#:no-provide] option disables automatic
providing, in which case the @racket[field-ctc]s are ignored.
}


@(close-eval the-eval)
