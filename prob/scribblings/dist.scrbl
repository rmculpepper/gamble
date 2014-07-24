;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     prob))

@(define the-eval (make-base-eval))
@(the-eval '(require prob (only-in prob/viz [hist-pict hist])))

@title[#:tag "dist"]{Probability Distributions}

@section[#:tag "dist-ops"]{Distribution Operations}

@defproc[(dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object,
@racket[#f] otherwise.
}

@defproc[(integer-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose
support is intrinsically integer-valued, @racket[#f] otherwise.

A discrete distribution whose values happen to be integers is
@emph{not} considered an integer distribution.
}

@defproc[(real-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose
support is intrinsically real-valued, @racket[#f] otherwise.

A discrete distribution whose values happen to be real numbers is
@emph{not} considered a real distribution. A distribution whose values
consist of real vectors, such as a Dirichlet distribution, is @emph{not}
considered real-valued.
}

@defproc[(finite-dist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a distribution object whose
support is finite, @racket[#f] otherwise.
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

Produces a sample distributed according to @racket[d].

@emph{Do no use @racket[dist-sample] within a sampler/solver; use
@racket[sample] instead.}
}

@;{
;; FIXME
@defproc[(dist-enum [d dist?])
         ...]
@defproc[(dist-support [d dist?])
         ...]
}

@deftogether[[
@defproc[(dist-mean [d dist?])
         (or/c any/c +nan.0 #f)]
@defproc[(dist-median [d dist?])
         (or/c any/c +nan.0 #f)]
@defproc[(dist-mode [d dist?])
         (or/c any/c +nan.0 #f)]
@defproc[(dist-variance [d dist?])
         (or/c any/c +nan.0 #f)]
]]{

Returns the mean, median, mode, or variance of the distribution
@racket[d], respectively.

If the distribution is integer-valued or real-valued, the statistic is
a real number. Other kinds of distribution may have other types for
these statistics.

A return value of @racket[+nan.0] indicates that the statistic is
known to be undefined.

A return value of @racket[#f] indicates that the statistic is unknown;
it may not be defined, it may be infinite, or the calculation simply
might not be implemented.
}

@defproc[(dist-energy [d dist?] [x any/c]) any/c]{

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
         any/c]{

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

@section{Integer Distribution Types}

@defstruct*[bernoulli-dist
            ([p (real-in 0 1)])]

@defstruct*[binomial-dist
            ([n exact-positive-integer?]
             [p (real-in 0 1)])]

@defstruct*[geometric-dist
            ([p (real-in 0 1)])]

@defstruct*[poisson-dist
            ([mean (>/c 0)])]

@section{Real Distribution Types}

@defstruct*[beta-dist
            ([a (>=/c 0)]
             [b (>=/c 0)])]

@defstruct*[cauchy-dist
            ([mode real?]
             [scale (>/c 0)])]

@defstruct*[exponential-dist
            ([mean (>/c 0)])]

@defstruct*[gamma-dist
            ([shape (>/c 0)]
             [scale (>/c 0)])]

@defstruct*[inverse-gamma-dist
            ([shape (>/c 0)]
             [scale (>/c 0)])]

@defstruct*[logistic-dist
            ([mean real?]
             [scale (>/c 0)])]

@defstruct*[normal-dist
            ([mean real?]
             [stddev (>/c 0)])]

@defstruct*[uniform-dist
            ([min real?]
             [max real?])]

@defstruct*[categorical-dist
            ([weights (vectorof (>=/c 0))])]

@defstruct*[dirichlet-dist
            ([alpha (vectorof (>/c 0))])]

@defstruct*[pareto-dist
            ([scale (>/c 0)]
             [shape (>/c 0)])]

@section{Discrete Distribution Type}

@defproc[(discrete-dist? [v any/c]) boolean?]

@defform[(discrete-dist maybe-normalize
           [value-expr weight-expr] ...)
         #:contracts ([weight-expr (>=/c 0)])]

@defproc[(make-discrete-dist [weighted-values dict?]
                             [#:normalize? normalize? any/c #t])
         discrete-dist?]

@defproc[(make-discrete-dist* [values vector?]
                              [weights (vectorof (>=/c 0)) (vector 1 ...)]
                              [#:normalize? normalize? any/c #t])
         discrete-dist?]

@defproc[(normalize-discrete-dist [d discrete-dist?])
         discrete-dist?]

@defproc[(discrete-dist-values [d discrete-dist?])
         vector?]

@(close-eval the-eval)
