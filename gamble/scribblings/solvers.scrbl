;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base racket/contract gamble gamble/pict))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble gamble/pict))
@(the-eval '(random-seed 1))

@title[#:tag "solvers"]{Samplers and Solvers}

A @deftech{sampler} is an object that contains a probabilistic model
and produces samples from the posterior distribution of its result
expression. Samplers can be either unweighted or weighted.

The following samplers are supported:
@itemlist[

@item{@racket[rejection-sampler] --- unweighted sampler, does not support
observations}

@item{@racket[importance-sampler] --- weighted sampler}

@item{@racket[mcmc-sampler] --- unweighted sampler, uses Markov-chain Monte
Carlo (MCMC) methods to sample from target distribution}

]

Aside from @tech{samplers}, there are other @deftech{solvers} that can
extract information from a probabilistic model. The following solvers
are supported:
@itemlist[

@item{@racket[enumerate] --- exhaustive enumeration, exponential in number of
random variables, cannot handle sampling from continuous random variables}

]

@section[#:tag "sampler-funs"]{Basic Sampler Functions}

@defproc[(sampler? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{sampler}, @racket[#f]
otherwise.
}

@defproc[(weighted-sampler? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{weighted sampler},
@racket[#f] otherwise.

Every @tech{sampler} is also a @tech{weighted sampler}; the samples it
produces always have weight @racket[1].
}

@defproc[(generate-samples [s weighted-sampler?]
                           [n exact-nonnegative-integer?]
                           [#:burn burn exact-nonnegative-integer? 0]
                           [#:thin thin exact-nonnegative-integer? 0])
         (vectorof any/c)]{

Generates @racket[n] samples from the sampler @racket[s]. The sampler is first
called @racket[burn] times and the results are discarded. In addition, the
sampler is called @racket[thin] times before every sample to be retained.
}

@defproc[(generate-weighted-samples [s weighted-sampler?] 
                                    [n exact-nonnegative-integer?]
                                    [#:burn burn exact-nonnegative-integer? 0]
                                    [#:thin thin exact-nonnegative-integer? 0])
         (values (vectorof any/c) (vectorof (>/c 0)))]{

Generates @racket[n] weighted samples from the @tech{weighted sampler}
@racket[s] passed through the optional function @racket[f]. The
weighted samples are returned as a vector of value-weight pairs.
}

@defproc[(sampler->discrete-dist [sampler weighted-sampler?]
                                 [n exact-positive-integer?]
                                 [#:burn burn exact-nonnegative-integer? 0]
                                 [#:thin thin exact-nonnegative-integer? 0])
         discrete-dist?]{

Returns the empirical distribution obtained by generating @racket[n] samples
from @racket[sampler].
}


@; ============================================================
@section[#:tag "samplers-basic"]{Basic Sampler Forms}

@defproc[(rejection-sampler [m model?]) sampler?]{

Produces a @tech{sampler} that, when applied, returns a value of
@racket[result-expr] arising from an execution where all observations
in the body are satisfied.

The sampler is implemented using rejection sampling---specifically,
``logic sampling''---for discrete random choices. The rejection
sampler can sample continuous random variables, but it cannot perform
observations (@racket[dscore], @racket[observe]) on them.

@examples[#:eval the-eval
(define s-A-given-AorB
  (rejection-sampler
    (lambda ()
      (define A (sample (boolean-dist 1/2)))
      (define B (sample (boolean-dist 1/2)))
      (unless (or A B) (fail))
      A)))
(sampler->discrete-dist s-A-given-AorB 100)

(define rs-count-heads
  (rejection-sampler
    (lambda () (count-heads 10))))
(sampler->discrete-dist rs-count-heads 10)
]}

@defproc[(importance-sampler [m model?]
                             [#:propose propose (or/c #f (-> any/c dist? dist?)) #f])
         weighted-sampler?]{

Like @racket[rejection-sampler], but returns a @emph{weighted sampler}
that uses weights to represent the quality of a particular sample
given the observations in the program. Thus unlike a rejection
sampler, an importance sampler can handle observations
(@racket[observe-sample]) on continuous random variables.
}


@; ------------------------------------------------------------
@section[#:tag "mcmc-sampler"]{MCMC Sampler and Transitions}

Metropolis-Hastings (MH) is an algorithm framework for producing a
correlated sequence of samples where each sample is based on the
previous. The algorithm is parameterized by the mechanism for
proposing a new state given the previous state; given a proposal, the
MH algorithm accepts or rejects it based on how the proposal was
generated and the relative likelihood of the proposed state.

The @racket[mcmc-sampler] function implements the MCMC framework, and the
proposal mechanisms are implemented by a variety of @deftech{MCMC transition}
types.

@defproc[(mcmc-sampler [f (-> any/c)]) sampler?]{

Returns a @tech{sampler} that produces samples using a variant of MCMC.

The @racket[transition-expr] determines the mechanism used to propose
new states. If absent, @racket[(single-site)] is used.

@;{
@examples[#:eval the-eval
(define mh-or
  (mh-sampler
    (define A (flip))
    (define B (flip))
    (observe/fail (or A B))
    A))
(hist (repeat mh-or 100))

(define mh-n-flips
  (mh-sampler
   (count-heads 10)))
(parameterize ((verbose? #t))
  (mh-n-flips))
(parameterize ((verbose? #t))
  (mh-n-flips))
(hist (repeat mh-n-flips 100))
(hist (repeat mh-n-flips 2000))
]
}
}

@; ----------------------------------------
@subsection[#:tag "mcmc-transitions"]{MCMC Transitions}

@defproc[(mcmc-transition? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents a @tech{MCMC transition},
@racket[#f] otherwise.
}

@defproc[(single-site-transition [#:proposal proposal proposal?]
                                 [#:any candidate? (or/c #f (-> any/c dist? boolean?)) #f])
         mcmc-transition?]{

A transition that proposes a new state by randomly (uniformly) selecting a
single random choice matching @racket[candidate?] and perturbing it according to
@racket[proposal] (see @secref["mh-proposals"]). If @racket[candidate?] is
@racket[#f], then all random choices are considered.

If @racket[proposal] fails to propose a new value for a specific random choice,
the random choice is resampled as a fallback (see @racket[resample-proposal]).
}

@defproc[(multi-site-transition [#:proposal proposal proposal?]
                                [#:all candidate? (or/c #f (-> any/c dist? boolean?)) #f])
         mcmc-transition?]{

A transition that proposes a new state by perturbing @emph{all} random
choices matching @racket[candidate?] according to @racket[proposal].

If @racket[proposal] fails to propose a new value for a specific random choice,
the random choice is resampled as a fallback (see @racket[resample-proposal]).
}

@defproc[(slice-transition [#:any candidate? (or/c #f (-> any/c dist? boolean?)) #f]
                           [#:method method (or/c 'step 'double) 'double]
                           [#:W W (>/c 0) 1.0]
                           [#:Wi Wi exact-positive-integer? 1]
                           [#:M M (or/c exact-positive-integer? +inf.0) +inf.0]
                           [#:small-dist-limit small-dist-limit exact-nonnegative-integer? 10])
         mcmc-transition?]{

A transition that picks a new state via slice sampling @;{FIXME: need reference}
on a single random choice selected randomly from those matching
@racket[candidate?]. The @racket[method] argument selects the technique for
finding the slice's interval to sample from, either stepping out or doubling.
The @racket[W] and @racket[Wi] arguments control the ``stepping out'' width
parameter used to find the slice bounds for continuous real distributions and
integer distributions, respectively. At most @racket[M] interval-widening steps
are performed. If the selected choice's distribution is finite with
@racket[small-dist-limit] elements or fewer, then the entire support is used.
}

@defproc[(enumerative-gibbs [#:any candidate? (or/c #f (-> any/c dist? boolean?)) #f])
         mcmc-transition?]{

A transition that chooses a single random choice from those matching
@racket[candidate?] and resamples it from its full conditional probability
distribution given the values of all of the other choices in the program. The
distribution of the choice to be perturbed must be finite; otherwise, an error
is raised. The choice to be perturbed must be @emph{non-structural}---that is,
its value must not determine whether subsequent choices are made or
not---otherwise, an error is raised.

Note: unlike traditional Gibbs sampling, this transition picks a
choice at random rather than perturbing all choices round-robin.
}

@defproc[(initialize-transition [get-value (-> any/c dist? (or/c #f (list/c any/c)))
                                           (lambda (addr dist) #f)]
                                [#:hash value-hash (or/c #f hash?) #f])
         mcmc-transition?]{

A transition that reruns the progam, setting random choices to their values
according to either @racket[value-hash] or @racket[get-value]. If neither
specifies a value for a random choice, its previous value is used if there is
one, otherwise it is sampled from the prior. The transition always accepts if
the execution's likelihood is nonzero. Use @racket[initialize-transition] to
initialize an MCMC sampler when random initialization is infeasible.
}


@; ----------------------------------------
@subsection[#:tag "mh-proposals"]{Metropolis-Hastings Proposals}

A @racket[single-site-transition] or @racket[multi-site-transition] is
parameterized by the proposal distribution used to perturb a single random
choice.

@defproc[(proposal? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a proposal object, @racket[#f]
otherwise.
}

@defproc[(proposal [#:propose1 propose1
                               (or/c #f (-> any/c dist? any/c
                                            (or/c (cons/c any/c real?) proposal?)))
                               #f]
                   [#:propose2 propose2
                               (or/c #f (-> any/c dist? dist? any/c
                                            (or/c (cons/c any/c real?) proposal?)))
                               #f]
                   [#:propose-dist propose-dist
                                   (or/c #f (-> any/c dist? any/c (or/c dist? #f)))
                                   #f])
         proposal?]{

Produces a proposal. The three callbacks are used as follows:
@itemlist[

@item{@racket[(propose1 _addr _dist _prev-value)]
--- Used by @racket[single-site-transition] to change the value of a single
random choice with address @racket[_addr] and distribution @racket[_dist]. The
value of the random choice on the previous execution was
@racket[_prev-value].

The function should return a value @racket[(cons _new-value _log-R/F)], where
@racket[_log-R/F] is the proposal's part of the Metropolis-Hastings acceptance
ratio. The function can also return @racket[#f] or another proposal object; on
@racket[#f] the result of @racket[(propose2 _addr _dist _dist _prev-value)] is
used instead; otherwise the operation is repeated on the returned proposal.}

@item{@racket[(propose2 _addr _new-dist _prev-dist _prev-value)]
--- Used by @racket[multi-site-transition] to change the value of multiple
random choices simultaneously. Since changing the value of one random choice may
change the distribution parameters of another random choice, the new value must
be chosen during program re-execution, and both the previous and current
distributions are available.

The function should return a value @racket[(cons _new-value _log-R/F)], where
@racket[_log-R/F] is the proposal's part of the Metropolis-Hastings acceptance
ratio. The function can also return @racket[#f] or another proposal object; on
@racket[#f] the @racket[propose-dist] method is tried instead; otherwise the
operation is repeated on the returned proposal.}

@item{@racket[(proposal-dist _addr _dist _value)]
--- Used as a fallback by the other proposal methods. If used, the function is
called twice: once to sample @racket[_new-value] from the distribution based on
@racket[_prev-value], and once more to assess the likelihood of
@racket[_prev-value] from the distribution based on @racket[_new-value].}

]
A proposal function must not return @racket[#f] or a proposal for some values
but not others; otherwise the probability of chaining to other proposal
functions would need to be accounted for in the MH acceptance ratio, and it is
not.
}

@defproc[(resample-proposal) proposal?]{

Returns a proposal that chooses the new value of a variable by simply
resampling from the variable's prior distribution.
}

@defproc[(drift-proposal [#:params? params? boolean? #t]
                         [#:scale scale-factor
                                  (or/c (>/c 0) (-> addr? dist? (>/c 0)))
                                  1.0])
         proposal?]{

Returns a proposal that attempts to choose the new value of a variable by
choosing a value near the current value. Not all distribution types have an
associated drift kernel.

If @racket[params?] is true, then the scale of the drift kernel depends both on
the @racket[scale] argument and the parameters of the random choice's
distribution; otherwise, it depends only on the @racket[scale] argument.
}


@; ============================================================
@section[#:tag "particles"]{Particle Filters}

A @deftech{particle set} consists of a collection of particles, each
of which contains a current state estimate and a likelihood
weight. Particle sets are updated via a (stochastic) state transformer
that produces a new state for each particle. Observations performed by
the state transformer adjust the particles' weights.


@defproc[(particles? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{particle set},
@racket[#f] otherwise.
}

@defproc[(make-particles [n exact-nonnegative-integer?]
                         [init-state any/c])
         particles?]{

Returns a particle set with @racket[n] particles, each with state
@racket[init-state] and weight @racket[1].
}

@defproc[(particles-count [ps particles?]) exact-nonnegative-integer?]{

Returns the number of particles in @racket[ps].
}

@defproc[(particles-update [ps particles?]
                           [update-state (-> any/c any/c)])
         particles?]{

Produces a new particle set where each particle corresponds to a
particle in @racket[ps], where each new particle's state is the result
of applying @racket[update-state] to the old state. Each new particle's
weight is the old particle's weight adjusted by observations performed
by @racket[update-state].
}

@defproc[(particles-score [ps particles?]
                          [score-state (-> any/c any)])
         particles?]{

Like @racket[particles-update], but the result of the
@racket[score-state] function is ignored and the state of each
particle is unchanged. Observations performed by @racket[score-state]
still affect the new particles' weights.

Equivalent to @racket[(particles-update ps (lambda (st) (score-state st) st))].
}

@defproc[(particles-resample [ps particles?]
                             [n exact-nonnegative-integer? (particles-count ps)]
                             [#:alg algorithm (or/c 'multinomial 'residual #f)
                              'multinomial])
         particles?]{

Produces a new particle set by resampling particles from
@racket[ps]. Every particle in the new particle set has weight
@racket[1]. See also @racket[resample].
}

@deftogether[[
@defproc[(particles-effective-count [ps particles?]) real?]
@defproc[(particles-effective-ratio [ps particles?]) real?]
]]{

Returns an estimate of the effective sample size and its ratio to the
number of particles, respectively.
}

@defproc[(particles-weighted-states [ps particles?])
         (vectorof (cons/c any/c (>/c 0)))]{

Returns a vector of the particle states and weights from
@racket[ps]. Particles with zero weight are omitted, so the length of
the vector may be less than @racket[(particle-count ps)].
}

@defproc[(particles-states [ps particles?])
         vector?]{

Returns a vector of the particle states from @racket[ps], regardless
of weight (except that particles with zero weight are omitted).

In general, it is only sensible to call this function when the weights
are known to be equal, such as after calling @racket[particles-resample].
}

@defproc[(in-particles [ps particles?])
         sequence?]{

Produces a sequence where each step produces two values: the particle
state and its weight. Particles with empty weights are omitted from
the sequence.
}


@; ============================================================
@section[#:tag "enumerate"]{Enumeration Solver}

@defproc[(enumerate [f (-> any/c)])
         discrete-dist?]{

Returns a discrete distribution of the values produced by @racket[f], weighted
by any conditioning or scoring performed by the function. The resulting discrete
distribution is @emph{not normalized}.

The @racket[enumerate] form works by exploring all possibilities using
the technique described in @cite{EPP}. Exploration ceases only when
all paths have been explored; if any path is infinite, then
@racket[enumerate] fails to terminate. Only discrete and finite
integer-valued distributions can be sampled with @racket[enumerate].

@examples[#:eval the-eval
(enumerate
  (lambda ()
    (define A (sample (boolean-dist 1/2)))
    (define B (sample (boolean-dist 1/2)))
    (unless (or A B) (fail))
    A))
]}

@(close-eval the-eval)
