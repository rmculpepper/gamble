;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base racket/contract gamble gamble/pict))

@(define (wiki suffix . content)
   (apply hyperlink (format "https://en.wikipedia.org/wiki/~a" suffix) content))

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
Carlo (MCMC) methods, samples are not independent}

]

Aside from @tech{samplers}, there are other @deftech{solvers} that can
extract information from a probabilistic model. The following solvers
are supported:
@itemlist[

@item{@racket[enumerate] --- exhaustive enumeration, exponential in number of
random variables, cannot directly handle continuous random variables}

]

@section[#:tag "sampler-funs"]{Basic Sampler Functions}

@defproc[(sampler? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{sampler}, @racket[#f]
otherwise.
}

@defproc[(generate-samples [s weighted-sampler?]
                           [n exact-nonnegative-integer?]
                           [#:burn burn exact-nonnegative-integer? 0]
                           [#:thin thin exact-nonnegative-integer? 0])
         (sample-frame/c any/c)]{

Generates @racket[n] samples from the sampler @racket[s]. The sampler is first
called @racket[burn] times and the results are discarded. In addition, the
sampler is called @racket[thin] times before every sample to be retained. The
results are returned in a @tech{sample frame}.
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

Produces a @tech{sampler} that uses rejection sampling---specifically,
``logic sampling''---for discrete random choices. The rejection
sampler can sample continuous random variables, but it cannot perform
@tech{observations}.
}

@defproc[(importance-sampler [m model?]
                             [#:propose propose (or/c #f (-> any/c dist? (or/c #f dist?))) #f])
         sampler?]{

Produces a @tech{sampler} that uses @wiki["Importance_sampling"]{importance
sampling}. That is, each evaluation of @racket[m] samples each random variable
from its prior by default, and each @tech{observation} adjust the current
sample's likelihood weight. The log-weight is included in the @tech{sample
frame} along with the sample value.

If @racket[propose] is not false, then each time a random variable is sampled,
@racket[propose] is called with the random variable's tag and prior
distribution. If the call returns @racket[#f], then the random variable is
sampled from the prior as usual. Otherwise, the result is a proposal
distribution: the random variable is sampled from the proposal distribution, and
the resulting bias is corrected by adjusting the sample's likelihood weight. If
the proposal distribution's support does not include the prior's support, the
sampler may be incorrect even in the limit.
}

@; ------------------------------------------------------------
@section[#:tag "mcmc-sampler"]{MCMC Sampler and Transitions}

@wiki["Markov_chain_Monte_Carlo"]{Markov-chain Monte Carlo (MCMC)} is an
algorithm framework for producing a correlated sequence of samples where each
sample is based on the previous. The algorithm is parameterized by the mechanism
for proposing a new state given the previous state; given a proposal, the MCMC
algorithm accepts or rejects it based on how the proposal was generated and the
relative likelihood of the proposed state.

The @racket[mcmc-sampler] function implements the MCMC framework, and the
proposal mechanisms are implemented by a variety of MCMC transition types.

@defproc[(mcmc-sampler [m model?]
                       [#:initialize initialize mcmc-transition? (initialize-transition)]
                       [#:transition transition (or/c mcmc-transition? mcmc-transition/single-site/c)
                                     (single-site-transition)])
         sampler?]{

Returns a @tech{sampler} that produces samples using a variant of MCMC.

The sampler is immediately initialized using @racket[initialize].
Subsequent transitions are performed by @racket[transition]. If
@racket[transition] is not an MCMC transition object
(@racket[mcmc-transition?]), then it is treated as a single-site transition
(@racket[mcmc-transition/single-site/c]) and wrapped with
@racket[single-site-transition].
}

@; ----------------------------------------
@subsection[#:tag "mcmc-transitions"]{MCMC Transitions}

@defproc[(mcmc-transition? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents a MCMC transition,
@racket[#f] otherwise.
}

@defproc[(single-site-transition [transition mcmc-transition/single-site/c #f]
                                 [#:any candidate? (or/c #f (-> any/c dist? boolean?)) #f])
         (and/c mcmc-transition?
                mcmc-transition/single-site?)]{

A transition that proposes a new state by randomly selecting a single random
variable matching @racket[candidate?] from the previous model execution and
updating it according to @racket[transition]. If @racket[candidate?] is
@racket[#f], then all random choices are considered.

If @racket[proposal] fails to propose a new value for a specific random choice,
the random choice is resampled as a fallback (see @racket[resample-proposal]).
}

@defproc[(mcmc-transition/single-site? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a primitive single-site MCMC transition
object, @racket[#f] otherwise.
}

@defthing[mcmc-transition/single-site/c contract?]{

Represents a transition to be applied based on a single random variable. One of
the following:
@itemlist[

@item{@racket[#f] --- The chosen random variable is resampled from its prior.}

@item{@racket[(proposal-value _value _l-R/F)] --- The chosen random variable is
set to @racket[_value], and @racket[_l-R/F] must be the log-ratio of the reverse
proposal over the forward proposal. For symmetric proposals, the log ratio is
@racket[0.0].}

@item{@racket[(proposal-kernel _kernel)] --- The chosen random variable's new
value is selected using the proposal kernel. In particular, the forward proposal
distribution is @racket[(_kernel _old-value)]; @racket[_new-value] is sampled
from the forward proposal distribution; the reverse proposal distribution is
@racket[(_kernel _new-value)]; and the proposal's log-ratio is automatically
computed from the reverse and forward proposals and the new and old values.}

@item{an object satisfying @racket[mcmc-transition/single-site?] --- The
transition object determines how the next state is chosen.}

@item{a procedure @racket[(-> any/c dist? any/c mcmc-transition/single-site/c)]
--- The procedure is called with the chosen random variable's tag, prior
distribution, and previous value, and it must return a single-site transition to
apply. This variant allows the choice of transition to depend on the selected
random variable.}

]
In the first three cases, the @wiki["Metropolis%E2%80%93Hastings_algorithm"]{Metropolis-Hastings}
algorithm is used to accept or reject the new state.
}

@defstruct*[proposal-value ([value any/c]
                            [l-R/F real?])]{

Represents a proposal with a specific value. See @racket[mcmc-transition/single-site/c] for use.
}

@defstruct*[proposal-kernel ([kernel (-> any/c dist?)])]{

Represents a proposal kernel. The @racket[kernel] field contains a function that
takes a value and returns a distribution based on that value.

Note that in a single @wiki["Metropolis%E2%80%93Hastings_algorithm"]{Metropolis-Hastings}
step, the kernel is generally applied twice, once to get a forward proposal
distribution, and again to get a reverse proposal distribution.
}

@defproc[(gibbs-transition [fallback (or/c #f mcmc-transition/single-site?) (slice-transition)])
         mcmc-transition/single-site?]{

Selects a random variable's next values via @wiki["Gibbs_sampling"]{Gibbs
sampling}.  This transition requires that the chosen random variable be
@tech{non-structural}.

If the posterior of the model slice corresponding to the chosen random variable
cannot be solved analytically, then the @racket[fallback] transition is used, or
if @racket[fallback] is @racket[#f] then an error is raised.
}

@defproc[(slice-transition [#:method method (or/c 'step 'double) 'double]
                           [#:W W (>/c 0.0) 1.0]
                           [#:M M (or/c exact-positive-integer? +inf.0) +inf.0]
                           [#:SD SD (>=/c 0) 5.0])
         mcmc-transition/single-site?]{

Selects a random variable's next value via @wiki["Slice_sampling"]{slice
sampling}.  This transition requires that the chosen random variable be
@tech{non-structural}.

The @racket[method] argument selects the technique for finding the slice's
interval to sample from, either stepping out or doubling.  The @racket[W]
argument controls the initial slice width, and if the @racket['step] method is
used, it is also the step size. If the @racket['step] method is used, at most
@racket[M] interval-widening steps are performed.

If the random variable's prior distribution is bounded and the bounds are less
than @racket[SD] apart, then the entire support is used as the initial slice
(that is, the stepping-out or doubling pass is skipped).
}

@defproc[(initialize-transition [get-value (-> any/c dist? (or/c #f proposal-value?)
                                               (or/c #f proposal-value?))
                                           (lambda (tag dist old-value) #f)])
         mcmc-transition?]{

A transition that reruns the progam, setting random variables to their values
according to @racket[get-value]. The transition always accepts if the
execution's likelihood is nonzero. Use @racket[initialize-transition] to
initialize an MCMC sampler when random initialization is infeasible.

For each random variable found in the execution of the model, @racket[get-value]
is called with the random variable's tag, its prior distribution. If the random
variable has a previous value, then the third argument to @racket[get-value] is
@racket[(proposal-value _prev-value 0.0)]; otherwise, the third argument is
@racket[#f]. If @racket[get-value] returns @racket[(proposal _new-value
_ignored)], then the random variable is set to @racket[_new-value]; if
@racket[get-value] returns @racket[#f], then the previous value is used, if it
exists, or the random variable is resampled from its prior distribution.
}

@; ============================================================
@section[#:tag "enumerate"]{Enumeration Solver}

@defproc[(enumerate [m model?]
                    [#:stop stop-limit (>=/c 0) 0]
                    [#:discretize discretize (or/c #f (-> real-dist? (or/c #f enumerable-dist?))) #f]
                    [#:normalize? normalize? boolean? #f])
         discrete-dist?]{

Returns a discrete distribution of the values produced by @racket[m], weighted
by any conditioning or scoring performed by the function.

The @racket[enumerate] form works by exploring all possibilities using the
technique described in @cite{EPP}. Exploration ceases only when the apparent
total probability weight of all unexplored paths is less than
@racket[stop-limit]. If exploration is not stopped, then any countable
distribution causes @racket[enumerate] to fail to terminate.

Only enumerable distributions can be sampled with @racket[enumerate]. If a
continuous distribution is encountered, and the @racket[discretize] argument is
a procedure, it is called to convert the distribution into an enumerable
approximation. If the @racket[discretize] argument is @racket[#f], or the
function returns @racket[#f], then @racket[enumerate] raises an exception.
}

@(close-eval the-eval)
