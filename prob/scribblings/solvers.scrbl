;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     prob
                     prob/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require prob (only-in prob/viz
                                   [hist-pict hist]
                                   [bin-pict bin])))

@title[#:tag "solvers"]{Samplers and Solvers}

The point of writing a generate model is typically not to simply run
it to generate data, but rather to do @emph{inference} on it---for
example, to estimate parameters given observed data. This requires
wrapping the model in a @tech{sampler} or @tech{solver} of some sort.

A @deftech{sampler} is an object that contains a probabilistic program
and produces samples from the posterior distribution of its result
expression. Samplers can be either unweighted (where each sample is
equally representative of the posterior distribution) or
@deftech[#:key "weighted sampler"]{weighted} (where each sample comes
with a factor that corrects for the ratio of its sampling frequency
and its probability in the posterior distribution).

The following samplers are supported:
@itemlist[
@item{@racket[rejection-sampler] --- unweighted sampler, does not
support observations on continuous random variables}
@item{@racket[importance-sampler] --- weighted sampler}
@item{@racket[enum-importance-sampler] --- weighted sampler}
@item{@racket[mh-sampler] --- unweighted sampler, uses MCMC to seek
high-probability zones}
]

Aside from @tech{samplers}, there are other @deftech{solvers} that can
extract information from a probabilistic model. The following solvers
are supported:
@itemlist[
@item{@racket[enumerate] --- exhaustive enumeration (up to optional
threshold), exponential in number of random variables, cannot handle
sampling from continuous random variables}
]

The examples in the following sections use the following function
definitions:

@interaction[#:eval the-eval
(define (count-heads n)
  (if (zero? n) 0 (+ (if (flip) 1 0) (count-heads (sub1 n)))))
(define (geom)
  (if (flip) 0 (add1 (geom))))
]

@section[#:tag "sampler-funs"]{Basic Sampler Functions}

@defproc[(sampler? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{sampler}, @racket[#f]
otherwise.

A sampler is also an applicable object. That is, if @racket[_s] is a
sampler, then evaluating @racket[(_s)] generates a sample.
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

Generates @racket[n] samples from the sampler @racket[s]. If
@racket[s] is not a sampler but only a weighted sampler, unweighted
samples are produced by residual resampling (see @racket[resample]).

The sampler is first called @racket[burn] times and the results are
discarded. In addition, the sampler is called @racket[thin] times
before every sample to be retained.
}

@defproc[(generate-weighted-samples [s weighted-sampler?] 
                                    [n exact-nonnegative-integer?]
                                    [#:burn burn exact-nonnegative-integer? 0]
                                    [#:thin thin exact-nonnegative-integer? 0])
         (vectorof (cons/c any/c (>=/c 0)))]{

Generates @racket[n] weighted samples from the @tech{weighted sampler}
@racket[s] and returns them as a vector of value-weight pairs.
}

@defproc[(sampler->discrete-dist [sampler weighted-sampler?]
                                 [n exact-positive-integer?]
                                 [f (-> any/c any/c) (lambda (x) x)]
                                 [#:burn burn exact-nonnegative-integer? 0]
                                 [#:thin thin exact-nonnegative-integer? 0])
         discrete-dist?]{

Returns the empirical distribution obtained by generating @racket[n]
samples from @racket[s], apply @racket[f] to each result.

@examples[#:eval the-eval
(sampler->discrete-dist (rejection-sampler (flip 1/2)) 100)
(sampler->discrete-dist
  (importance-sampler
    (define R (binomial 20 1/2))
    (observe-at (normal-dist R 1) 9)
    R)
  100)
]
}


@; ============================================================
@section[#:tag "samplers-basic"]{Basic Sampler Forms}

The samplers supported by this language consist of simple samplers and
a more complicated and flexible
@seclink["mh-sampler"]{Metropolis-Hastings sampler framework}.

@defform[(rejection-sampler def/expr ... result-expr maybe-when-clause)
         #:grammar ([maybe-when-clause (code:line)
                                       (code:line #:when condition-expr)])]{

Produces a @tech{sampler} that, when applied, returns a value of
@racket[result-expr] arising from an execution where
@racket[condition-expr], if present, was satisfied.

The sampler is implemented using rejection sampling---specifically,
``logic sampling''---for discrete random choices. That is, the
@racket[def/expr]s and @racket[result-expr] are evaluated. Then
@racket[condition-expr] is evaluated, if present; if it produces
@racket[#t], the execution is accepted and the value of
@racket[result-expr] is returned; otherwise, the process is repeated.

The rejection sampler can sample continuous random variables, but it
cannot perform observations (@racket[observe-at]) on them.

@examples[#:eval the-eval
(define s-or
  (rejection-sampler
    (define A (flip))
    (define B (flip))
    A
    #:when (or A B)))
(hist (repeat s-or 100))
(hist (repeat s-or 1000))

(define rs-count-heads
  (rejection-sampler
   (count-heads 10)))
(rs-count-heads)
(rs-count-heads)
(hist (repeat rs-count-heads 100))
]
}

@defform[(importance-sampler def/expr ... result-expr maybe-when-clause)
         #:grammar ([maybe-when-clause (code:line)
				       (code:line #:when condition-expr)])]{

Like @racket[rejection-sampler], but returns a @emph{weighted sampler}
that uses weights to represent the quality of a particular sample
given the observations in the program. Thus unlike a rejection
sampler, an importance sampler can handle observations
(@racket[observe-at]) on continuous random variables.
}


@; ------------------------------------------------------------
@section[#:tag "mh-sampler"]{Metropolis-Hastings Sampler and Transitions}

Metropolis-Hastings (MH) is an algorithm framework for producing a
correlated sequence of samples where each sample is based on the
previous. The algorithm is parameterized by the mechanism for
proposing a new state given the previous state; given a proposal, the
MH algorithm accepts or rejects it based on how the proposal was
generated and the relative likelihood of the proposed state.

The @racket[mh-sampler] form implements the Metropolis-Hastings
framework, and the proposal mechanisms are implemented by a variety of
@deftech{MH transition} types.


@defform[(mh-sampler def/expr ... result-expr 
           maybe-when-clause 
           maybe-transition-clause)
         #:grammar ([maybe-when-clause (code:line)
                                       (code:line #:when condition-expr)]
                    [maybe-transition-clause (code:line)
                                             (code:line #:transition transition-expr)])]{

Returns a @tech{sampler} that produces samples using a variant of
Metropolis-Hastings.

The @racket[transition-expr] determines the mechanism used to propose
new states. If absent, @racket[(single-site)] is used.

@examples[#:eval the-eval
(define mh-or
  (mh-sampler
    (define A (flip))
    (define B (flip))
    A
    #:when (or A B)))
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

@defform[(hmc-sampler def/expr ... result-expr
                      maybe-epsilon-clause maybe-L-clause maybe-when-clause)
         #:grammar ([maybe-epsilon-clause (code:line)
                                          (code:line #:epsilon epsilon-expr)]
                    [maybe-L-clause (code:line)
                                    (code:line #:L L-expr)]
                    [maybe-when-clause (code:line)
                                       (code:line #:when cond-expr)])]{

Equivalent to the following:
@racketblock[(mh-sampler def/expr ... result-expr
               #:when condition-expr
               #:transition (hmc epsilon-expr L-expr))]
}


@; ----------------------------------------
@subsection[#:tag "mh-transitions"]{Metropolis-Hastings Transitions}

@defproc[(mh-transition? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents a @tech{MH transition},
@racket[#f] otherwise.
}

@defproc[(single-site [proposal (or/c proposal? (-> proposal?)) (default-proposal)]
                      [#:zone zone-pattern any/c #f]
                      [#:record-obs? record-obs? boolean? #t])
         mh-transition?]{

A transition that proposes a new state by randomly (uniformly)
selecting a single random choice in any zone matching
@racket[zone-pattern] and perturbing it according to @racket[proposal]
(see @secref["mh-proposals"]).

@; FIXME: perturbation parameters!!!

A @deftech{zone pattern} matches a zone if the two values are
@racket[equal?] or if the zone pattern is @racket[#f].

If the set of observations in the probabilistic program is always the
same (that is, the parameters may change, but the number of
observations and the observed values stay the same), then it is safe
to set @racket[record-obs?] to @racket[#f] to avoid creating database
entries for observations. This can improve the efficiency of typical
programs. If the observation set changes from run to run, however,
then @racket[record-obs?] must be @racket[#t].
}

@defproc[(multi-site [proposal (or/c proposal? (-> proposal?)) (default-proposal)]
                     [#:zone zone-pattern any/c #f]
                     [#:record-obs? record-obs? boolean? #t])
         mh-transition?]{

A transition that proposes a new state by perturbing @emph{all} random
choices in all zones matching @racket[zone-pattern].
}

@defproc[(slice [#:scale scale-factor (>/c 0) 1]
                [#:zone zone-pattern any/c #f])
         mh-transition?]{

A transition that picks a new state via slice sampling @;{FIXME: need
reference} on a single random choice selected randomly from any zone
@techlink[#:key "zone pattern"]{matching} @racket[zone-pattern].

The @racket[scale-factor] argument controls the width parameter used
to find the slice bounds.
}

@defproc[(enumerative-gibbs [#:zone zone-pattern any/c #f]
                            [#:record-obs? record-obs? boolean? #t])
         mh-transition?]{

A transition that chooses a single random choice from a zone matching
@racket[zone-pattern] and resamples it from its full conditional
probability distribution given the values of all of the other choices
in the program. The distribution of the choice to be perturbed must be
finite; otherwise, an error is raised. The choice to be perturbed must
be @emph{non-structural}---that is, its value must not determine
whether subsequent choices are made or not---otherwise, an error is
raised.

Note: unlike traditional Gibbs sampling, this transition picks a
choice at random rather than perturbing all choices round-robin.
}

@defproc[(hmc [epsilon (>/c 0) 0.01]
              [L exact-positive-integer? 10]
              [#:zone zone-pattern any/c #f])
         mh-transition?]{

A transition that picks a new state via Hamiltonian mechanics using
the negative log-likelihood of a state as its potential energy.

This transition is @bold{experimental} and comes with a number of
restrictions:

@itemlist[
@item{There must be no structural dependencies among the distributions
of @racket[def/expr ... result-expr]}
@item{All the distributions must be continuous.}
@item{Any distribution that has parameters that depend on the value of
another distribution must be wrapped in a @racket[derivative]
form. See @seclink["hmc-utils"] for more information.}
]

The parameters @racket[epsilon] and @racket[L] specify the size of
each Hamiltonian step and the number of Hamiltonian steps to take in
the course of obtaining a sample, respectively.  Note that careful
tuning may be required to achive good results.

@examples[#:eval the-eval
(define one-dim-loc-hmc
  (hmc-sampler
     (define Hid (label 'Hid (derivative (normal 10 1) #f #f)))
     (derivative (observe-at (normal-dist Hid 0.5) 9.0)
                 [(Hid) (λ (hid) 1)]
                 #f)
     Hid
     #:epsilon 0.01
     #:L 90))

(bin (repeat one-dim-loc-hmc 100))
]
}

@defproc[(cycle [tx mh-transition?] ...+)
         mh-transition?]{

Returns a transition that uses an endless cycle of the @racket[tx]s to
produce samples.
}

@defproc[(mixture [txs (listof mh-transition?)]
                  [weights (listof (>/c 0)) (map (lambda (tx) 1) txs)])
         mh-transition?]{

Returns a transition that randomly selects a transition from
@racket[txs], weighted by @racket[weights], on each step.
}

@; ----------------------------------------
@subsection[#:tag "mh-proposals"]{Metropolis-Hastings Proposals}

The @racket[single-site] and @racket[multi-site] transitions are
parameterized by the proposal distribution used to perturb a single
random choice.

@defproc[(proposal? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a proposal object, @racket[#f]
otherwise.
}

@defproc[(proposal:resample) proposal?]{

Returns a proposal that chooses the new value of a variable by simply
resampling from the variable's prior distribution.
}

@defproc[(proposal:drift) proposal?]{

Returns a proposal that chooses the new value of a variable by
choosing a value near the current value, with an adaptive variance.
}

@defparam[default-proposal proposal
          (or/c proposal? (-> proposal?))]{

Parameter for the default proposal used by MH transitions such as
@racket[single-site] and @racket[multi-site]. If the value of the
parameter is a function, it is applied when the transition is created
to get an actual proposal.

The initial value is @racket[proposal:drift].
}

@; ----------------------------------------
@subsection[#:tag "hmc-utils"]{Specifying Derivatives for HMC}

The @racket[hmc] transition requires that all the distributions in the
model are continuous.  It further requires partial derivatives for
each parameter of the distribution of each random variable in terms of
any previous random variable. Such information is provided using the
@racket[derivative] special form.

@defform[(derivative sampling-expr parameter-derivative-clause ...)
         #:grammar ([parameter-derivative-clause
                     (code:line [(label-ids ...) partial-fn-expr])
                     (code:line #f)])]{

Annotates the @racket[sampling-expr] expression with the partial
derivatives of its parameters.

The @racket[sampling-expr] should be either a call to @racket[sample]
or a use of one of the functions from @secref["erps"].

There should be as many parameter derivative clauses as there are
parameters of the underlying distribution. Each
@racket[parameter-derivative-clause] consists of a sequence of labels
of the random variables that the parameter depends on together with a
function that produces the partial derivative of that parameter value
with respect to the listed random variables, given the values of those
random variables. The clause @racket[[() (lambda () (values))]], which
indicates that the parameter is independent of all previous random
variables, can be abbreviated as @racket[#f].

@examples[#:eval the-eval
(define derivative-example
  (hmc-sampler
   (define A (label 'A-lbl (derivative (normal 0 1) #f #f)))
   (define B (label 'B-lbl (derivative (normal 1 1) #f #f)))

   (define C (derivative (normal (- (* A A) (* B B)) 1)
                         [(A-lbl B-lbl)
                          (λ (a b)
                            (values (* 2 a)
                                    (- (* 2 b))))]
                         #f))
   B))
]

In the example above, @racket[A] and @racket[B] do not depend on any
other random variables, so the derivatives of their parameters are
zero (shorthand: @racket[#f]). The mean (first parameter) of
@racket[C], on the other hand, depends on both @racket[A] and
@racket[B], so its derivative clause lists their labels, and the
function returns two values: the partial derivative of @racket[(- (* A
A) (* B B))] with respect to @racket[A] and @racket[B], in that order.
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
@section[#:tag "enum"]{Enumeration Solver}

@defform[(enumerate def/expr ... result-expr
           maybe-when-clause maybe-limit-clause maybe-normalize-clause)
         #:grammar ([maybe-when-clause (code:line)
                                       (code:line #:when condition-expr)]
                    [maybe-limit-clause (code:line)
                                        (code:line #:limit limit-expr)]
                    [maybe-normalize-clause (code:line)
                                            (code:line #:normalize? normalize?-expr)])]{

Returns a discrete distribution of the form @racket[(discrete-dist
[_value _prob] ...)], where @racket[_value] is a value produced by
@racket[result-expr] and @racket[_prob] is its normalized probability
given @racket[condition-expr] and the observations in the program. If
@racket[normalize?-expr] is given and evaluates to @racket[#f], then
the probability is unnormalized instead.

The @racket[enumerate] form works by exploring all possibilities using
the technique described in @cite{EPP}. If @racket[limit-expr] is
absent or evaluates to @racket[#f], then exploration ceases only when
all paths have been explored; if any path is infinite, then
@racket[enumerate] fails to terminate.  If @racket[limit-expr] is
given and evaluates to a probability @racket[_limit], then exploration
ceases when the error of the normalized result distribution would be
less than @racket[_limit]---that is, when the unexplored paths have
probability less than @racket[_limit] times the sum of the
probabilities of the paths accepted so far.

Only discrete and integer-valued distributions can be sampled with
@racket[enumerate], and infinite-range distributions (such as
@racket[geometric-dist]) and infinitely-deep recursive functions (such
as @racket[geometric]) require the use of @racket[#:limit] for
termination.

@examples[#:eval the-eval
(enumerate
  (define A (flip))
  (define B (flip))
  A
  #:when (or A B))
]

Use @racket[#:limit] to control when exploration should be cut
off.

@interaction[#:eval the-eval
(enumerate
  (geom)
  #:limit 1e-6)
]

Use @racket[#:normalize? #f] to get the result probabilities without
normalizing by the acceptance rate:

@interaction[#:eval the-eval
(enumerate
 (define (drop-coin?) (flip 0.9))
 (define (drunk-flips n)
   (cond [(zero? n)
          #t]
         [(drop-coin?)
          'failed]
         [else
          (and (flip) (drunk-flips (sub1 n)))]))
 (define A (drunk-flips 10))
 (eq? A #t)
 #:when (not (eq? A 'failed))
 #:normalize? #f)
]
}


@(close-eval the-eval)

