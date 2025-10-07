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
@(the-eval '(require racket/class gamble gamble/pict))
@(the-eval '(random-seed 1))

@title[#:tag "intro"]{Introduction}

The @racketmodname[gamble] library supports
@itemlist[
@item{the expression of generative probabilistic models, and}
@item{inference over those models.}
]

@; ----------------------------------------
@section{Probabilistic Models}

A probabilistic model consists of a computation involving @tech{primitive random
variables} created with @racket[sample] and @tech{observations} expressed with
@racket[observe] or @racket[score]. These forms may be used within a
@racket[model] expression, and the computation should use only the pure subset
of Racket plus the stochastic forms provided by this library.

Here is a trivial probabilistic model. This model has one @tech{primitive random
variable}, because an execution of the model calls @racket[sample] once --- even
though the random variable does not correspond to a Racket variable.

@interaction[#:eval the-eval
(define m1 (model (sample (bernoulli-dist 1/4))))
]

The @racket[sample] form randomly draws a value from the given distribution,
called the random variable's @wiki["Prior_probability"]{prior distribution}. The
@racket[(bernoulli-dist 1/2)] distribution represents a distribution on the
values @racket[1] and @racket[0], where @racket[1] has the probability
@racket[1/4]. (There is also a @racket[boolean-dist] distribution type that
produces true and false instead.)

A model is typically executed in the context of a sampler or the
@racket[enumerate] solver, but it can also be evaluated using the
@racket[run-model] form:

@interaction[#:eval the-eval
(run-model m1)
]

Running the model with @racket[run-model] returns the result of the model's
computation, and it also prints the log-likelihood of that execution of the
model. The @wiki["Likelihood_function"]{likelihood} corresponds to the
observations, not the priors, and this model has no observations.

Random functions can be mixed with ordinary Racket code:

@interaction[#:eval the-eval
(run-model (model (for/list ([i 10]) (sample (bernoulli-dist 1/4)))))
]

This model has ten @tech{primitive random variables}.

@; ----------------------------------------
@section[#:tag "intro-obs"]{Models and Observations}

A typical model samples random variables and then uses @tech{observations} to
adjust the probability based on evidence. For example, suppose there is some
process that has an unknown success probability, such as flipping a coin with
unknown bias. Since we start with no opinion about the success probability, we
will use @racket[(uniform-dist 0 1)] as its @wiki["Prior_probability"]{prior
distribution}.

Then suppose we run ten trials and get three successes. It appears that the
success probability must be about 30%, but perhaps we simply got unusual results
from the small number of trials we have performed so far. Perhaps if we ran more
trials we would discover that the success probability was higher --- or even
lower. Bayesian analysis quantifies this uncertainty as the
@wiki["Posterior_probability"]{posterior distribution} of the success
probability @emph{given} the evidence available so far. Evidence is incorporated
using the @racket[observe] form:

@interaction[#:eval the-eval
(define num-heads 3)
(define num-tails 7)
(define coin-bias/m
  (model
    (define p (sample (uniform-dist 0 1)))
    (for ([i (in-range num-heads)])
      (observe (bernoulli-dist p) 1))
    (for ([i (in-range num-tails)])
      (observe (bernoulli-dist p) 0))
    p))
]

We can run the model using @racket[run-model] as before:

@interaction[#:eval the-eval
(run-model coin-bias/m)
(run-model coin-bias/m)
]

The @racket[run-model] form, however, gives us very little control over the
execution of the model, and it does not offer a convenient way to capture the
likelihood. To repeat this process many times and capture the likelihood each
time, we must create a @emph{sampler}. An importance sampler
(@racket[importance-sampler]) is the simplest sampler that supports
observations. Then we can use @racket[generate-samples] to produce a
@tech{sample frame} containing a vector of values and a corresponding vector of
log-likelihood weights.

@interaction[#:eval the-eval
(define coin-bias/s (importance-sampler coin-bias/m))
(generate-samples coin-bias/s 5)
]

A sample frame with real-valued samples can be visualized using
@racket[samples->pict]:
@interaction[#:eval the-eval
(samples->pict (generate-samples coin-bias/s 100))
]
The plot shows the sample points (blue circles), the
@wiki["Empirical_distribution_function"]{empirical CDF}, and
@wiki["Kernel_density_estimation"]{Gaussian kernel density estimators} with
various smoothing bandwidths.

Alternatively, we can collect the samples as a discrete @emph{empirical
distribution}. Then that distribution can be visualized with
@racket[dist->pict]:

@interaction[#:eval the-eval
(dist->pict (sampler->discrete-dist coin-bias/s 100))
]

This plot's y-axis scale, probability weight, is different because
@racket[sampler->discrete-dist] by default produces @emph{normalized}
distribution --- its probability weights sum to 1. The shape of this plot may
differ slightly from the one above, because we have generated another 100
samples to create the empirical distribution.

The model above performs success and failure observations in a loop. There are
better ways of expressing this. When the observation distribution is the same,
@racket[observe*] can be used with multiple observed values collected as a
vector:

@interaction[#:eval the-eval
(define coin-bias2/m
  (model
    (define p (sample (uniform-dist 0 1)))
    (observe* (bernoulli-dist p)
              (make-vector num-heads 1))
    (observe* (bernoulli-dist p)
              (make-vector num-tails 0))
    p))
]

Alternatively, multiple Bernoulli observations can be expressed as a single
observation from a binomial distribution (@racket[binomial-dist]):

@interaction[#:eval the-eval
(define coin-bias3/m
  (model
    (define p (sample (uniform-dist 0 1)))
    (observe (binomial-dist (+ num-heads num-tails) p)
             num-heads)
    p))
(define coin-bias3/s (importance-sampler coin-bias3/m))
(dist->pict (sampler->discrete-dist coin-bias3/s 100))
]

In fact, the posterior probability distribution of this model is exactly a beta
distribution (@racket[beta-dist]) parameterized by the number of observed
successes and failures (plus one). The beta distribution is a
@wiki["Conjugate_prior"]{conjugate prior} for Bernoulli and binomial
observations, and Uniform(0,1) is equivalent to Beta(1,1).  We can compare the
visualizations of that distribution to the model's empirical distribution to
confirm that our sampler produces roughly the right result:

@interaction[#:eval the-eval
(dist->pict (beta-dist (add1 num-heads) (add1 num-tails)))
]


@; ----------------------------------------
@section[#:tag "intro-mcmc"]{MCMC Sampling}
@(the-eval '(random-seed 1))

Importance sampling, while conceptually simple, performs badly when a model's
posterior distribution differs much from its prior distribution.

Suppose we have an oven and we want to know its current temperature. Suppose
that the oven's temperature range is bounded by 0°F and 800°F. Suppose the oven
has several thermometers, but they are quite imprecise. To model that
imprecision, let's say that each thermometer reading is normally distributed,
centered around the true temperature with a standard deviation of 5°F. Given
thermometer measurements, what is the true temperature of the oven? Here is the
model:

@interaction[#:eval the-eval
(define THERMO-ERROR 5) (code:comment "measurement imprecision as std.dev.")
(define thermometer-measurements (vector 415 418 407 415)) ;; 410
(define thermo/m
  (model
    (define temperature (sample (uniform-dist 0 800)))
    (observe* (normal-dist temperature THERMO-ERROR)
              thermometer-measurements)
    temperature))
]

As before, we can create an importance sampler, generate samples to make an
empirical distribution, and plot that:

@interaction[#:eval the-eval
(dist->pict (sampler->discrete-dist (importance-sampler thermo/m) 100))
]

As expected, there is a spike (or multiple spikes) around the measurements. But
notice that there are only a handful of samples, out of 100 generated, that are
in the vicinity of the measurements. In an importance sampler, the samples (by
default) are generated from the prior distributions, and in this model the prior
distribution, @racket[(uniform-dist 0 800)], is much wider than the posterior
distribution. Most of the samples are useless, and generating them is wasted
effort.

If we already know the posterior distribution, roughly, we can direct the
importance sampler to generate temperatures from a
@wiki["Importance_sampling"]{proposal distribution} instead of from the prior
distribution. This introduces bias, but the sampler automatically corrects the
bias by adjusting the likelihiood weights. For example, we can generate samples
focused on the vicinity of 400°F (normally distributed with a standard deviation
of 20°F) as follows:

@interaction[#:eval the-eval
(dist->pict
 (sampler->discrete-dist
  (importance-sampler thermo/m
                      #:propose (lambda (tag prior-dist)
                                  (normal-dist 400 20)))
  100))
]

What if we don't already know the answer, or can't easily describe it?

Rather than generating every sample @emph{independently}, an alternative
approach is to recognize when we have found a ``good'' sample and then try to
use that value as the basis for generating the next value. This is the rough
idea behind the @wiki["Markov_chain_Monte_Carlo"]{Markov-chain Monte Carlo
(MCMC)} family of techniques. Within the MCMC framework, there are different
ways of generating the next sample based on the previous state; this library
represents each technique as a @emph{transition} (@racket[mcmc-transition?])
object.  In general, if the next value is ``better'' (more likely), it is kept;
otherwise it is randomly kept or discarded, with a probability based on its
likelihood. It is necessary to sometimes take ``bad'' steps to actually explore
the posterior distribution. Since the likelihood is used to determine whether to
keep or discard the new sample, it must not be reused to weight the sample's
importance, so all samples from an MCMC sampler have the same weight. Instead,
when a proposed value is discarded, the previous value is repeated; thus high
likelihood translates to frequency of repeated values.

The default transition selects a single random variable from the model,
resamples from its prior distribution, and re-evaluates the model. This model
has a single random variable, but in general, random variables not selected for
change retain their values from the previous run. We typically discard
(``burn'') the first few samples so the sampler is more likely to start in a
high-probability zone.

@interaction[#:eval the-eval
(dist->pict
 (sampler->discrete-dist
  (mcmc-sampler thermo/m)
  100 #:burn 10))
]

The sampler has found the ``good'' part of the distribution, but once it finds a
good value, it barely moves at all! The problem is that a random value resampled
from the prior is very unlikely by comparison, so the sampler almost always
sticks to the same value. In the limit, it is still correct, but it will
take a large number of samples to converge to an accurate picture of the
posterior distribution. Here is the same sampler with 10000 samples:

@interaction[#:eval the-eval
(dist->pict
 (sampler->discrete-dist
  (mcmc-sampler thermo/m)
  10000 #:burn 10))
]

A better idea, given a relatively ``good'' value, would be to propose new values
from its vicinity. Then each step is more likely to produce a better value, and
even worse values will be only moderately worse, and more likely to be accepted
anyway.

@(the-eval '(random-seed 1))
@interaction[#:eval the-eval
(dist->pict
 (sampler->discrete-dist
  (mcmc-sampler thermo/m
                #:transition (proposal-kernel
                              (lambda (x) (normal-dist x 20))))
  100 #:burn 25))
]

This is better, but out of 100 samples there are still relatively few distinct
values. (Each sample is equally weighted, so the height of a point above the
x-axis represents its frequency --- and repeated values indicate the sampler's
lack of exploration.)

Decreasing the step size can result in more motion:

@(the-eval '(random-seed 1))
@interaction[#:eval the-eval
(dist->pict
 (sampler->discrete-dist
  (mcmc-sampler thermo/m
                #:transition (proposal-kernel
                              (lambda (x) (normal-dist x 10))))
  100 #:burn 25))
]

On the other hand, decreasing the step size too much increases autocorrelation
between samples, and even distinct samples might not be meaningfully distinct.

Another kind of transition is
@racket[(slice-transition)]. @wiki["Slice_sampling"]{Slice sampling} is often
effective at reducing autocorrelation between samples, but it has the
disadvantage of requiring multiple evaluations of the model per sample
generated.

@interaction[#:eval the-eval
(dist->pict
 (sampler->discrete-dist
  (mcmc-sampler thermo/m #:transition (slice-transition))
  100 #:burn 10))
]

In fact, this model also has an exact answer: the posterior is a truncated
normal distribution. More generally, when it possible to calculate a random
variable's posterior distribution conditioned on the current values of any other
random variables, @racket[slice-transition] does @wiki["Gibbs_sampling"]{Gibbs
sampling} for that variable by default. Basic inspection of a model can be done
via @racket[model-slice]:

@interaction[#:eval the-eval
(define-values (thermo-tags thermo-pdist thermo-lj thermo-eval)
  (model-slice thermo/m))
thermo-pdist
;(dist->pict thermo-pdist)
(map thermo-lj '(411 412 413 414 415))
]

The second result is the conditional distribution of the slice variable
conditioned on the current values of all other variables --- since this model
has only one variable, it is also the model's posterior distribution. The third
result evaluates the slice's unnormalized posterior log-density at the given
point.

@; ------------------------------------------------------------
@section{Enumeration}

Enumeration uses delimited continuations to make a probability-weighted tree of
possibile execution paths. Exhaustive (or nearly exhaustive) exploration of the
tree is done with the @racket[enumerate] solver form.

@interaction[#:eval the-eval
(enumerate (model (for/sum ([i 10]) (sample (bernoulli-dist 1/2)))))
]

The results above agree with @racket[binomial-dist]:

@interaction[#:eval the-eval
(enumerate (model (sample (binomial-dist 10 1/2))))
]

The @racket[enumerate] form can be used to approximate countable distributions
by using a limit parameter; the exploration stops when the apparent total
probability of all unexplored paths is less than the given limit.

@interaction[#:eval the-eval
(enumerate
  #:stop 1e-3
  (model
    (define (geom) (if (sample (boolean-dist 1/2)) 0 (add1 (geom))))
    (geom)))
]

Note that the probabilities do not quite sum to 1, because the search stops
early. Use @racket[#:normalize? #t] to normalize the distribution.

Continuous random variables cannot be exhaustively enumerated, but
@racket[enumerate] can use an optional discretizer function to replace
continuous prior distributions with enumerable approximations.

@interaction[#:eval the-eval
(dist->pict
 (enumerate
   #:discretize (lambda (tag dist) (dist-discretize dist 100))
   #:normalize? #t
   coin-bias/m))
]

@(close-eval the-eval)
