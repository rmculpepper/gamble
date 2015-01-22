;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     gamble
                     (only-in gamble/private/interfaces
                              weighted-sampler<%>
                              sampler<%>)))

@title[#:tag "interfaces"]{Gamble Internal Interfaces}

@defmodule[gamble/private/interfaces]

Defines the interfaces representing samplers as well as related
auxiliary interfaces, base classes, and types.

@section[#:tag "sampler-ifc"]{Sampler Interfaces}

@definterface[weighted-sampler<%> ()]{

Represents a weighted sampler.

@defmethod[(sample/weight) (cons/c any/c (>=/c 0))]{

Produce a weighted sample @racket[(cons _value _weight)].
}
}

@definterface[sampler<%> (weighted-sampler<%>)]{

Represents a sampler. Samplers are applicable objects; that is, they
can be applied like procedures; doing so calls the @method[sampler<%>
sample] method.

@defmethod[(sample) any/c]{

Produce an unweighted sample.
}
}


@section[#:tag "stochastic-ctx"]{Stochastic Contexts}

@definterface[stochastic-ctx<%> ()]{

Represents an execution context within a sampler/solver. Provides
implementations of stochastic behavior via
@racket[(current-stochastic-ctx)], which is set by the enclosing
sampler/solver before executing the probabilistic program.

May be stateful. For example, the @racket[db-stochastic-ctx%] class
records and replays @method[stochastic-ctx<%> sample] choices, adjusts
trace likelihoods on @method[stochastic-ctx<%> observe-sample], etc.

@defmethod[(sample [d dist?]) any/c]{

Samples from @racket[dist]. Implements @racket[sample].
}

@defmethod[(observe-sample [d dist?] [v any/c]) void?]{

Represents observing the value @racket[v] from the distribution
@racket[d], whose parameters typically depend on previous random
choices within the trace. The effect is typically to adjust the
current trace's likelihood.

Implements @racket[observe-sample].
}

@defmethod[(fail [reason any/c]) none/c]{

Represents condition failure. Causes the current trace to
terminate. Typically, a new trace is then started with different
choices.

Implements @racket[fail].
}

@defmethod[(mem [f procedure?]) procedure?]{

Returns a memoized version of @racket[f]. The memoized function may
only be used while the stochastic context is still active.

Implements @racket[mem].
}
}

@defparam[current-stochastic-ctx ctx (is-a?/c stochastic-ctx<%>)]{

Holds the stochastic context of the current execution of the current
running sampler/solver.
}

@section[#:tag "zones"]{Zones}

A Zone is any value other than @racket[#f].

A ZonePattern is either a Zone---which matches itself---or
@racket[#f], which matches any Zone.

@defparam[current-zones zones list?]{

The current zones, nearest enclosing first.
}
 
