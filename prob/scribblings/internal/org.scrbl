;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     prob
		     prob/private/interfaces))

@title[#:tag "org"]{Prob Internals Organization}

The @racketmodname[prob] language consists of the following parts:
@itemlist[
@item{some probability-related things (eg, distributions)}
@item{some samplers and solvers for probabilistic models}
@item{a source instrumenter that implements a variant of the Bher
addressing (``naming'') scheme for random choices}
]

@bold{Probability-related things}

Distributions.


@bold{Samplers and solvers}

Samplers are represented as objects implementing the
@racket[sampler<%>] or @racket[weighted-sampler<%>] interface.

The following sampler/solver implementations exist:
@itemlist[
@item{@racket[rejection-sampler%] --- unweighted sampler, uses logic sampling}
@item{@racket[importance-sampler%] --- weighted sampler}
@item{enumerate --- exhaustive enumeration using delimited continuations}
@item{@racket[enum-importance-sampler%] --- weighted sampler using delimited
continuations}
@item{@secref["mh-sampler"] --- DB-based MH sampler framework, relies
on instrumenter; supports programmable and extensible ``transitions''}
]

See also @secref["interfaces"].

@defmodule[prob/private/prob-syntax #:no-declare]

Defines syntax (macros) for each kind of sampler/solver. The sampler
classes are not exposed to the user.


@bold{Instrumenter}

The instrumentor rewrites expanded code to track
``addresses''---unique, stable values associated with points in
probabilistic program execution. These addresses are used by DB-based
samplers/solvers like @racket[mh-sampler].

@defmodule[prob/private/context #:no-declare]

Contains the representation of contexts, the parameter that holds the
current context, and some macros and functions for manipulating
contexts.

@defmodule[prob/private/instrument #:no-declare]

Contains the instrumenter itself.

@defmodule[prob/private/instrumenting-lang #:no-declare]

Like @racketmodname[racket/base] except with the instrumenter enabled;
that is, it is @racketmodname[prob] without any of the library
support. It is useful for writing support functions that need
instrumentation (eg, replacements for HO functions like @racket[map]).

