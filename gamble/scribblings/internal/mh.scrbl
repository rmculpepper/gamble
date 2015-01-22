;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     gamble))

@title[#:tag "mh-sampler"]{Metropolis-Hastings Implementation}

@defmodule[gamble/private/prob-mh]

Contains the implementation of the Metropolis-Hastings framework,
realized by @racket[mh-sampler]. The MH sampler can use several
different kinds of transitions, and it relies on the call-site
instrumentation done by the instrumenter (@secref["instrument"]).

@section{Database}

The MH sampler retains the trace of the last program execution. Most
MH transitions use the last trace---with small changes---as a starting
point for the next execution. Since programs are (should be)
effect-free other than their stochastic behavior, the trace can be
represented as a database mapping points in the program (Addresses,
see @secref["instrument"]) to the stochastic primitives and their
results.

@racketblock[DB = (Hashof Address => Entry)]

The @racket[db-stochastic-ctx%] class manages the creation of the
current database for a program execution, given a database to
replay. The replay database is represented as the last database
(@racket[last-db]) and a delta (@racket[delta-db]) to avoid copying. 

The DB context also has a @racket[record-obs?] init field that
controls whether observations are entered in the database. Turning
@racket[record-obs?] off saves time and database space, but it means
that the set of observations must be kept constant between
executions---otherwise, not enough information is preserved to compare
the traces for the MH threshold. If changes to observations are rare
and predictable, an alternative to turning on @racket[record-obs?]
would be to just rerun the last trace (with an empty delta) and
automatically accept it. Beware, however, that changing the
observations could turn a possible trace into an impossible trace.

@defstruct*[entry
            ([zones list?]
             [dist dist?]
             [value any]
             [ll real?]
             [pinned? boolean?])]{

@racketblock[ll = (dist-pdf dist value #t)]

Entries with @racket[pinned?] field are only created in the database
if @racket[record-obs?] is true.
}


@section{MH Acceptance}

Let X, X' be full traces (dbs)

@verbatim{
X  = Xsame  U Xdiff  U Xstale  U Xfresh
X' = Xsame' U Xdiff' U Xstale' U Xfresh'

  Xsame = Xsame' -- part of trace that stayed same
  Xdiff != Xdiff' -- part of trace w/ changed params, kept value
                  -- OR is the directly perturbed part
  Xstale -- part of old trace not reused
  Xfresh' -- new part of new trace

  Note: Xfresh = empty, Xstale' = empty
}

MH acceptance ratio:

@verbatim{
  P(X' | Obs)   Q(X | X')
  ----------- * ---------
  P(x  | Obs)   Q(X' | X)
}

where Q is whole-trace proposal distribution

MH acceptance ratio from Bher paper:

@verbatim{
  P(X')   Kt(x|x', theta)   P(Xstale)
  ----- * --------------- * ----------
  P(X)    Kt(x'|x, theta)   P(Xfresh')
}

where x,x' are values of a single ERP.

Many things cancel, resulting in

@verbatim{
  P(Xdiff')   Kt(x|x', theta)
  --------- * ---------------
  P(Xdiff)    Kt(x'|x, theta)
}

So, need to accumulate

@verbatim{
  lldiff = log Kt(x|x', theta) - log Kt(x'|x, theta)
           + log P(Xdiff') - log P(Xdiff')
}

depending on only choices reused w/ different params.
