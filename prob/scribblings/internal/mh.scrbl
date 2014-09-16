;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     prob))

@title[#:tag "mh-sampler"]{Metropolis-Hastings Implementation}

@defmodule[prob/private/prob-mh]

Contains the implementation of the Metropolis-Hastings framework,
realized by @racket[mh-sampler]. The MH sampler can use several
different kinds of transitions, and it relies on the call-site
instrumentation done by the instrumenter (@secref["instrument"]).

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
