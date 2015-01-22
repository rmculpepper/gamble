;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     gamble))

@title[#:tag "instrument"]{Instrumenter}

The instrumenter performs two tasks:
@itemlist[
@item{call-site instrumentation for @secref["mh-sampler"]}
@item{observable-context instrumentation for @racket[observe]}
]

@defmodule[gamble/private/context]

Contains the representation of contexts, the parameter that holds the
current context, and some macros and functions for manipulating
contexts.


@section{Call-Site Instrumentation}

How to represent an Address (ie, a point in evaluation, reasonably stable 
across runs with different random choices)?

@bold{Version 0 (old):}

An Address0 is a @racket[(listof CallSite)], the list of call sites in
the context (ie, continuation), most recent first.

Note: call sites, not functions. Consider @racket[(define (f) (g (h)
(h)))]---need to distinguish separate calls to @racket[h]. But what if
function changes? Well, what if argument changes? We're ignoring the
latter, why not ignore the former too?

Store address in context using @racket[with-continuation-mark] (WCM)
and retrieve using @racket[current-continuation-marks] (CCM).

Problem: tail calls. If @racket[f] tail-calls itself, then second WCM
overwrites first; calls within the two activations of @racket[f] will
have colliding addresses. Also if @racket[f] tail-calls @racket[g]
then @racket[g] tail-calls @racket[f], collisions in two @racket[f]
activations. And so on.

@bold{Version 1 (old):}

An Address is a @racket[(listof CallSequence)], a list of tail-call
sequences, most recent first.

A CallSequence is an improper list of CallSite: the list of tail calls
(most recent first) together with the non-tail call they start from at
the end.

Issue: May change space complexity of program---but it needs to make finer
distinctions than original program, so somewhat justified. Maybe devise ad-hoc
representation optimizations: eg, RLE for self-tail-calling functions.

@bold{Version 2 (current):}

An Address is a @racket[(listof CallSite)], a list of call sites, most
recent first, with no tail-call collapsing.

Use parameter instead of continuation marks. Parameter access seems to
be specially optimized in Racket runtime, so it is considerably faster
than @racket[current-continuation-marks].

Potential issue: will parameters cause problems for enumerate?

Another possibility is @racket[call-with-immediate-continuation-mark],
which in my microbenchmarks is slightly faster than parameters. It
would also allow for safety checking. The downside is all reads to the
context have to be in tail position wrt a call from instrumented
code. So eg flip impl would have to either grab context or call
(current-ERP) in tail position. Or flip could be defined in
instrumented module. But would contracts interfere? (Probably not, if
both definition and use site are instrumented.)


@section{Observable Contexts Instrumentation}

The goal is to implement @racketblock[(observe CC[(sample d)] v)] as
@racketblock[CC[(observe-sample d CC^-1[v])]].

Conditioning contexts (CC) are defined thus:

@verbatim{
CC ::=                   -- CCFrame rep:
       []
    |  (+ v CC)          -- v
    |  (- CC)            -- '-
    |  (cons CC e)       -- 'car
    |  (cons v CC)       -- 'cdr
    |  (reverse CC)      -- 'reverse
    |  ... more invertible functions that don't affect density
           (eg, * is bad, I think)
}

A ConditioningContext = @racket[(list CCFrame ...)].

The CC is implicitly stored in the continuation mark stack under the
obs-mark key. The value of the observation is stored in the observing?
parameter.

Note: in order to cooperate with unannotated code---ie, to detect
uninstrumented non-conditionable frames in context---store the CC one
frame back in the context. So in general:

@verbatim{
  at CC[e], cms(CC) = (cons 'unknown 〚CC〛)
}

So if
@racketblock[
  CC[(f)] -> CC[B[(g)]]
          -> CC[B[CC*[e]]]
]
where @racket[B] stands for bad frame, non-conditionable, then
@racket[cms(CC[B[CC*[]]])] will have an internal @racket['unknown]
frame indicating that we cannot invert the context to adjust the
observed value.

(Need to explain this better. TODO: reread Jay's paper on serializable
continuations for servlets, which also wants to detect bad frames.)

So sample function will just overwrite top frame using WCM. Better to
overwrite than ignore first @racket['unknown], since overwriting also catches
@racket[CC[B[(observe ...)]]].

@bold{Major problem: How to check that condition is applied?}

Idea 1: @racket[(observe e v)] also checks @racket[e] evaluates to
@racket[v] (@racket[equal?]).  
Problem: floating-point arithmetic may mean equality doesn't hold.

Idea 2: check approximate numeric equality.
Problem: .... eh, maybe, but what tolerance?

Idea 3: Mutable flag checked when condition used by sample.
Problem: mutation bad for @racket[enumerate]; given @racket[cons], can
have many subconditions.

Idea 4: Check structure of @racket[v] up to numeric leaves, then use
@itemlist[
@item{4a: approximate equality on reals}
@item{4b: mutable counter for reals}
]

@bold{What constitutes an unused condition, anyway?}

For example, in @racket[(observe (cons 1 (bernoulli)) '(1 . 1))], the
first part of the condition is ``unused'' but still satisfied.


@bold{Observation failure: error vs fail}

For continuous RVs, generally expect conditioning to always succeed
(or only fail by setting trace weight to 0). But for some examples, eg
PCFG, want something more like automatic @racket[fail]
propoagation. Maybe want two variants. check-observe seems more useful
in second variant.

@bold{More challenges:}

@itemlist[
@item{@racket[(let ([t conditionable-expr]) (+ v t))] --- cf laziness
w/ 2 kinds of forcing contexts}
@item{conditions on accumulated lists (eg, @racket[for/list] expansion)}
@item{@racket[(begin (define X conditionable-expr) (observe X v))]}
@item{conditioning through lists/vectors/tables (indexed RVs)}
@item{smarter @racket[+]: pick conditionable expr even if not last}
]

@bold{Optimizations:}

@itemlist[
@item{limit instrumentation to functions that could be called in CC}
]
