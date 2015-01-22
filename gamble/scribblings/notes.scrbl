;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     (only-in racket/base map)
                     (except-in gamble map)
                     gamble/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble))

@title[#:tag "notes"]{Notes, Plans, Limitations, and Known Bugs}

Users must various language features, including side effects
(particularly mutation) and certain higher-order functions in certain
situations.

My code is not written numerically carefully at all. It will need
review.

There's a very preliminary typed lang that builds on Typed Racket. It
will probably be necessary to add some of @racketmodname[gamble]'s
primitives to the type environment (ie, assert, not typecheck). Also,
there's a binding issue that prevents definitions from being used
outside the module.

@section{Notes on @racket[mh-sampler]}

The address-tracking part is in pretty good shape. The ML part is
lacking.

Currently the ``proposal function'' just picks an ERP choice at random
(uniformly from the list of ERP choices from the database) and deletes
it. That corresponds to the naive ``just re-evaluate it'' strategy
mentioned in @cite{Church}. It's pretty bad.

One possible path for improvement is adding automatic differentiation
like @cite{Bher}, but it looks messy. Specifically, it looks like it
would complicate interoperation with libraries.

The context of an ERP use must not contain any higher-order functions
imported from uninstrumented modules, such as Racket's @racket[map]
function. Higher-order functions that take a single function argument
and apply it at most once (such as @racket[call-with-input-file])
are safe, however.

@subsection{Address representation}

Addresses are collected in continuation marks rather than as passed as
an additional argument. That avoids changing function signature,
simplifying interop.

Version 1 (bad): @italic{An Address is a list of CallSite---the list
of call sites in the context (continuation), most recent first.}

Note: call site, not function. Consider @racket[(define (f) (g (h)
(h)))]---need to distinguish separate calls to @racket[h]. (But what
if function changes? Well, what if an argument changes? We're ignoring
the latter, so might as well ignore the former too.)

Store address in context using @racket[with-continuation-mark] (WCM)
and retrieve using @racket[current-continuation-marks] (CCM).

Problem: tail calls. If a function @racket[f] tail-calls itself, then
second WCM overwrites first; calls within the two activations of
@racket[f] will have colliding addresses. Also if @racket[f]
tail-calls @racket[g] then @racket[g] tail-calls @racket[f], we get a
collision from the two @racket[f] activations. And so on.  (See
@racket[sum-n-flips*] in @tt{examples.rkt}.)

Version 2 (current): @italic{An Address is a list of CallSequence---a
list of tail-call sequences, most recent first. A CallSequence is an
improper list of CallSite.}

A CallSequence is started by a non-tail call; that call site is the
end of the improper list. Each subsequent tail call gets added to the
list.

Issue: This may change the space complexity of the program. But it
needs to make finer distinction than original program, so it's
somewhat justified.

It might be possible to devise ad-hoc representation optimizations,
like run-length encoding for self-tail-calling functions. CFA could
probably help.

@subsection{Instrumentation}

Call-sites are instrumented after expansion, so macros like
@racket[for/list] get instrumented but higher-order functions like
@racket[map] don't, since they're defined in uninstrumented modules.


@section{Notes on @racket[enumerate]}

The context of an ERP use must not include any uses of
@racket[parameterize], which doesn't cooperate correctly with
delimited continuations.

@cite{EPP} talks about 
@itemlist[

@item{@bold{reification and reflection}---Working, using @racket[enumerate]
to reify and @racket[discrete-from-enumeration] to reflect. See
example @tt{xor-flips} (exponential time) and @tt{xor-flips*}
(linear).

Question: is the @tt{memo} function at the end of@cite{EPP} section 3
just naive memoization?}

@item{@bold{importance sampling with look-ahead}---Not implemented
yet. Looks promising.}

@item{@bold{lazy evaluation}---I think the implementation of
@racket[mem] I have for is a generalization of their @tt{letlazy}. See
@tt{flips-all-true} and @tt{flips-all-true*} for supporting evidence.}

]

I made up some stuff to handle countable distributions and infinite
trees by pruning low-relevance paths.

Note: pruning only happens on terminated paths. There's no way to
reject a path based on the choices it's made so far. How would such a
condition be formulated, and how would it be implemented?

@section[#:tag "nesting"]{Nesting Samplers/Solvers}

What nestings are okay? I've put more thought and effort into the
@racket[enumerate] nestings; there's some basic stuff missing for
@racket[mh-sampler] nesting.

@itemlist[

@item{@racket[enumerate] within @racket[enumerate]

      Should work, including @racket[mem] (mostly). An
      outer-mem-function used in the inner @racket[enumerate] should
      work (ie, fork the outer context), and an inner-mem-function
      that escapes to the outer @racket[enumerate] should raise an
      out-of-context error.

      TODO: Handle the HO case where an inner-mem-function is passed
      to an outer-mem-function which then calls it. That should be
      considered the inner-mem-function escaping its context.}

@item{@racket[enumerate] within @racket[mh-sampler]

      Should work, except for some @racket[mem] cases.

      TODO: An outer-mem-function called in inner @racket[mh-sampler]
      should use outer (MH) ERP impl.

      An inner-mem-function that escapes should raise an
      out-of-context error.

      TODO: same HO case as above.

      TODO: if @racket[mh-sampler] gets caching, disable within
      @racket[enumerate].}

@item{@racket[mh-sampler] within @racket[enumerate]

      Should work, except for @racket[mem].

      TODO: implement out-of-context check for mem-functions created
      within @racket[mh-sampler].

      TODO: prohibit outer-mem-function from being called from
      @racket[mh-sampler]. Also, same cases as above.}

@item{@racket[mh-sampler] within @racket[mh-sampler]

      Inner MH database won't affect outer MH database. Thus
      outer-mem-function used in inner @racket[mh-sampler] won't
      retain value.}
]

@section{Plans}

Improve the syntax of @racket[mh-sampler] etc to support conditions
intermixed with definitions, subject to proper scoping. That would
make it easier (potentially) to reject samples at an earlier stage.

Module/unit/structure/functor-like thing for models. Should be able to
define a model once and then plug it into any kind of sampler/solver
framework, compose it with other models, add conditions, apply
transformations (eg, replace definitions a la traits or mixin
modules?), etc.

Address-based caching.
              
@(close-eval the-eval)
