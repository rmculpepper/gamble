#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          racket/list
          (for-label racket/contract
                     prob
                     prob/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require prob prob/viz))

@title[#:tag "notes"]{Notes, Limitations, and Known Bugs}

Side effects, especially mutation, should be avoided.

Solvers currently don't nest and don't mix well, particularly
when it comes to memoized functions.

@section{Notes on @racket[mh-sampler]}

Currently the ``proposal function'' just picks an ERP choice at random
(uniformly from the list of ERP choices from the database) and deletes
it. That corresponds to the naive ``just re-evaluate it'' strategy
mentioned in @cite{Church}.

The context of an ERP use must not contain any higher-order functions
imported from uninstrumented modules, such as Racket's @racket[map]
function. Higher-order functions that take a single function argument
and apply it at most once (such as @racket[call-with-input-from-file])
are safe, however.

@section{Notes on @racket[enumerate]}

The context of an ERP use must not include any uses of
@racket[parameterize]; it doesn't cooperate correctly with delimited
continuations.


@(close-eval the-eval)
