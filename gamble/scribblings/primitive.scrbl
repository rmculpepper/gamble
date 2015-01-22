;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     gamble
                     gamble/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble (only-in gamble/viz [hist-pict hist])))

@title[#:tag "primitive"]{Primitive Stochastic Functions}

This language consists of four primitive stochastic functions:
@itemlist[
@item{@racket[sample] --- draw a sample from a @seclink["dist"]{distribution}}
@item{@racket[mem] --- apply memoization to a (stochastic) function}
@item{@racket[fail] --- represents a predicate observation failure;
abort the current program execution}
@item{@racket[observe-sample] --- represents a point observation;
weights the current program evaluation by the likelihood of drawing
the given value from the given distribution}
]
The implementation of these features depends on the sampler/solver
context in which they are executed. In a @racket[mh-sampler] context,
for example, @racket[sample] might reuse a choice from a previous run,
subject to random perturbations. In an @racket[enumerate] context,
@racket[sample] will fork its context and potentially the consequences
of all values in its distribution.


@defproc[(sample [dist dist?]) any]{

Returns a sample from @racket[dist]. The @racket[sample] procedure
cooperates with the enclosing sampler/solver, unlike
@racket[dist-sample].

See also @secref["erps"].
}

@defproc[(mem [f procedure?])
         procedure?]{

Returns a memoized version of @racket[f].

In general, a memoized function must not be called outside of the
dynamic extent of the sampler/solver context in which it was
created.

@;{ FIXME: See @seclink["nesting"] for more discussion. }

@examples[#:eval the-eval
(define f (mem (lambda (n) (bernoulli))))
(f 1)

(code:line (f 1) (code:comment "calling (f 1) again will get the same value"))
(for/list ([i 10]) (f i))
(for/list ([i 10]) (f i))
]
}

@defproc[(fail [reason any/c #f]) any]{

Used to express observation failure. When used within a sampler or
solver, it typically causes the sampler/solver to try again with
different values for the previous choices.

For example, the following function models two coin flips where at
least one of them is known to be heads (@racket[#t]):

@racketblock[
(define (flip-two-coins/one-heads)
  (define A (flip))
  (define B (flip))
  (unless (or A B) (fail))
  (list A B))
]
}

@defproc[(observe-sample [dist dist?] [value any/c])
         void?]{

Represents observing the value of a random variable distributed as
@racket[dist] being @racket[value]. Typically, the effect is to adjust
the likelihood of the current sampler/solver execution.
}


@(close-eval the-eval)
