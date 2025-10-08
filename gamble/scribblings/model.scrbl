;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base racket/contract gamble))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble))
           @(the-eval '(random-seed 1))

@title[#:tag "model"]{Probabilistic Models}

@defproc[(model? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a probabilistic model produced by
@racket[model], @racket[#f] otherwise.
}

@defform[(model definition-or-expression ...+)]{

Expresses a probabilistic model as a computation involving random variables and
observations. The model's definitions and expressions may use the operations
described in @secref["model-ops"]. The model body must end with a single-valued
expression that represents the model's result.
}

@defproc[(run-model [m model?]) any/c]{

Runs the model @racket[m], returning the result of its computation, and printing
the log likelihood of the model's observations.

This operation may also be used within another model. In that case, it returns
the result of the inner model's computation and incorporates the inner model's
observations into the outer model's likelihood.
}

@; ------------------------------------------------------------
@section[#:tag "model-ops"]{Model-Level Operations}

The operations described in this section are only allowed within @racket[model]
expressions. References outside of a @racket[model] raise a syntax error.

@; ----------------------------------------
@subsection[#:tag "model-rvs"]{Random Variables}

@defproc[(sample [dist dist?] [tag any/c #f]) any]{

Returns a value distributed according to @racket[dist]. The @racket[sample]
procedure cooperates with the enclosing sampler/solver, unlike
@racket[dist-sample]. The @racket[tag] is used by some features of this library
to identify or classify the random variable.

Every evaluation of a @racket[sample] expression within a model creates a
@deftech{primitive random variable} --- whether or not the value is bound to a
Racket variable. Each random variable receives an internal identifier called an
@deftech{address}; addresses are not exposed to programs but they are visible
through logging. Addresses are determined by control flow, but they are
relatively independent of program values. That stability justifies considering
the ``same'' random variable to exist across multiple executions. This library
uses a variant of the addressing scheme of @cite["LMH"].

A random variable's tag must not change from one execution of a model to
another. This restriction is not always enforced, but @racket[mcmc-sampler]
requires it. Note that the tag expression is not required to be constant; one
@racket[sample] call may correspond to multiple random variables. The following
example illustrates the tag rules:

@racketblock[
(model
 (code:comment "OK: constant tag")
 (define n (sample (binomial-dist 10 1/2) 'count))
 (define s
   (for/sum ([i (in-range n)])
     (code:comment "OK: not constant expression, but same RV gets same tag")
     (sample (uniform-dist 0 1) `(lo ,i))))
 (define t
   (for/sum ([i (in-range n 10)])
     (code:comment "BAD: tag varies based on value of n")
     (sample (uniform-dist 0 2) `(hi ,i))))
 (+ s t))
]}


@; ----------------------------------------
@subsection[#:tag "model-obs"]{Observations}

@defproc[(observe [dist dist?] [value any/c]) void?]{

Represents an @deftech{observation} of the given @racket[value] from the
distribution @racket[dist]. The effect is to adjusts the likelihood of the
current model execution.

Equivalent to @racket[(score (dist-density dist value))], except that some
features of this libary may benefit from knowing the observation distribution.
}

@defproc[(observe* [dist dist?] [vs vector?]) void?]{

Like @racket[observe], but represents multiple observations.

Equivalent to @racket[(for ([v vs]) (observe dist v))].
}

@defproc[(score [s (or/c real? dnum?)]) void?]{

Adjusts the likelihood of the current model execution by @racket[s].
If @racket[s] is a real number, then it is interpreted as a logspace quantity.
}

@defproc[(fail [reason any/c #f]) any]{

Used to express observation failure. When used within a sampler or solver, it
typically causes the sampler/solver to try again with different values for the
previous choices.

Equivalent to @racket[(score -inf.0)], except that @racket[reason] may be used
for debugging.

For example, consider the following model of two coin flips where at least one
of them is known to be heads (@racket[#t]):

@interaction[#:eval the-eval
(enumerate
 (model
  (define A (sample (boolean-dist 1/2)))
  (define B (sample (boolean-dist 1/2)))
  (unless (or A B) (fail))
  (list A B)))
]}


@;{
@; ----------------------------------------
@subsection[#:tag "model-other"]{Other Operations}

@defproc[(mem [f procedure?]) procedure?]{

Returns a memoized version of @racket[f].

In general, a memoized function must not be called outside of the
dynamic extent of the sampler/solver context in which it was
created.

@examples[#:eval the-eval
(define f (mem (lambda (n) (sample (bernoulli-dist 1/2)))))
(f 1)

(code:line (f 1) (code:comment "calling (f 1) again will get the same value"))
(for/list ([i 10]) (f i))
(for/list ([i 10]) (f i))
]}
}

@; ----------------------------------------
@section[#:tag "model-defs"]{Defining Model-Level Operations}

@defform[(begin-model-definitions definition ...)]{

Defines new model-level functions. These functions can use model-level
operations like @racket[sample] and @racket[observe], but the functions can only
be used within @racket[model] expressions or other model-level definitions.

Each @racket[definition] must have the syntactic shape of a function definition,
and keyword functions are not currently supported.

@examples[#:eval the-eval
(begin-model-definitions
  (define (flip [p 1/2]) (sample (boolean-dist p))))
(define geom/m
  (model (let geom () (if (flip) 0 (add1 (geom))))))
(enumerate #:stop 1e-3 geom/m)
]}

@(close-eval the-eval)
