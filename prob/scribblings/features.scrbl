;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     math/distributions
                     prob
                     prob/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require prob (only-in prob/viz [hist-pict hist])))

@title[#:tag "features"]{Probabilistic Features}

The probabilistic features of this language consist of @tech[#:key
"erp"]{elementary random procedures (ERPs)}, a memoizer, and a set of
sampler/solvers.

At a high level, every ERP when called produces a value from an
associated probability distribution. The precise behavior of an ERP,
however, depends on the sampler/solver context it is executed
under. In a @racket[mh-sampler] context, for example, an ERP might
reuse a choice from a previous run, subject to random
perturbations. In an @racket[enumeration] context, an ERP will fork
its context and potentially the consequences of all values in its
distribution.

@section[#:tag "erps"]{Elementary Random Procedures}

An @deftech[#:key "erp"]{elementary random procedure (ERP)} returns a
value drawn from some distribution each time it is called.

@subsection[#:tag "discrete-erps"]{Discrete ERPs}

@defproc[(flip [p probability? 1/2])
         boolean?]{

Returns @racket[#t] with probability @racket[p], @racket[#f] with
probability @racket[(- 1 p)].
}

@defproc[(d2 [p probability? 1/2])
         (or/c 1 0)]{

Returns @racket[1] with probability @racket[p], @racket[0] with
probability @racket[(- 1 p)].
}

@defproc*[([(discrete [n exact-positive-integer?])
            exact-nonnegative-integer?]
           [(discrete [vals (non-empty-listof any/c)])
            any/c]
           [(discrete [vals (non-empty-listof any/c)]
                      [weights (non-empty-listof (>/c 0))])
            any/c])]{

In the first form, returns an integer drawn uniformly from [0, @racket[n]).

In the second and third, returns a value drawn from @racket[vals],
with the probability of each element in @racket[vals] weighted by the
corresponding element in @racket[weights], if present.

See also @racket[discrete-dist].
}

@defproc[(discrete-from-enumeration [dist (listof (list/c any/c (>/c 0)))])
         any/c]{

Given @racket[(list (list _val _weight) ...)], equivalent to
@racket[(discrete (list _val ...) (list _weight ...))].
}


@subsection[#:tag "integer-erps"]{Integer-Valued ERPs}

@defproc[(binomial [count exact-nonnegative-integer?]
                   [p probability?])
         exact-nonnegative-integer?]{

Returns an integer drawn from @racket[(binomial-dist count p)].
}

@defproc[(geometric [p probability? 1/2])
         exact-nonnegative-integer?]{

Returns an integer drawn from @racket[(geometric-dist p)].
}


@defproc[(poisson [mean (>/c 0)])
         exact-nonnegative-integer?]{

Returns an integer drawn from @racket[(poisson-dist p)].
}


@subsection[#:tag "real-erps"]{Real-Valued ERPs}

@defproc[(beta [a (>/c 0)]
               [b (>/c 0)])
         real?]{

Returns a real drawn from @racket[(beta-dist a b)].
}

@defproc[(cauchy [mode real?]
                 [scale (>/c 0) 1])
         real?]{

Returns a real drawn from @racket[(cauchy-dist mode scale)].
}

@defproc[(exponential [mean (>/c 0)])
         real?]{

Returns a real drawn from @racket[(exponential-dist mean)].
}

@defproc[(gamma [shape (>/c 0) 1]
                [scale (>/c 0) 1])
         real?]{

Returns a real drawn from @racket[(gamma-dist shape scale)].
}

@defproc[(logistic [mean real? 0]
                   [scale (>/c 0) 1])
         real?]{

Returns a real drawn from @racket[(logistic-dist mean scale)].
}

@defproc[(normal [mean real? 0]
                 [stddev (>/c 0) 1])
         real?]{

Returns a real drawn from @racket[(normal-dist mean stddev)].
}

@defproc*[([(uniform)
            real?]
           [(uniform [hi real?])
            real?]
           [(uniform [lo real?] [hi real?])
            real?])]{

Returns a real drawn from @racket[(uniform-dist)],
@racket[(uniform-dist hi)], or @racket[(uniform-dist lo hi)],
respectively.
}


@section[#:tag "fail"]{Condition Failure}

@defproc[(fail [reason any/c #f]) any]{

Used to express condition failure. When used within a sampler or
solver, it typically causes the sampler/solver to try again with a
different set of choices.

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


@section[#:tag "table"]{Indexed Tables}

@defform*[[(table ([var-id sequence-expr] ...+) maybe-lazy body ...+)
           (table (var-id ...+) body ...+)]
          #:grammar ([maybe-lazy (code:line)
                                 (code:line #:lazy)])
          #:contracts ([sequence-expr sequence?])]{

Creates an indexed collection that acts like a function; the function
takes as many arguments as there are @racket[var-id]s.

In the first form, the table is finite, and it is eagerly populated
with an entry for every combination of elements from each
@racket[sequence-expr]. (Each @racket[sequence-expr] must be finite.)
If a finite table is addressed with indexes that
do not occur in @racket[sequence-expr], an error is raised.

@examples[#:eval the-eval
(define F (table ([i 10] [j 20]) (flip)))
(F 0 0)
(F 2 3)
(F 7 13)
]

If the @racket[#:lazy] keyword appears after the variable binding
sequence, then the table's entries are not evaluated until they are
looked up (see also @racket[pdelay]).

@examples[#:eval the-eval
(define LF (table ([i 10] [j 20]) #:lazy (printf "flipping!\n") (flip)))
(LF 0 0)
(LF 0 0)
]

In the second form, the table is conceptually infinite, and it is
lazily populated as entries are requested. This form is equivalent to
@racket[(mem (lambda (var-id ...) body ...))].
}


@section[#:tag "mem"]{Memoization}

@defproc[(mem [f procedure?])
         procedure?]{

Returns a memoized version of @racket[f].

In general, a memoized function must not be called outside of the
dynamic extent of the sampler/solver context in which it was
created. See @seclink["nesting"] for more discussion.

@examples[#:eval the-eval
(define f (mem (lambda (n) (d2))))
(f 1)

(code:line (f 1) (code:comment "calling (f 1) again will get the same value"))
(for/list ([i 10]) (f i))
(for/list ([i 10]) (f i))
]
}

@(close-eval the-eval)
