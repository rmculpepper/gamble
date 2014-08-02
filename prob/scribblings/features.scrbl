;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
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
perturbations. In an @racket[enumerate] context, an ERP will fork
its context and potentially the consequences of all values in its
distribution.

@defproc[(sample [dist dist?]) any]{

Returns a sample from @racket[dist]. The @racket[sample] procedure
cooperates with the enclosing sampler/solver, unlike
@racket[dist-sample].
}

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

@defproc[(observe-at [dist dist?] [value any/c])
         void?]{

Represents observing the value of a random variable distributed as
@racket[dist] being @racket[value]. Typically, the effect is to adjust
the likelihood of the current sampler/solver execution.
}

@defproc[(mem [f procedure?])
         procedure?]{

Returns a memoized version of @racket[f].

In general, a memoized function must not be called outside of the
dynamic extent of the sampler/solver context in which it was
created. See @seclink["nesting"] for more discussion.

@examples[#:eval the-eval
(define f (mem (lambda (n) (bernoulli))))
(f 1)

(code:line (f 1) (code:comment "calling (f 1) again will get the same value"))
(for/list ([i 10]) (f i))
(for/list ([i 10]) (f i))
]
}


@; ------------------------------------------------------------
@section[#:tag "erps"]{Elementary Random Procedures}

An @deftech[#:key "erp"]{elementary random procedure (ERP)} returns a
value drawn from some distribution each time it is called.

@subsection[#:tag "discrete-erps"]{Discrete ERPs}

@defproc[(flip [p (real-in 0 1) 1/2])
         boolean?]{

Returns @racket[#t] with probability @racket[p], @racket[#f] with
probability @racket[(- 1 p)].

Equivalent to @racket[(= 1 (sample (bernoulli-dist p)))].
}

@defproc*[([(discrete [n exact-positive-integer?])
            exact-nonnegative-integer?]
           [(discrete [weighted-vals (listof (cons/c any/c (>=/c 0)))])
            any/c])]{

In the first form, returns an integer drawn uniformly from [0, @racket[n]).

In the second form, @racket[(discrete (list (cons _val _weight) ...))]
returns a value drawn from the @racket[_val]s with probability
proportional to the corresponding @racket[_weight].

Equivalent to
@racket[(sample (make-discrete-dist (for/hash ([i n]) (values i (/ n)))))]
and @racket[(sample (make-discrete-dist weighted-vals))], respectively.
}

@defproc[(discrete* [vals (non-empty-listof any/c)]
                    [weights (non-empty-listof (>/c 0))])
         any/c]{

Returns a value drawn from @racket[vals], with the probability of each
element in @racket[vals] weighted by the corresponding element in
@racket[weights], if present.

Equivalent to @racket[(sample (make-discrete-dist* vals weights))].
}


@subsection[#:tag "integer-erps"]{Integer-Valued ERPs}

@defproc[(bernoulli [p (real-in 0 1) 1/2])
         (or/c 1 0)]{

Returns @racket[1] with probability @racket[p], @racket[0] with
probability @racket[(- 1 p)].

Equivalent to @racket[(sample (bernoulli-dist p))].
}

@defproc[(binomial [count exact-nonnegative-integer?]
                   [p (real-in 0 1)])
         exact-nonnegative-integer?]{

Equivalent to @racket[(sample (binomial-dist count p))].
}

@defproc[(geometric [p (real-in 0 1) 1/2])
         exact-nonnegative-integer?]{

Equivalent to @racket[(sample (geometric-dist p))].
}

@defproc[(poisson [mean (>/c 0)])
         exact-nonnegative-integer?]{

Equivalent to @racket[(sample (poisson-dist mean))].
}


@subsection[#:tag "real-erps"]{Real-Valued ERPs}

@defproc[(beta [a (>/c 0)]
               [b (>/c 0)])
         real?]{

Equivalent to @racket[(sample (beta-dist a b))].
}

@defproc[(cauchy [mode real?]
                 [scale (>/c 0) 1])
         real?]{

Equivalent to @racket[(sample (cauchy-dist mode scale))].
}

@defproc[(exponential [mean (>/c 0)])
         real?]{

Equivalent to @racket[(sample (exponential-dist mean))].
}

@defproc[(gamma [shape (>/c 0) 1]
                [scale (>/c 0) 1])
         real?]{

Equivalent to @racket[(sample (gamma-dist shape scale))].
}

@defproc[(logistic [mean real? 0]
                   [scale (>/c 0) 1])
         real?]{

Equivalent to @racket[(sample (logistic-dist mean scale))].
}

@defproc[(normal [mean real? 0]
                 [stddev (>/c 0) 1])
         real?]{

Equivalent to @racket[(sample (normal-dist mean stddev))].
}

@defproc*[([(uniform)
            real?]
           [(uniform [hi real?])
            real?]
           [(uniform [lo real?] [hi real?])
            real?])]{

Equivalent to @racket[(sample (uniform-dist lo hi))].
}


@(close-eval the-eval)
