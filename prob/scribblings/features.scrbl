#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          racket/list
          (for-label racket/contract
                     math/distributions
                     prob))

@(define the-eval (make-base-eval))
@(the-eval '(require prob))

@title[#:tag "features"]{Probabilistic Features}

@section[#:tag "erps"]{Elementary Random Procedures}

An elementary random procedure (@deftech{ERP}) returns a value drawn
from some distribution each time it is called.

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


@section[#:tag "mem"]{Stochastic Memoization}

@defproc[(mem [f procedure?])
         procedure?]{

Returns a memoized version of @racket[f].

@examples[#:eval the-eval
(define f (mem (lambda (n) (d2))))
(f 1)

(code:line (f 1) (code:comment "calling (f 1) again will get the same value"))
(for/list ([i 10]) (f i))
(for/list ([i 10]) (f i))
]

TODO: document interaction between memoized functions and dynamic
extents of solvers.
}


@section[#:tag "util"]{Utilities}

@defproc[(probability? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is a real number between 0 and 1
(inclusive), @racket[#f] otherwise.
}

@defparam[verbose? v? boolean?]{

Parameter that controls whether informative messages are printed by
solvers and ERPs.
}

@defproc[(repeat [thunk (-> any/c)]
                 [n exact-nonnegative-integer?])
         list?]{

Calls @racket[thunk] @racket[n] times, accumulating the results in a
list.

@examples[#:eval the-eval
(repeat flip 10)
]
}

@defproc[(lag [thunk (-> any/c)]
              [n exact-positive-integer?])
         (-> any/c)]{

Lags a @tech{sampler}; when the resulting thunk is called, the given
@racket[thunk] is called @racket[n] times and only the last value is
returned.
}


@(close-eval the-eval)
