;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          racket/list
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

@examples[#:eval the-eval
(repeat (lag (let ([n 0]) (lambda () (set! n (add1 n)) n)) 10) 10)
]
}


@section[#:tag "viz"]{Visualization Utilities}

@defmodule[prob/viz]

This module provides very basic utilities for visualizing data. For a
much more flexible and comprehensive visualization support, see the
@racketmodname[plot] library.

@defproc[(hist [vals (listof any/c)])
         @#,elem{display}]{

Plots a histogram of @racket[vals].

@examples[#:eval the-eval
(hist (list 1 2 3 1 1))
]
}


@(close-eval the-eval)
