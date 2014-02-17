#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          racket/list
          (for-label racket/contract
                     prob))

@(define the-eval (make-base-eval))
@(the-eval '(require prob))

@title[#:tag "features"]{Probabilistic Features}

@section[#:tag "erps"]{Elementary Random Procedures}

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

@defproc[(randn [n exact-positive-integer?])
         exact-nonnegative-integer?]{

Returns an integer drawn uniformly from [0, @racket[n]).
}


@section[#:tag "mem"]{Stochastic Memoization}

@defproc[(mem [f procedure?])
         procedure?]{

Returns a memoized version of @racket[f].

TODO: document interaction between memoized functions and dynamic
extents of solvers.
}


@section[#:tag "util"]{Utilities}

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
