;; Copyright (c) 2014-2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/class
                     racket/match
                     gamble gamble/util/dnum))

@(define (wiki suffix . content)
   (apply hyperlink (format "https://en.wikipedia.org/wiki/~a" suffix) content))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble gamble/util/dnum racket/match))
@(the-eval '(random-seed 1))

@title[#:tag "util"]{Utilities}

@; ============================================================
@section[#:tag "dnum"]{Dimorphic Numbers}

@defmodule[gamble/util/dnum]

A dimorphic number (@racket[dnum?]) represents a nonnegative real. It has two variants:
@itemlist[

@item{@racket[(linear-dnum _x)], the @emph{linear space} representation,
represents the number @racket[_x], which can be either exact or inexact}

@item{@racket[(logspace-dnum _lx)], the @emph{logspace} representation,
represents the number @racket[(exp _lx)]; the number @racket[_lx] must be a flonum}

]
Operations on dnums produce logspace results if any argument is in logspace.

@defproc[(dnum? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a dnum, @racket[#f] otherwise.
}

@defproc[(linear-dnum [x (>=/c 0)]) dnum?]{

Produces a @racket[dnum] representing @racket[x] using the linear space variant.
}

@defproc[(logspace-dnum [lx flonum?]) dnum?]{

Produces a @racket[dnum] representing @racket[(exp lx)] using the logspace variant.
}

@deftogether[[
@defproc[(dnum-logspace? [dn dnum?]) boolean?]
@defproc[(dnum-linear? [dn dnum?]) boolean?]
]]{

Checks whether @racket[dn] is represented using the logspace or linear space
variant, respectively.
}

@deftogether[[
@defproc[(dnum->linear-real [dn dnum?]) real?]
@defproc[(dnum->logspace-real [dn dnum?]) real?]
]]{

Converts @racket[dn] to a real number in linear space or logspace, respectively.

@examples[#:eval the-eval
(dnum->linear-real (linear-dnum 2))
(dnum->logspace-real (linear-dnum 2))
(dnum->linear-real (logspace-dnum -1.0))
(dnum->logspace-real (logspace-dnum -1.0))
]}

@defproc[(dnum-zero? [dn dnum?]) boolean?]{

Returns @racket[#t] if @racket[dn] represents zero, @racket[#f] otherwise.

@examples[#:eval the-eval
(dnum-zero? (linear-dnum 2))
(dnum-zero? (linear-dnum 0))
(dnum-zero? (logspace-dnum -1.0))
(dnum-zero? (logspace-dnum -inf.0))
]}

@deftogether[[
@defproc[(dnum+ [dn1 dnum?] [dn2 dnum?]) dnum?]
@defproc[(dnum- [dn1 dnum?] [dn2 dnum?]) dnum?]
@defproc[(dnum* [dn1 dnum?] [dn2 dnum?]) dnum?]
@defproc[(dnum/ [dn1 dnum?] [dn2 dnum?]) dnum?]
]]{

Computes the sum, difference, product, or ratio of two dnums, respectively.

The @racket[dnum-] operation signals an error if the result would be negative.
The @racket[dnum/] operation signals an error on division by exact zero or if
the result would be @racket[+nan.0].
}

@deftogether[[
@defproc[(dnum-sum [dns (listof dnum?)]) dnum?]
@defproc[(dnum-product [dns (listof dnum?)]) dnum?]
]]{

Computes the sum or product of a list of dnums, respectively.
}

@deftogether[[
@defproc[(dnum=? [dn1 dnum?] [dn2 dnum?]) boolean?]
@defproc[(dnum<? [dn1 dnum?] [dn2 dnum?]) boolean?]
@defproc[(dnum<=? [dn1 dnum?] [dn2 dnum?]) boolean?]
]]{

Compares two dnums.
}


@; ============================================================
@section[#:tag "test-util"]{Utilities for Testing and Comparing Distributions}

@defproc[(samples-KS [samples sample-frame/c]
                     [ref (or/c dist? (-> real? real?))])
         real?]{

Calculates the
@wiki["Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov statistic}
of @racket[samples] with respect to the reference distribution or CDF @racket[ref].
The result is a measure of the goodness of fit of the samples to the distribution.

@examples[#:eval the-eval
(samples-KS
 (generate-samples (importance-sampler (model (sample (uniform-dist 0 1)))) 100)
 (uniform-dist 0 1))
(samples-KS
 (generate-samples (importance-sampler (model (sample (normal-dist 0 1)))) 100)
 (normal-dist 0.1 1))
]}

@defproc[(discrete-dist-error [dist1 discrete-dist?]
                              [dist2 discrete-dist?])
         (>=/c 0)]{

Returns a measure of the difference between two discrete
distributions. The result is the probability mass that would need to
be reassigned in order to transform @racket[dist1] into
@racket[dist2].

@examples[#:eval the-eval
(discrete-dist-error
 (discrete-dist ['A 3/5] ['B 2/5])
 (discrete-dist ['A 1/2] ['B 1/2]))
]

In the example above, @racket[1/10] of the probability mass of
@racket['A] in the first distribution would have to be shifted to
@racket['B] to transform the first distribution into the second.
}


@; ============================================================
@(close-eval the-eval)
