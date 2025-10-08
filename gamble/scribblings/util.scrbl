;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/class
                     racket/match
                     gamble gamble/util/dnum gamble/pict))

@(define (wiki suffix . content)
   (apply hyperlink (format "https://en.wikipedia.org/wiki/~a" suffix) content))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble gamble/util/dnum gamble/pict racket/match))
@(the-eval '(random-seed 1))

@title[#:tag "util"]{Utilities}

@; ============================================================
@section[#:tag "dnum"]{Dimorphic Numbers}

@defmodule[gamble/util/dnum]

A @deftech{dnum} (dimorphic number) represents a nonnegative real.
It has two variants:
@itemlist[

@item{@racket[(linear-dnum _x)], the @emph{linear space} representation,
represents the number @racket[_x], which can be either exact or inexact}

@item{@racket[(logspace-dnum _lx)], the @emph{logspace} representation,
represents the number @racket[(exp _lx)]; the number @racket[_lx] must be a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{flonum}}

]
Operations on dnums produce logspace results if any argument is in logspace.

@defproc[(dnum? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{dnum}, @racket[#f] otherwise.
}

@defproc[(linear-dnum [x (>=/c 0)]) dnum?]{

Produces a @tech{dnum} representing @racket[x] using the linear space variant.
}

@defproc[(logspace-dnum [lx flonum?]) dnum?]{

Produces a @tech{dnum} representing @racket[(exp lx)] using the logspace variant.
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

Computes the sum, difference, product, or ratio of two @tech{dnums},
respectively.

The @racket[dnum-] operation signals an error if the result would be negative.
The @racket[dnum/] operation signals an error on division by exact zero or if
the result would be @racket[+nan.0].
}

@deftogether[[
@defproc[(dnum-sum [dns (listof dnum?)]) dnum?]
@defproc[(dnum-product [dns (listof dnum?)]) dnum?]
]]{

Computes the sum or product of a list of @tech{dnums}, respectively.
}

@deftogether[[
@defproc[(dnum=? [dn1 dnum?] [dn2 dnum?]) boolean?]
@defproc[(dnum<? [dn1 dnum?] [dn2 dnum?]) boolean?]
@defproc[(dnum<=? [dn1 dnum?] [dn2 dnum?]) boolean?]
]]{

Compares two @tech{dnums}.
}

@; ============================================================
@section[#:tag "pict"]{Visualizing Distributions and Sample Frames}

@defmodule[gamble/pict]

@defproc[(dist->pict [dist dist?]
                     [ref-dist (or/c #f numeric-dist?) #f])
         pict?]{

Produces a visualization of @racket[dist] as a pict, according the following cases:
@itemlist[

@item{If @racket[(integer-dist? dist)], then the pict contains a plot of the
distribution's values and CDF.}

@item{If @racket[(real-dist? dist)], then the pict contains a plot of the
distribution's PDF (density) and CDF.}

@item{If @racket[dist] is a discrete distribution containing only real values and a
sufficient number of distinct values, then the pict contains a plot of the
values, kernel density estimators, and the empirical CDF.}

@item{If @racket[dist] is a finite distribution that does not satisfy any of the
criteria above, then the pict contains a vertical histogram listing the values
of @racket[dist] and their probabilities.}

@item{If @racket[dist] is any other kind of distribution, an error is raised.}

]
If @racket[ref-dist] is not false, its PDF (if continuous) and CDF are also
plotted in green with dotted lines.

@examples[#:eval the-eval
(define coin/m
  (model
   (define p (sample (uniform-dist 0 1)))
   (observe (binomial-dist 10 p) 3)
   p))
(dist->pict (sampler->discrete-dist (importance-sampler coin/m) 100)
            (beta-dist (add1 3) (add1 7)))
]}

@defproc[(samples->pict [samples (sample-frame/c real?)]
                        [ref-dist (or/c #f numeric-dist?) #f]
                        [#:normalize? normalize? boolean? #t])
         pict?]{

Like @racket[dist->pict], but using the values of @racket[samples] instead.
}


@; ============================================================
@(close-eval the-eval)
