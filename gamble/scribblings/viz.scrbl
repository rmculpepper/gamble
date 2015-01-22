;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     racket/class
                     (except-in pict table)
                     (only-in plot/pict plot)
                     (except-in plot plot density)
                     gamble
                     gamble/viz
                     gamble/viz/multi))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble (only-in gamble/viz [hist-pict hist])))

@title[#:tag "viz"]{Visualization Utilities}

@defmodule[gamble/viz]

This module provides very basic utilities for visualizing data. For
more flexible and comprehensive visualization support, see the
@racketmodname[plot] library.

@defproc[(hist [vals (listof any/c)])
         @#,elem{display}]{

Plots a histogram of @racket[vals].

@examples[#:eval the-eval
(hist (list 1 2 3 1 1))
]
}

@section[#:tag "viz-multi"]{Multiple Visualizations}

@defmodule[gamble/viz/multi]

@definterface[multi-viz<%> ()]{

Interface for displays capable of showing and navigating among
multiple visualizations. Currently all visualizations must be
represented as picts (@racket[pict?]). See the
@racketmodname[pict] and @racketmodname[plot/pict] libraries for
high-level pict constructors, and see the low-level @racket[dc] pict
constructor for a way of creating a pict from a drawing procedure.

@defmethod[(add-pict [p pict?]) void?]{

Adds @racket[p] to the end of the sequence of visualizations.
}
}

@defproc[(make-multi-viz-frame [#:label label string? "Visualization"])
         (is-a?/c multi-viz<%>)]{

Creates a frame that can show multiple visualizations. Add
visualizations with the @method[multi-viz<%> add-pict] method.

@racketblock[
(require @#,(racketmodname pict) @#,(racketmodname plot/pict))
(define mv (make-multi-viz-frame))
(send mv add-pict (plot (function sin (- pi) pi)))
(send mv add-pict (circle 400))
]
}

@(close-eval the-eval)
