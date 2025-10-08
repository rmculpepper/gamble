;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/base racket/contract gamble))

@title[#:version "2.0"]{Gamble: Probabilistic Programming}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[gamble]

A library for probabilistic programming, with probabilistic models as functional
programs with managed stochastic effects.

@(local-table-of-contents)

@bold{Development} Development of this library is hosted by
@hyperlink["http://github.com"]{GitHub} at the following project page:

@centered{@url{https://github.com/rmculpepper/gamble}}

@bold{Acknowledgements} Thanks to Olin Shivers, Mitch Wand, Andrew Cobb, Sean
Stromsen, Theo Giannakopoulos, Aleksey Kliger, Greg Sullivan, and the PPAML
project for support and collaboration. Thanks to Neil Toronto for creating the
@racketmodname[math] library, particularly @racketmodname[math/distributions]
for probability distribution support.

@include-section["intro.scrbl"]
@include-section["model.scrbl"]
@include-section["dist.scrbl"]
@include-section["solvers.scrbl"]
@include-section["samples.scrbl"]
@include-section["util.scrbl"]

@; ----------------------------------------

@bibliography[
#:tag "prob-bibliography"

@bib-entry[#:key "Church"
           #:title "Church: a language for generative models"
           #:author "Noah Goodman, Vikash Mansinghka, Daniel Roy, Keith Bonawitz, and Joshua Tenenbaum"
]

@bib-entry[#:key "LMH"
           #:title "Lightweight Implementations of Probabilistic Programming Languages Via Transformational Compilation"
           #:author "David Wingate, Andreas Stuhlüller, and Noah Goodman"
           #:location "Proc. of the 14th Artificial Intelligence and Statistics"
           #:url "http://stanford.edu/~ngoodman/papers/WSG-AIStats11.pdf"]

@bib-entry[#:key "EPP"
           #:title "Embedded Probabilistic Programming"
           #:author "Oleg Kiselyov and Chung-chieh Shan"
           #:location "Domain-Specific Languages, pp 360-384"
           #:url "http://dx.doi.org/10.1007/978-3-642-03034-5_17"]

]
