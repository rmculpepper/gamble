;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     prob))

@title{Prob: Probabilistic Programming}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodulelang[prob]

A language for expressing probabilistic models as functional programs
with managed stochastic effects.

@(local-table-of-contents)

@bold{Development} Development of this library is hosted by
@hyperlink["http://github.com"]{GitHub} at the following project page:

@centered{@url{https://github.com/rmculpepper/prob}}

@bold{Acknowledgements} This library borrows from many
sources. 

@itemlist[

@item{Visualization and elementary probability distribution support
use Neil Toronto's excellent @racketmodname[plot] and
@racketmodname[math/distributions] libraries.}

@item{The @racket[enumerate] form uses techniques described in
@cite{EPP}.}

@item{The @racket[mh-sampler] form uses techniques described in
@cite{Bher}.}

]

@bold{Copying} Redistribution and use in source and binary forms, with
or without modification, are permitted provided that the following
conditions are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

@include-section["intro.scrbl"]
@include-section["dist.scrbl"]
@include-section["features.scrbl"]
@include-section["solvers.scrbl"]
@include-section["util.scrbl"]
@include-section["viz.scrbl"]
@; @include-section["notes.scrbl"]

@;{----------------------------------------}

@bibliography[
#:tag "prob-bibliography"

@bib-entry[#:key "Church"
           #:title "Church: a language for generative models"
           #:author "Noah Goodman, Vikash Mansinghka, Daniel Roy, Keith Bonawitz, and Joshua Tenenbaum"
]

@bib-entry[#:key "Bher"
           #:title "Lightweight Imeplementations of Probabilistic Programming Languages Via Transformational Compilation"
           #:author "David Wingate, Andreas Stuhlüller, and Noah Goodman"
           #:location "Proc. of the 14th Artificial Intelligence and Statistics"
           #:url "http://stanford.edu/~ngoodman/papers/WSG-AIStats11.pdf"]

@bib-entry[#:key "EPP"
           #:title "Embedded Probabilistic Programming"
           #:author "Oleg Kiselyov and Chung-chieh Shan"
           #:location "Domain-Specific Languages, pp 360-384"
           #:url "http://dx.doi.org/10.1007/978-3-642-03034-5_17"]

]
