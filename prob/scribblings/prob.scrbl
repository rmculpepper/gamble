#lang scribble/doc
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     prob))

@title{Prob: Probabilistic Programming}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[prob]

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

@bold{Copying} This program is free software: you can redistribute it
and/or modify it under the terms of the
@hyperlink["http://www.gnu.org/licenses/lgpl.html"]{GNU Lesser General
Public License} as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License and GNU Lesser General Public License for more
details.

@include-section["intro.scrbl"]
@include-section["erp.scrbl"]


@;{----------------------------------------}

@bibliography[
#:tag "prob-bibliography"

@bib-entry[#:key "EPP"
           #:title "Embedded Probabilistic Programming"
           #:author "Oleg Kiselyov and Chung-chieh Shan"
           #:location "Domain-Specific Languages, pp 360-384"
           #:url "http://dx.doi.org/10.1007/978-3-642-03034-5_17"]

@bib-entry[#:key "Bher"
           #:title "Lightweight Imeplementations of Probabilistic Programming Languages Via Transformational Compilation"
           #:author "David Wingate, Andreas Stuhlüller, and Noah Goodman"
           #:location "Proc. of the 14th Artificial Intelligence and Statistics"
           #:url "http://stanford.edu/~ngoodman/papers/WSG-AIStats11.pdf"]

]
