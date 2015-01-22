;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket/contract
                     gamble))

@title{Gamble Internals}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(local-table-of-contents)

@include-section["org.scrbl"]
@include-section["interfaces.scrbl"]
@include-section["mh.scrbl"]
@include-section["instrument.scrbl"]
