;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          racket/list
          (for-label racket/contract
                     prob))

@(define the-eval (make-base-eval))
@(the-eval '(require prob))

@title[#:tag "intro"]{Introduction}



@(close-eval the-eval)
