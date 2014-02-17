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
