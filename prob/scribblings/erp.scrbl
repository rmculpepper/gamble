#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          racket/list
          (for-label racket/contract
                     prob))

@(define the-eval (make-base-eval))
@(the-eval '(require prob))

@title[#:tag "erps"]{Elementary Random Procedures}

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


@(close-eval the-eval)
