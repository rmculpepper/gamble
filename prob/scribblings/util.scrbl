;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     racket/class
                     prob
                     prob/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require prob (only-in prob/viz [hist-pict hist])))

@title[#:tag "util"]{Probabilistic Utilities}

@defproc[(probability? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is a real number between 0 and 1
(inclusive), @racket[#f] otherwise.
}

@defparam[verbose? v? boolean?]{

Parameter that controls whether informative messages are printed by
solvers and ERPs.
}

@defproc[(discrete-dist-error [dist1 (listof (cons/c any/c (>=/c 0)))]
                              [dist2 (listof (cons/c any/c (>=/c 0)))])
         (>=/c 0)]{

Returns a measure of the difference between two discrete
distributions. The result is the probability mass that would need to
be reassigned in order to transform @racket[dist1] into
@racket[dist2].

@examples[#:eval the-eval
(discrete-dist-error '((A . 3/5) (B . 2/5))
                     '((A . 1/2) (B . 1/2)))
]

In the example above, @racket[1/10] of the probability mass of
@racket['A] in the first distribution would have to be shifted to
@racket['B] to transform the first distribution into the second.
}

@defproc[(sampler->KS [sampler (-> real?)]
                      [iterations exact-positive-integer?]
                      [dist dist?])
         (>=/c 0)]{

Gets @racket[iterations] samples from @racket[sampler] and calculates
the @hyperlink["http://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov
statistic} with @racket[dist]. The result is a measure of the goodness
of fit of the samples to the distribution.

@examples[#:eval the-eval
(sampler->KS (lambda () (uniform 0 1))
             1000
             (uniform-dist 0 1))
(sampler->KS (lambda () (normal 0 1))
             1000
             (uniform-dist 0 1))
(sampler->KS (lambda () (for/sum ([i 3]) (uniform -1 1)))
             100
             (normal-dist 0 1))
]
}


@section[#:tag "sample-utils"]{Sampler Utilities}

@defproc[(repeat [thunk (-> any/c)]
                 [n exact-nonnegative-integer?])
         list?]{

Calls @racket[thunk] @racket[n] times, accumulating the results in a
list.

@examples[#:eval the-eval
(repeat flip 10)
]
}

@defproc[(lag [thunk (-> any/c)]
              [n exact-positive-integer?])
         (-> any/c)]{

Lags a @tech{sampler}; when the resulting thunk is called, the given
@racket[thunk] is called @racket[n] times and only the last value is
returned.

@examples[#:eval the-eval
(repeat (lag (let ([n 0]) (lambda () (set! n (add1 n)) n)) 10) 10)
]
}

@defproc[(sampler->discrete-dist [sampler (-> _A)]
                                 [n exact-positive-integer?]
                                 [f (-> _A _B) (lambda (x) x)])
         discrete-dist?]{

Generates @racket[n] samples using @racket[(f (sampler))], and
produces a list of the results with probability weights.

@examples[#:eval the-eval
(sampler->discrete-dist (lambda () (flip 1/2)) 100)
]
}

@defproc[(sampler->mean+variance [sampler (-> _A)]
                                 [f (-> _A real?) (lambda (x) x)])
         (values real? real?)]{

Generates @racket[n] samples using @racket[(f (sampler))], and returns
the mean and variance. If @racket[sampler] does not return real-valued
samples, then @racket[f] must be given and must convert samples into
real values.

@examples[#:eval the-eval
(sampler->mean+variance (lambda () (flip 1/2))
                        100
                        (indicator/value #t))
]
}

@deftogether[[
@defproc[(indicator/value [v any/c])
         (-> any/c (or/c 1 0))]
@defproc[(indicator/predicate [pred (-> any/c boolean?)])
         (-> any/c (or/c 1 0))]
]]{

Produces an indicator function for the value @racket[v] or the set of
values accepted by the predicate @racket[pred], respectively.

@examples[#:eval the-eval
(define z (indicator/value 0))
(z 0)
(z 2.74)
(z 'apple)
]
}


@section[#:tag "lazy"]{Laziness via Memoization}

@defform[(pdelay body ...+)]{

Delays the @racket[body] computation, producing a promise
(@racket[ppromise?]) that can be forced with @racket[pforce].
The @racket[pdelay] form uses @racket[mem] internally, the promises
cooperate with the enclosing solver/sampler context.

Use @racket[pdelay] to make a random choice lazy when it may not be
relevant to all execution paths.

@examples[#:eval the-eval
(time (code:comment "eager, explores 2^10 possibilities")
 (enumerate
  (define flips (for/list ([i 10]) (flip)))
  (andmap (lambda (x) x) flips)))
(time (code:comment "lazy, explores 11 possibilities")
 (enumerate
  (define flips (for/list ([i 10]) (pdelay (flip))))
  (andmap pforce flips)))
]
}

@defproc[(ppromise? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a promise produced by
@racket[pdelay], @racket[#f] otherwise.
}


@defproc[(pforce [p ppromise?]) any]{

Evaluates @racket[p]'s body and caches the result, if @racket[p] has
not been previously forced, or returns the cached result otherwise.
}


@section[#:tag "table"]{Indexed Tables}

@defform*[[(table ([var-id sequence-expr] ...+) maybe-lazy body ...+)
           (table (var-id ...+) body ...+)]
          #:grammar ([maybe-lazy (code:line)
                                 (code:line #:lazy)])
          #:contracts ([sequence-expr sequence?])]{

Creates an indexed collection that acts like a function; the function
takes as many arguments as there are @racket[var-id]s.

In the first form, the table is finite, and it is eagerly populated
with an entry for every combination of elements from each
@racket[sequence-expr]. (Each @racket[sequence-expr] must be finite.)
If a finite table is addressed with indexes that
do not occur in @racket[sequence-expr], an error is raised.

@examples[#:eval the-eval
(define F (table ([i 10] [j 20]) (flip)))
(F 0 0)
(F 2 3)
(F 7 13)
]

If the @racket[#:lazy] keyword appears after the variable binding
sequence, then the table's entries are not evaluated until they are
looked up (see also @racket[pdelay]).

@examples[#:eval the-eval
(define LF (table ([i 10] [j 20]) #:lazy (printf "flipping!\n") (flip)))
(LF 0 0)
(LF 0 0)
]

In the second form, the table is conceptually infinite, and it is
lazily populated as entries are requested. This form is equivalent to
@racket[(mem (lambda (var-id ...) body ...))].
}


@(close-eval the-eval)
