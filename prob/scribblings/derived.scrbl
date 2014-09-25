;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     prob
                     prob/viz))

@(define the-eval (make-base-eval))
@(the-eval '(require prob (only-in prob/viz [hist-pict hist])))

@title[#:tag "features"]{Derived Stochastic Forms and Functions}

This section describes features derived from the primitive stochastic
functions.


@; ============================================================

@section[#:tag "erps"]{Sampling Convenience Functions}

The stochastic functions in this section, sometimes called
@deftech{elementary stochastic procedures} (ERPs), are convenience
functions equivalent to calling @racket[sample] on an appropriate
distribution.

@deftogether[[
@defproc[(bernoulli [p (real-in 0 1) 1/2])
         (or/c 1 0)]
@defproc[(binomial [count exact-nonnegative-integer?]
                   [p (real-in 0 1)])
         exact-nonnegative-integer?]
@defproc[(categorical [weights (vectorof (>=/c 0))])
         exact-nonnegative-integer?]
@defproc[(discrete-uniform [n exact-positive-integer?])
         exact-nonnegative-integer?]
@defproc[(geometric [p (real-in 0 1) 1/2])
         exact-nonnegative-integer?]
@defproc[(poisson [mean (>/c 0)])
         exact-nonnegative-integer?]
]]{

Equivalent to the following, respectively:
@racketblock[
(sample (bernoulli-dist p))
(sample (binomial-dist count p))
(sample (categorical-dist weights))
(sample (categorical-dist (make-vector n (/ n))))
(sample (geometric-dist p))
(sample (poisson-dist mean))
]
}

@deftogether[[
@defproc[(beta [a (>/c 0)]
               [b (>/c 0)])
         real?]
@defproc[(cauchy [mode real?]
                 [scale (>/c 0) 1])
         real?]
@defproc[(exponential [mean (>/c 0)])
         real?]
@defproc[(gamma [shape (>/c 0) 1]
                [scale (>/c 0) 1])
         real?]
@defproc[(logistic [mean real? 0]
                   [scale (>/c 0) 1])
         real?]
@defproc[(normal [mean real? 0]
                 [stddev (>/c 0) 1])
         real?]
@defproc[(pareto [scale (>/c 0)] [shape (>/c 0)])
         real?]
@defproc*[([(uniform)
            real?]
           [(uniform [hi real?])
            real?]
           [(uniform [lo real?] [hi real?])
            real?])]
]]{

Equivalent to the following, respectively:
@racketblock[
(sample (beta-dist a b))
(sample (cauchy-dist mode scale))
(sample (exponential-dist mean))
(sample (gamma-dist shape scale))
(sample (logistic-dist mean scale))
(sample (normal-dist mean stddev))
(sample (pareto-dist scale shape))
(sample (uniform-dist lo hi))
]
}

@deftogether[[
@defproc[(discrete [weighted-vals (listof (cons/c any/c (>=/c 0)))])
         any/c]
@defproc[(discrete* [vals (non-empty-listof any/c)]
                    [weights (non-empty-listof (>/c 0))])
         any/c]
]]{

Equivalent to the following, respectively:
@racketblock[
(sample (make-discrete-dist weighted-vals))
(sample (make-discrete-dist* vals weights))
]
}

@defproc[(flip [p (real-in 0 1) 1/2])
         boolean?]{

Equivalent to @racket[(= 1 (sample (bernoulli-dist p)))].
}


@; ============================================================

@section[#:tag "lazy"]{Laziness via Memoization}

@defform[(pdelay body ...+)]{

Delays the @racket[body] computation, producing a promise
(@racket[ppromise?]) that can be forced with @racket[pforce].
The @racket[pdelay] form uses @racket[mem] internally, so the promises
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

@defform[(deflazy id expr)]{

Similar to @racket[(define id (pdelay expr))], except that when a
reference to @racket[id] is evaluated, it is automatically forced.

@examples[#:eval the-eval
(deflazy x (begin (printf "flipping!\n") (flip)))
(code:comment "flip hasn't occurred yet")
(if x 1 0)
]
}

@defform*[[(defmem (fun-id arg-id ...) body ...+)
           (defmem (fun-id arg-id ... . rest-arg-id) body ...+)]]{

Convenience form for defining memoized functions. Equivalent to the
following, respectively:
@racketblock[
(define fun-id (mem (lambda (arg-id ...) body ...)))
(define fun-id (mem (lambda (arg-id ... . rest-arg-id) body ...)))
]
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


@; ============================================================

@section[#:tag "observe"]{Observations}

@defform[(observe observable-expr value-expr)]{

Like @racket[observe-at], except that instead of a distribution, the
observation conditions the result of evaluating
@racket[observable-expr]. The @racket[observable-expr] must evaluate
to a call to @racket[sample] (either explicitly or implicitly through
one of the random procedures below) in an @tech{observable context};
the call to @racket[sample] is replaced with a call to
@racket[observe-at] with a suitably adjusted value.

A @deftech{observable context} (OC) is (currently) defined as
follows:

@racketgrammar[#:literals (+ cons reverse)
               OC @#,(racketkeywordfont "[ ]")
                  (+ expr ... OC)
                  (cons OC expr)
		  (cons expr OC)
		  (reverse OC)]

Support for other invertible built-in functions will be added in the
future.

Thus, for example, the following are valid:
@interaction[#:eval the-eval
(observe (+ 10 (normal 0 1)) 11.5)
(observe (cons (bernoulli) (normal 0 1))
	 (cons 0 .2))
(observe (build-list 3 (lambda (i) (bernoulli)))
         '(1 1 0))
]
but the following are not:
@interaction[#:eval the-eval
(observe (+ (normal 0 1) 10) 11.5)
(observe (vector->list (build-vector 3 (lambda (i) (bernoulli))))
	 '(1 1 0))
]

The call to sample can occur in another function, as long as it occurs
in an observable context with respect to the function's body and the
function call also occurs within an observable context. For example:
@interaction[#:eval the-eval
(define (f x) (+ (* 3 x) 12 (normal 0 1)))
(observe (f 9) 40)
]

Note: because of floating-point imprecision, the result of
@racket[observable-expr] may not be exactly equal to
@racket[value-expr].
}

@(close-eval the-eval)
