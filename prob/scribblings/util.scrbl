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
                     prob/viz
                     (prefix-in math: math/array)
                     (prefix-in math: math/matrix)
                     (prefix-in typed: typed/racket/base)))

@(define the-eval (make-base-eval))
@(the-eval '(require prob (only-in prob/viz [hist-pict hist])))

@title[#:tag "util"]{Utilities}

@; ============================================================
@section[#:tag "stat-util"]{Statistical Utilities}

For the purpose of this section, a RealVector is a real
(@racket[real?]), a vector of reals (@racket[(vectorof real?)]), or a
column matrix (@racket[col-matrix?]).

@defstruct*[statistics
            ([dim exact-positive-integer?]
             [n exact-positive-integer?]
             [mean col-matrix?]
             [cov matrix?])]{

Represents some basic statistics of a sample sequence of RealVectors
of compatible shapes. The @racket[dim] field represents the dimension
of the sample vectors; @racket[n] is the number of samples;
@racket[mean] is the mean of the samples; and @racket[cov] is the
covariance matrix.
}

@deftogether[[
@defproc[(sampler->statistics [s sampler?]
                              [n exact-positive-integer?]
                              [f (-> any/c @#,(elem "RealVector")) values]
                              [#:burn burn exact-nonnegative-integer? 0]
                              [#:thin thin exact-nonnegative-integer? 0])
         statistics?]
@defproc[(samples->statistics [samples (vectorof (vectorof real?))])
         statistics?]
]]{

Returns the statistics of @racket[samples] or of @racket[n] samples
drawn from @racket[s] and passed through @racket[f].

@examples[#:eval the-eval
(sampler->statistics (mh-sampler (normal 0 1)) 1000)
]
}

@defproc[(sampler->mean+variance [sampler sampler?]
                                 [n exact-positive-integer?]
                                 [f (-> any/c real?) values]
                                 [#:burn burn exact-nonnegative-integer? 0]
                                 [#:thin thin exact-nonnegative-integer? 0])
         (values real? real?)]{

Like @racket[sample->statistics], but returns the mean and variance as
two scalar values.

@examples[#:eval the-eval
(sampler->mean+variance (rejection-sampler (flip 1/2))
                        100
                        (indicator/value #t))
]
}


@; ============================================================
@section[#:tag "test-util"]{Utilities for Testing and Comparing Distributions}

@deftogether[[
@defproc[(sampler->KS [sampler sampler?]
                      [iterations exact-positive-integer?]
                      [dist dist?])
         (>=/c 0)]
@defproc[(samples->KS [samples (vectorof real?)]
                      [dist dist?])
         real?]
]]{

Calculates the
@hyperlink["http://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov
statistic} of a sample set with @racket[dist]. The result is a measure
of the goodness of fit of the samples to the distribution.

@examples[#:eval the-eval
(sampler->KS (rejection-sampler (uniform 0 1))
             1000
             (uniform-dist 0 1))
(sampler->KS (rejection-sampler (normal 0 1))
             1000
             (uniform-dist 0 1))
(sampler->KS (rejection-sampler (for/sum ([i 3]) (uniform -1 1)))
             100
             (normal-dist 0 1))
]
}

@defproc[(discrete-dist-error [dist1 discrete-dist?]
                              [dist2 discrete-dist?])
         (>=/c 0)]{

Returns a measure of the difference between two discrete
distributions. The result is the probability mass that would need to
be reassigned in order to transform @racket[dist1] into
@racket[dist2].

@examples[#:eval the-eval
(discrete-dist-error
 (discrete-dist ['A 3/5] ['B 2/5])
 (discrete-dist ['A 1/2] ['B 1/2]))
]

In the example above, @racket[1/10] of the probability mass of
@racket['A] in the first distribution would have to be shifted to
@racket['B] to transform the first distribution into the second.
}


@; ============================================================
@section[#:tag "matrix-util"]{Arrays and Matrices}

This language provides a wrapped version of the array and matrix types
defined in @racketmodname[math/array] and
@racketmodname[math/matrix]. Unlike the version provided by those
libraries, this library's array and matrix types are specialized to
real numbers, and they are simpler and faster to use in untyped code.

Most of the functions listed in the documentation for
@racketmodname[math/array] and @racketmodname[math/matrix] have
corresponding monomorphic-wrapped functions exported by this
language. In addition, the following functions and special forms are
provided.

@deftogether[[
@defproc[(array? [v any/c]) boolean?]
@defproc[(mutable-array? [v any/c]) boolean?]
@defproc[(matrix? [v any/c]) boolean?]
@defproc[(square-matrix? [v any/c]) boolean?]
@defproc[(row-matrix? [v any/c]) boolean?]
@defproc[(col-matrix? [v any/c]) boolean?]
]]{

Like @racket[math:array?], @racket[math:mutable-array?],
@racket[math:matrix?], @racket[math:square-matrix?],
@racket[math:row-matrix?], and @racket[math:col-matrix?],
respectively, but for this library's monomorphic-wrapped arrays
instead.
}

@deftogether[[
@defstruct*[ImmArray ([contents (math:Array typed:Real)])]
@defstruct*[MutArray ([contents (math:Mutable-Array typed:Real)])]
]]{

Monomorphic wrapper structs around the @racketmodname[math] library's
arrays and matrices.
}

@defproc[(Array-contents [a array?]) (math:Array typed:Real)]{

Gets the underlying @racketmodname[math/array] array from the
wrapper.
}

@deftogether[[
@defform[(array #[#[...] ...])]
@defform[(mutable-array #[#[...] ...])]
@defform[(matrix [[element-expr ...] ...])]
@defform[(col-matrix [element-expr ...])]
@defform[(row-matrix [element-expr ...])]
@defform[(for/matrix numrows-expr numcols-expr (for-clause ...) body ...+)]
@defform[(for*/matrix numrows-expr numcols-expr (for-clause ...) body ...+)]
]]{

Untyped wrapping versions of @racket[math:array],
@racket[math:mutable-array], @racket[math:matrix],
@racket[math:col-matrix], @racket[math:row-matrix],
@racket[math:for/matrix], and @racket[math:for*/matrix], respectively.
}

@defproc[(matrix11->value [matrix? m]) real?]{

Extracts the single value from a 1x1 matrix.
}

@defproc[(array->immutable-array [array? a]) array?]{

Converts an array into an immutable array.
}

@defproc[(make-mutable-matrix [m exact-nonnegative-integer?]
                              [n exact-nonnegative-integer?]
                              [fill real?])
         matrix?]{

Creates a mutable matrix of shape @racket[m] by @racket[n] filled with
initial value @racket[fill].
}

@defproc[(matrix-set! [m matrix?]
                      [i exact-nonnegative-integer?]
                      [j exact-nonnegative-integer?]
                      [value real?])
         void?]{

Sets the entry of @racket[m] at row @racket[i] and column @racket[j]
to be @racket[value].
}

@defproc[(matrix-symmetric? [m matrix?])
         boolean?]{

Returns @racket[#t] if @racket[m] is a square, symmetric matrix,
@racket[#f] otherwise.
}

@deftogether[[
@defproc[(array-sqrt/nan [a array?]) array?]
@defproc[(array-sqrt/err [a array?]) array?]
]]{

Like @racket[math:array-sqrt], but for negative entries either returns
@racket[+nan.0] or raises an error, so as to avoid producing an array
of complex numbers.
}

@defproc[(matrix-cholesky [m matrix?])
         matrix?]{

Given a symmetric, positive-definite matrix @racket[_A], returns a
lower-triangular matrix @racket[_L] such that @racket[(matrix* _L
(matrix-transpose _L))] is equal to @racket[_A].

If the Cholesky decomposition cannot be calculated, the function
raises an error.
}

@defproc[(matrix-ldl [m matrix?])
         (values matrix? (vectorof real?))]{

Given a symmetric, positive-definite matrix @racket[_A], returns the
LDL decomposition, consisting of a lower-triangular matrix and a
diagonal matrix.
}


@; ============================================================
@section[#:tag "misc-util"]{Miscellaneous Utilities}

@defproc[(probability? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is a real number between 0 and 1
(inclusive), @racket[#f] otherwise.
}

@defparam[verbose? v? boolean?]{

Parameter that controls whether informative messages are printed by
solvers and ERPs.
}

@defproc[(repeat [thunk (-> any/c)]
                 [n exact-nonnegative-integer?])
         list?]{

Calls @racket[thunk] @racket[n] times, accumulating the results in a
list.

@examples[#:eval the-eval
(repeat flip 10)
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

@defproc[(resample [samples vector?]
                   [weights (vectorof (>=/c 0))]
                   [n exact-nonnegative-integer? (vector-length samples)]
                   [#:alg algorithm (or/c 'multinomial 'residual #f)
                   'multinomial])
         vector?]{

Resamples @racket[n] values from @racket[samples] with corresponding
weights given by @racket[weights].
}

@(close-eval the-eval)
