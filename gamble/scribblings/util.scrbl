;; Copyright (c) 2014-2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/contract
                     racket/class
                     racket/match
                     gamble
                     gamble/viz
                     (prefix-in math: math/array)
                     (prefix-in math: math/matrix)
                     (prefix-in typed: typed/racket/base)))

@(define the-eval (make-base-eval))
@(the-eval '(require gamble racket/match (only-in gamble/viz [hist-pict hist])))
@(the-eval '(random-seed 1))

@title[#:tag "util"]{Utilities}

@; ============================================================
@section[#:tag "model-util"]{Models: Lightweight Modules}

@defform[(defmodel model-name body ...+)]{

Defines @racket[model-name] as a model---a lightweight module. The
@racket[body] forms are not evaluated until the model is invoked using
@racket[open-model]. A model has no ``result value''; it only exports
the names it defines. Note that @racket[model-name] can only be used
with @racket[open-model]; in particular, it is not a variable, and a
model is not a value.

Every name defined within the model is exported, but an export bound
to unknown syntax (eg, a macro definition) raises an error if
used. Names bound with @racket[defmem] and @racket[deflazy] may be
exported and used safely. Names imported via @racket[open-model] are
automatically re-exported.

@examples[#:eval the-eval
(defmodel m1
  (define x 1)
  (deflazy y (begin (displayln "computing y!") 2))
  (define-syntax-rule (z) (+ x y)))
(defmodel m2
  (open-model m1)
  (deflazy w (list x y)))
(open-model m2)
w
x
y
(code:line (z) (code:comment "can't use macro from model"))
]

Models can be used to define reusable generative models:

@interaction[#:eval the-eval
(defmodel true-strength
  (code:comment "type Person = Symbol")
  (code:comment "type Match = (list Team Team)")

  (code:comment "strength : Person -> Real")
  (defmem (strength p) (normal 10 2))

  (code:comment "lazy? : Person -> Boolean")
  (define (lazy? p) (flip 0.1))

  (code:comment "pulling-power : Person -> Real")
  (define (pulling-power p)
    (if (lazy? p)
        (/ (strength p) 2.0)
        (strength p)))

  (code:comment "team-pulling-power : (Listof Person) -> Real")
  (define (team-pulling-power t)
    (for/sum ([p (in-list t)]) (pulling-power p)))

  (code:comment "team1-wins? : Match -> Boolean")
  (define (team1-wins? m)
    (> (team-pulling-power (car m))
       (team-pulling-power (cadr m))))

  (observe/fail (team1-wins? '((james david) (brian john))) #t))
]

The generative model can then be combined with different queries:

@interaction[#:eval the-eval
(sampler->discrete-dist
  (mh-sampler
    (open-model true-strength)
    (> (strength 'james) (strength 'brian)))
  1000)
(sampler->discrete-dist
  (mh-sampler
    (open-model true-strength)
    (team1-wins? '((james david) (bob andrew))))
  1000)
]
}

@defform[(defmodel+ model-name body ...)]{

Extends an existing model named @racket[model-name] with additional
@racket[body] elements, or defines it if it is not already defined.

@examples[#:eval the-eval
(defmodel+ arith
  (define a 1)
  (define b 2))
(defmodel+ arith
  (define c (+ a b)))
(open-model arith)
(list a b c)
]
}

@defform[(open-model model-name)]{

Executes the body of the model corresponding to @racket[model-name]
and defines its exported names in the enclosing scope.
}


@; ============================================================
@section[#:tag "lazy-struct"]{Lazy Structures}

@defform[(lazy-struct name-id (field-id ...))]{

Defines a struct type called @racket[name-id] whose constructor delays
its arguments using @racket[pdelay] and whose accessors automatically
force the corresponding fields. If the constructor @racket[_name] is
used with the wrong number of arguments, a compile-time error is
raised.

In addition to the standard names (@racket[_name], @racket[_name?],
@racket[_name-field] ...), the name @racket[_strict-make-name] is
bound to a strict constructor procedure.

Printing, @racket[equal?] comparison, and hashing all automatically
force all fields.

Instances of lazy structs can be destructured with @racket[match]
using one of the following patterns:

@specsubform[(name-id #:strict field-pattern ...)]{

Forces each @racket[_field] and matches it against @racket[_field-pattern].
}

@specsubform[(name-id #:thunk field-var-id ...)]{

Binds each @racket[_field-var-id] to a thunk that when applied forces
the corresponding field.
}

@examples[#:eval the-eval
(lazy-struct lpair (x y))
(define lp (lpair 'a (/ 1 0)))
(lpair-x lp)
(lpair-y lp)
lp
(match lp
  [(lpair #:thunk get-x get-y)
   (get-x)])
(match (lpair 'a 'b)
  [(lpair #:strict x y)
   (list x y)])
]
}

@; ============================================================
@section[#:tag "stat-util"]{Statistical Utilities}

@defstruct*[statistics
            ([dim exact-positive-integer?]
             [n exact-positive-integer?]
             [mean col-matrix?]
             [cov matrix?])]{

Represents some basic statistics of a sample sequence of real vectors
of compatible shapes. The @racket[dim] field represents the dimension
of the sample vectors; @racket[n] is the number of samples;
@racket[mean] is the mean of the samples; and @racket[cov] is the
covariance matrix.
}

@defproc[(real-vector-like? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a real, a real vector, or a
column matrix of reals; @racket[#f] otherwise.
}

@deftogether[[
@defproc[(samples->statistics [samples (vectorof real-vector-like?)])
         statistics?]
@defproc[(sampler->statistics [s sampler?]
                              [n exact-positive-integer?]
                              [f (-> any/c real-vector-like?) values]
                              [#:burn burn exact-nonnegative-integer? 0]
                              [#:thin thin exact-nonnegative-integer? 0])
         statistics?]
]]{

Returns the statistics of @racket[samples].

The second form is equivalent to the following:
@racketblock[(samples->statistics (generate-samples s n f #:burn burn #:thin thin))]

@examples[#:eval the-eval
(sampler->statistics (mh-sampler (normal 0 1)) 1000)
]
}

@deftogether[[
@defproc[(samples->mean+variance [rvs (vectorof real?)])
         (values real? real?)]
@defproc[(sampler->mean+variance [sampler sampler?]
                                 [n exact-positive-integer?]
                                 [f (-> any/c real?) values]
                                 [#:burn burn exact-nonnegative-integer? 0]
                                 [#:thin thin exact-nonnegative-integer? 0])
         (values real? real?)]
]]{

Like @racket[sample->statistics], but returns the mean and variance as
two scalar values.

The second form is equivalent to the following:
@racketblock[(samples->mean+variance (generate-samples s n f #:burn burn #:thin thin))]

@examples[#:eval the-eval
(sampler->mean+variance (rejection-sampler (flip 1/2)) 100 (indicator #t))
]
}

@deftogether[[
@defproc[(samples->mean [rvs (vectorof any/c)])
         any/c]
@defproc[(sampler->mean [sampler weighted-sampler?]
                        [n exact-positive-integer?]
                        [f (-> any/c any/c) values]
                        [#:burn burn exact-nonnegative-integer? 0]
                        [#:thin thin exact-nonnegative-integer? 0])
         any/c]
]]{

Like @racket[sample->mean+variance], but returns only the mean. In
contrast to the other functions in this section,
@racket[sampler->mean] handles weighted samplers, and the sample
results can be numbers, lists, vectors, arrays, and matrices. If two
values have incompatible shapes, the function returns @racket[+nan.0]
at the incompatible positions.

@examples[#:eval the-eval
(sampler->mean (rejection-sampler (dirichlet '#(1 1 1))) 10)
(sampler->mean (rejection-sampler (for/list ([i (+ 2 (discrete-uniform 4))]) 17)) 10)
]
}


@; ============================================================
@section[#:tag "test-util"]{Utilities for Testing and Comparing Distributions}

@defproc[(samples->KS [samples (vectorof real?)]
                      [dist dist?])
         real?]{

Calculates the
@hyperlink["http://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test"]{Kolmogorov--Smirnov
statistic} of a sample set @racket[samples] with respect to
@racket[dist]. The result is a measure of the goodness of fit of the
samples to the distribution.

@examples[#:eval the-eval
(samples->KS (generate-samples (rejection-sampler (uniform 0 1)) 1000)
             (uniform-dist 0 1))
(samples->KS (generate-samples (rejection-sampler (normal 0 1)) 1000)
             (uniform-dist 0 1))
(samples->KS (generate-samples (rejection-sampler (for/sum ([i 3]) (uniform -1 1))) 100)
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
@defproc[(indicator [value-or-pred
                     (or/c (-> any/c) any/c)])
         (-> any/c (or/c 1 0))]
@defproc[(indicator/value [value any/c])
         (-> any/c (or/c 1 0))]
@defproc[(indicator/predicate [pred (-> any/c boolean?)])
         (-> any/c (or/c 1 0))]
]]{

Produces an indicator function for the value @racket[value] or the set
of values accepted by the predicate @racket[pred], respectively.

@examples[#:eval the-eval
(define zero (indicator 0))
(zero 0)
(zero 2.74)
(zero 'apple)
(define pos (indicator positive?))
(pos 0)
(pos -2.3)
(pos 4.5)
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
