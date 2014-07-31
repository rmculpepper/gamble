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
@(the-eval '(require prob (only-in prob/viz
                                   [hist-pict hist]
                                   [bin-pict bin])))

@title[#:tag "solvers"]{Samplers and Solvers}

A @deftech{sampler} is a procedure that takes zero arguments, runs a
stochastic program, and produces a single result.

The examples in the following sections use the following function
definitions:

@interaction[#:eval the-eval
(define (count-heads n)
  (if (zero? n) 0 (+ (if (flip) 1 0) (count-heads (sub1 n)))))
(define (geom)
  (if (flip) 0 (add1 (geom))))
]

@defform[(rejection-sampler def/expr ... result-expr maybe-when-clause)
         #:grammar ([maybe-when-clause (code:line)
                                       (code:line #:when condition-expr)])]{

Produces a @tech{sampler} that, when applied, returns a value of
@racket[result-expr] arising from an execution where
@racket[condition-expr], if present, was satisfied.

The sampler is implemented using rejection sampling. That is, the
@racket[def/expr]s and @racket[result-expr] are evaluated. Then
@racket[condition-expr] is evaluated, if present; if it produces
@racket[#t], the execution is accepted and the value of
@racket[result-expr] is returned; otherwise, the process is repeated.

@examples[#:eval the-eval
(define s-or
  (rejection-sampler
    (define A (flip))
    (define B (flip))
    A
    #:when (or A B)))
(hist (repeat s-or 100))
(hist (repeat s-or 1000))

(define rs-count-heads
  (rejection-sampler
   (count-heads 10)))
(rs-count-heads)
(rs-count-heads)
(hist (repeat rs-count-heads 100))
]
}

@defform[(mh-sampler def/expr ... result-expr maybe-when-clause)
         #:grammar ([maybe-when-clause (code:line)
                                       (code:line #:when condition-expr)])]{

Like @racket[rejection-sampler], but when applied repeatedly uses a
variant of Metropolis-Hastings as described in @cite{Bher}.

@examples[#:eval the-eval
(define mh-or
  (mh-sampler
    (define A (flip))
    (define B (flip))
    A
    #:when (or A B)))
(hist (repeat mh-or 100))
(hist (repeat (lag mh-or 100) 100))

(define mh-n-flips
  (mh-sampler
   (count-heads 10)))
(parameterize ((verbose? #t))
  (mh-n-flips))
(parameterize ((verbose? #t))
  (mh-n-flips))
(hist (repeat mh-n-flips 100))
(hist (repeat (lag mh-n-flips 100) 100))
(hist (repeat mh-n-flips 2000))
]
}

@defform[(hmc-sampler def/expr ... result-expr
                      maybe-epsilon-clause
                      maybe-L-clause
                      maybe-when-clause)
         #:grammar ([maybe-epsilon-clause (code:line)
                                          (code:line #:epsilon epsilon-expr)]
                    [maybe-L-clause (code:line)
                                    (code:line #:L L-expr)]

                    [maybe-when-clause (code:line)
                                       (code:line #:when cond-expr)])]{

Like @racket[mh-sampler], but when applied uses a Hamiltonian Monte Carlo method
for MH proposals.

This solver is EXPERIMENTAL and comes with a number of restritctions:

@itemlist[
  @item{There must be no structural dependencies among the distributions of @racket[def/expr ... result-expr]}
  @item{All the distributions must be continuous.}
  @item{Any distribution that has parameters that depend on the value of another distribution
        must be wrapped in a @racket[derivative] form. See @seclink["hmc-utils"] for more information.}
]

The parameters @racket[epsilon-expr] and @racket[L-expr] specify the size of each Hamiltonian step and the
number of Hamiltonian steps to take in the course of obtaining a sample.  Note that careful tuning
may be required to achive good results.

@examples[#:eval the-eval
(define one-dim-loc-hmc
  (hmc-sampler
     (define Hid (label 'Hid (derivative (normal 10 0.5) #f #f)))
     (define Obs (derivative (normal Hid 0.01)
                             [(Hid)
                              (λ (hid) 1)]
                             #f))
     
     Hid
     #:when (< (abs (- Obs 9.7)) 0.01)
     #:epsilon 0.005
     #:L 40))

(bin (repeat one-dim-loc-hmc 100))
]
}

@defform[(enumerate def/expr ... result-expr
           maybe-when-clause maybe-limit-clause maybe-normalize-clause)
         #:grammar ([maybe-when-clause (code:line)
                                       (code:line #:when condition-expr)]
                    [maybe-limit-clause (code:line)
                                        (code:line #:limit limit-expr)]
                    [maybe-normalize-clause (code:line)
                                            (code:line #:normalize? normalize?-expr)])]{

Returns a distribution table of the form @racket[(list (list _value
_prob) ...)], where @racket[_value] is a value produced by
@racket[result-expr] and @racket[_prob] is its normalized probability
given @racket[condition-expr]. If @racket[normalize?-expr] is given
and evaluates to @racket[#f], then the probability is unnormalized
instead.

The @racket[enumerate] form works by exploring all possibilities using
the technique described in @cite{EPP}. If @racket[limit-expr]
evaluates to a probability @racket[_limit], then exploration ceases
when the unexplored possibilities have probability less than
@racket[_limit] times the sum of the probabilities of the results
accepted by @racket[condition-expr] so far. If @racket[limit-expr]
evalues to @racket[#f], then exploration ceases only when all paths
have been explored; if any path is infinite, then @racket[enumerate]
fails to terminate.

Only discrete and integer-valued @tech{ERP}s can be used with
@racket[enumerate], and infinite-range ERPs (such as
@racket[geometric]) require the use of @racket[#:limit] to enforce
termination.

@examples[#:eval the-eval
(enumerate
  (define A (flip))
  (define B (flip))
  A
  #:when (or A B))
(enumerate
  (define A (geom))
  A
  #:when (< 10 A 20))
]

Use @racket[#:limit] to control when exploration should be cut
off. The default limit is @racket[1e-6].

@interaction[#:eval the-eval
(enumerate
  (geom)
  #:limit 1e-9)
]

Use @racket[#:normalize? #f] to get the result probabilities without
normalizing by the acceptance rate:

@interaction[#:eval the-eval
(enumerate
 (define (drop-coin?) (flip 0.9))
 (define (drunk-flips n)
   (cond [(zero? n)
          #t]
         [(drop-coin?)
          'failed]
         [else
          (and (flip) (drunk-flips (sub1 n)))]))
 (define A (drunk-flips 10))
 (eq? A #t)
 #:when (not (eq? A 'failed))
 #:normalize? #f
 (code:comment "Default limit is too high to detect #t case")
 #:limit #f)
]
}


@defform[(importance-sampler def/expr ... result-expr maybe-when-clause)
         #:grammar ([maybe-when-clause (code:line)
                                       (code:line #:when condition-expr)])]{

Like @racket[rejection-sampler], but uses the lazy-search tree
mechanism of @racket[enumerate], although without exhaustively
exploring it.

Produces a @emph{weighted sampler}.

@examples[#:eval the-eval
(define ws
  (importance-sampler
    (define A (flip))
    (define B (flip))
    A
    #:when (or A B)))
(repeat ws 10)
]
}

@section[#:tag "hmc-utils"]{Special utilities for Harmonic Monte Carlo}

The @racket[hmc-solver] requires that all the distrubutions in the
model are continuous.  It further requires partial derivatives of each
expression that mentions previously defined random variables that may
occur in the parameters of another random variable.  Such information
may be provided using the @racket[derivative] special form.

@defform[(derivative dist-expr parameter-derivative-clause ...)
         #:grammar ([parameter-derivative-clause (code:line [(label-ids ...) partial-fn-expr])
                                          (code:line #f)])]{

Anottate the @racket[dist-expr] distribution sample expression with
the partial derivatives of its parameters.  There should be as many
parameter derivative clauses as there are parameters of the underlying
distribution.

Each @racket[parameter-derivative-clause] must be either @racket[#f]
which indicates that the parameter is constant (with respect to random
variables). Or else it must consist of a list of previously applied
@racket[label] symbols - one for each random variable that occurs in
the expression for the corresponding parameter of @racket[dist-expr].
The @racket[partial-fn-expr] should be a procedure that takes one
argument for each random variable and returns the same number of
values.  Each value is the partial derivative with respect to the
correspondingly labeled variable.

@examples[#:eval the-eval
(define derivative-example
  (hmc-sampler
   (define A (label 'A-lbl (derivative (normal 0 1) #f #f)))
   (define B (label 'B-lbl (derivative (normal 1 1) #f #f)))

   (define C (derivative (normal (- (* A A) (* B B)) 1)
                         [(A-lbl B-lbl)
                          (λ (a b)
                            (values (* 2 a)
                                    (- (* 2 b))))]
                         #f))
   B))
]

In the example above, @racket[A] and @racket[B] do not depend on any other random variables,
so the derivatives of their parameters are zero (shorthand: @racket[#f]).  On the other hand,
the mean of @racket[C] mentions both of them.  Therefore the first derivative clause lists two labels,
and the expression returns two values: the partial derivative of @racket[(- (* A A) (* B B))] with respect to
@racket[A] and @racket[B], in that order.

}

@(close-eval the-eval)

