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

@title[#:tag "intro"]{Introduction}

The @racketmodname[prob] language supports
@itemlist[
@item{the expression of generative probabilistic models, and}
@item{inference over those models.}
]

@section{Probabilistic Models}

A probabilistic model consists a sequence of definitions, expressions,
and conditions. A model should only use the pure subset of Racket plus
the random functions provided by this library.

Here is one of the simplest probabilistic models:

@interaction[#:eval the-eval
(flip)
]

The @racket[flip] function is a random function that flips a coin
(optionally biased) and returns a boolean result.

Random functions can be mixed with ordinary Racket code:

@interaction[#:eval the-eval
(repeat (lambda () (flip 0.3)) 10)
]

Conditions can be expressed using the @racket[fail] function:

@interaction[#:eval the-eval
(unless (for/and ([i 10]) (flip 0.3))
  (fail))
]

Models are typically executed in the context of a sampler or solver
designed to explore their probability distribution. The general form is

@racketblock[
(_sampler/solver-form
  _def/expr @#,(racketidfont "...")
  _sample-result-expr 
  #:when _condition-expr)
]

For example, @racket[rejection-sampler] creates a sampler that simply
runs a model until it generates a sample satisfying the given
condition.

@interaction[#:eval the-eval
(define s-2flips
  (rejection-sampler
    (define A (flip))
    (define B (flip))
    A
    #:when (or A B)))
(s-2flips)
(s-2flips)
]

Samplers can be run multiple times to estimate properties of the
probability distributions they represent or to finitely approximate
the distribution.

@interaction[#:eval the-eval
(sampler->mean+variance s-2flips 100 (indicator/value #t))
(sampler->discrete-dist s-2flips 100)
]

Probability distributions can be visualized with the simple
@racket[hist] function. More comprehensive visualization support is
available through the @racketmodname[plot] library.

@interaction[#:eval the-eval
(hist (repeat s-2flips 100))
]

Other sampler and solver forms use more sophisticated techniques to
explore the probability distribution represented by a probabilistic
model.


@section{Metropolis-Hastings Sampler}

The @racket[mh-sampler] form implements Metropolis-Hastings sampling.

Here is a simple function that calls @racket[flip] @racket[n] times
and counts the number of true results:

@interaction[#:eval the-eval
(define (count-true-flips n)
  (if (zero? n)
      0
      (+ (if (flip) 1 0)
         (count-true-flips (sub1 n)))))
]

We can define a MH sampler for @racket[count-true-flips] thus:

@interaction[#:eval the-eval
(define s-flips (mh-sampler (count-true-flips 10)))
]

Calling the sampler produces a sample, but it also records its
choices, so that subsequent calls can explore similar sequences of
choices. We can use @racket[verbose?] to see the choices as they're
made. 

@interaction[#:eval the-eval
(parameterize ((verbose? #t))
  (s-flips))
]

Each line ends with a series of numbers that identifies the
``address'' of the call to @racket[flip]; see @cite{Bher} for
details. (Note: if you get a ``collision'' error, check to make sure
your module is using @litchar{#lang prob}---the language performs
call-site instrumentation needed by @racket[mh-sampler].)

If we run the sampler again, we see that one of the choices is
resampled, and the rest are reused.

@interaction[#:eval the-eval
(parameterize ((verbose? #t))
  (s-flips))
]

As before, we can use various summarization and visualization
functions on the sampler:

@interaction[#:eval the-eval
(sampler->mean+variance s-flips 1000)
(sampler->discrete-dist s-flips 1000)
(hist (repeat s-flips 1000))
;; (hist (repeat (lambda () (count-true-flips 10)) 100))
]

ERP results can be memoized using the @racket[mem] higher-order
function:

@interaction[#:eval the-eval
(define s-mem
  (mh-sampler
    (define mflip (mem (lambda (i) (if (flip) 1 0))))
    (for/sum ([i 10]) (mflip (modulo i 5)))))
]

When we run this sampler, it makes fresh choices for the first five
flips, then reuses the memoized choices for the second five flips.

@interaction[#:eval the-eval
(parameterize ((verbose? #t))
  (s-mem))
]

Note: the call to @racket[mem] must happen in the dynamic extent of
the @racket[mh-sampler]; otherwise, naive memoization will be used
instead.

Certain kinds of conditions can be enforced directly using
@racket[observe-at], rather than sampling forward and rejecting if the
condition is unsatisfied. Indeed, for conditions on continuous random
variables, direct enforcement is the only feasible option.

@interaction[#:eval the-eval
(define (make-s-cd stddev_R)
  (mh-sampler
   (define R (normal 10 stddev_R))
   (observe-at (normal-dist R 1) 9)
   R))
(sampler->mean+variance (make-s-cd 3) 1000)
(sampler->mean+variance (make-s-cd .5) 1000)
]


@section{Enumeration via Delimited Continuations}

The second technique uses delimited continuations to make a
probability-weighted tree of possibile execution paths.

Exhaustive (or nearly exhaustive) exploration of the tree is done with
the @racket[enumerate] solver form.

@interaction[#:eval the-eval
(enumerate
  (count-true-flips 10))
]

The results above agree with the results produced by the
@racket[binomial] distribution:

@interaction[#:eval the-eval
(enumerate
  (binomial 10 1/2))
]

The @racket[enumerate] form can be used to approximate countable
distributions by using a limit parameter; the tree search stops when
the distribution is correct to within the given limit.

@interaction[#:eval the-eval
(define (geom)
  (if (flip) 0 (add1 (geom))))
(enumerate
  (geom)
  #:limit 1e-6)
]

Note that the probabilities are not quite the negative powers of 2,
because they are normalized after the search stops at @racket[19]. Use
@racket[#:normalize? #f] to skip normalization:

@interaction[#:eval the-eval
(enumerate
  (geom)
  #:limit 1e-6
  #:normalize? #f)
]

The @racket[enumerate] form supports memoization through
@racket[mem]:

@interaction[#:eval the-eval
(enumerate
  (define f (mem (lambda (n) (if (flip) 1 0))))
  (list (f 1) (f 2) (f 1) (f 2)))
]

The @racket[enumerate] form supports conditioning through a final
@racket[#:when] clause:

@interaction[#:eval the-eval
(enumerate
  (define A (flip))
  (define B (flip))
  A
  #:when (or A B))
]

@interaction[#:eval the-eval
(enumerate
  (define A (geom))
  A
  #:when (< 20 A 30)
  #:limit 1e-6)
]

Here's an example from @cite{EPP} that shows that this technique can
detect miniscule probabilities that sampling might miss. We disable
the limit to explore the tree fully, and we avoid normalizing the
resulting probabilities by the acceptance rate of the condition.

@interaction[#:eval the-eval
(enumerate
 (define (drunk-flip)
   (if (flip 0.9)
       (fail) (code:comment "dropped the coin")
       (flip .05)))
 (define (drunk-andflips n)
   (cond [(zero? n)
          #t]
         [else
          (and (drunk-flip)
               (drunk-andflips (sub1 n)))]))
 (drunk-andflips 10)
 #:normalize? #f)
]

Enumeration can be nested:

@interaction[#:eval the-eval
(enumerate
 (define A (flip))
 (define B
   (enumerate
    (define C (flip))
    (define D (flip))
    (or C D)
    #:when (or (and C D) A)))
 (list A B))
]

But a memoized function must not be used outside the context that
creates it, otherwise an error is raised:

@interaction[#:eval the-eval
(enumerate
  (define D
    (enumerate (mem flip)))
  (define f (vector-ref (discrete-dist-values D) 0))
  (f))
]

The technique of reification and reflection discussed in @cite{EPP}
can reduce the complexity of enumerating probabilities. Reification is
done using @racket[enumerate] and reflection with
@racket[sample]. The following pair of programs shows an exponential
search tree reduced to a linear one using reification and reflection.

@interaction[#:eval the-eval
(define (xor a b) (and (or a b) (not (and a b))))
(define (xor-flips n)
  (if (zero? n)
      #t
      (xor (flip) (xor-flips (sub1 n)))))
(time (enumerate (xor-flips 12)))
]

@interaction[#:eval the-eval
(define (xor-flips* n)
  (if (zero? n)
      #t
      (let ([r (sample (enumerate (xor-flips* (sub1 n))))])
        (xor (flip) r))))
(time (enumerate (xor-flips* 12)))
(time (enumerate (xor-flips* 120)))
]

Another technique is to delay choices until they are needed. The
@tt{letlazy} function in @cite{EPP} is subsumed by
@racket[mem]. Here's an example.

@interaction[#:eval the-eval
(define (flips-all-true n)
  (enumerate
    (define Flips (for/list ([i n]) (flip)))
    (andmap values Flips)))

(time (flips-all-true 12))
]

The search tree has 2@superscript{12} paths, but most of them are
redundant because when examining the flip results, we stop looking as
soon as we see a @racket[#f]. By making flips lazy, we only explore a
flip when it is actually relevant.

@interaction[#:eval the-eval
(define (flips-all-true* n)
  (enumerate
    (define LFlips (for/list ([i n]) (mem flip)))
    (andmap (lambda (f) (f)) LFlips)))
(time (flips-all-true* 12))
]

The @racket[enumerate] solver cannot handle continuous random
variables.

@(close-eval the-eval)
