;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base racket/contract gamble gamble/pict))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/class gamble gamble/pict))
@(the-eval '(random-seed 1))

@title[#:tag "intro"]{Introduction}

The @racketmodname[gamble] language supports
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
(sample (bernoulli-dist 1/2))
]

The @racket[sample] function samples a value from the given distribution. The
@racket[bernoulli-dist] function constructs a distribution on the values
@racket[1] and @racket[0], where @racket[1] has the given probability. (There is
also a @racket[boolean-dist] type that uses true and false instead.)

Random functions can be mixed with ordinary Racket code:

@interaction[#:eval the-eval
(for/list ([i 10]) (sample (bernoulli-dist 1/2)))
]

Conditions can be expressed using the @racket[fail] function:

@interaction[#:eval the-eval
(unless (for/and ([i 10]) (sample (boolean-dist 0.2)))
  (fail))
]

Models are typically executed in the context of a sampler or solver designed to
explore their probability distribution.
For example, the following program uses @racket[rejection-sampler] to create a
sampler and uses @racket[sampler->discrete-dist] to generate the samples and
collect them as an @emph{unnormalized} discrete distribution:

@interaction[#:eval the-eval
(define s-or2flips
  (rejection-sampler
    (lambda ()
      (define A (sample (boolean-dist 1/2)))
      (define B (sample (boolean-dist 1/2)))
      (or A B))))
(sampler->discrete-dist s-or2flips 10)
]

In addition to sampling from random distributions, programs can also
perform observations specified by a condition expression using
@racket[fail]. A rejection sampler will simply run the model
until it generates a sample satisfying the given condition.

@interaction[#:eval the-eval
(define s-A-given-AorB
  (rejection-sampler
    (lambda ()
      (define A (sample (boolean-dist 1/2)))
      (define B (sample (boolean-dist 1/2)))
      (unless (or A B) (fail))
      A)))
(sampler->discrete-dist s-A-given-AorB 10)
]

Probability distributions can be visualized with the simple
@racket[dist->pict] function. More comprehensive visualization support is
available through the @racketmodname[plot] library.

@interaction[#:eval the-eval
(dist->pict (binomial-dist 10 1/3))
(dist->pict (sampler->discrete-dist s-A-given-AorB 1000))
]

Other sampler and solver forms use more sophisticated techniques to explore the
probability distribution represented by a probabilistic model. For example, here
is an importance sampler; the @racket[dist-rescore] function is used to
normalize the resulting distribution.

@interaction[#:eval the-eval
(define is
  (importance-sampler
    (lambda ()
      (define x (sample (normal-dist 10 2)))
      (observe (normal-dist x 1) 8.0)
      x)))
(define isd (sampler->discrete-dist is 1000))
(dist->pict (dist-rescore isd))
]


@; ------------------------------------------------------------
@section{MCMC Sampler}

The @racket[mcmc-sampler] function implements MCMC sampling.

Here is a simple model

@interaction[#:eval the-eval
(define s-obs
  (mcmc-sampler
    (lambda ()
      (define x (sample (uniform-dist 0 10) 'x))
      (observe (normal-dist x 1) 7.1)
      (observe (normal-dist x 1) 7.8)
      (observe (normal-dist x 1) 7.3)
      x)))
#;(sampler->discrete-dist s-obs 10)
(dist->pict (sampler->discrete-dist s-obs 1000))
(dist->pict (dist-rescore (sampler->discrete-dist s-obs 1000)))
]



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
your module is using @litchar{#lang gamble}---the language performs
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
@racket[observe-sample], rather than sampling forward and rejecting if the
condition is unsatisfied. Indeed, for conditions on continuous random
variables, direct enforcement is the only feasible option.

@interaction[#:eval the-eval
(define (make-s-cd stddev_R)
  (mh-sampler
   (define R (normal 10 stddev_R))
   (observe-sample (normal-dist R 1) 9)
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
  #:limit 1e-6
  (geom))
]

Note that the probabilities are not quite the negative powers of 2,
because they are normalized after the search stops at @racket[19]. Use
@racket[#:normalize? #f] to skip normalization:

@interaction[#:eval the-eval
(enumerate
  #:limit 1e-6
  #:normalize? #f
  (geom))
]

The @racket[enumerate] form supports memoization through
@racket[mem]:

@interaction[#:eval the-eval
(enumerate
  (define f (mem (lambda (n) (if (flip) 1 0))))
  (list (f 1) (f 2) (f 1) (f 2)))
]

The @racket[enumerate] form supports conditioning:

@interaction[#:eval the-eval
(enumerate
  #:limit 1e-6
  (define A (geom))
  (observe/fail (< 20 A 30))
  A)
]

Here's an example from @cite{EPP} that shows that this technique can
detect miniscule probabilities that sampling might miss. We disable
the limit to explore the tree fully, and we avoid normalizing the
resulting probabilities by the acceptance rate of the condition.

@interaction[#:eval the-eval
(enumerate
 #:normalize? #f
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
 (drunk-andflips 10))
]

Enumeration can be nested:

@interaction[#:eval the-eval
(enumerate
 (define A (flip))
 (define B
   (enumerate
    (define C (flip))
    (define D (flip))
    (observe/fail (or (and C D) A))
    (or C D)))
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
