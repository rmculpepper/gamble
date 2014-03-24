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

The @racketmodname[prob] language implements two techniques for
probabilistic programming. One is an ``MH'' sampler based on the
log-replay technique of @cite{Bher}. The other is a tree exploration
based on @cite{EPP}, using delimited continuations.


@section{Log-replay sampler}

Here is a simple function that uses @racket[flip]:

@interaction[#:eval the-eval
(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (if (flip) 1 0)
         (sum-n-flips (sub1 n)))))
]

We can define a sampler for a program that uses @racket[sum-n-flips]
thus:

@interaction[#:eval the-eval
(define s-flips (mh-sampler (sum-n-flips 10)))
]

Calling the sampler produces a sample, but it also records its
choices, so that subsequent calls can explore similar sequences of
choices. We can use @racket[verbose?] to see the choices as they're
made. (Each line ends with a series of numbers that identifies the
``address'' of the call to @racket[flip]; see @cite{Bher} for
details.)

@interaction[#:eval the-eval
(parameterize ((verbose? #t))
  (s-flips))
]

If we run the sampler again, we see that one of the choices is
deleted, and the rest are reused.

@interaction[#:eval the-eval
(parameterize ((verbose? #t))
  (s-flips))
]

We can use @racket[repeat] and @racket[hist] to visualize the sampler
results:

@interaction[#:eval the-eval
(hist (repeat s-flips 1000))
;; (hist (repeat (lambda () (sum-n-flips 10)) 100))
]

Here's a program that uses @racket[mem] to memoize a flip:

@interaction[#:eval the-eval
(define s-mem
  (mh-sampler
    (define mflip (mem (lambda (i) (if (flip) 1 0))))
    (for/sum ([i 10]) (mflip (modulo i 5)))))
]

When we apply this sampler, it makes fresh choices for the first five
flips, then reuses the memoized choices for the second five flips.

@interaction[#:eval the-eval
(parameterize ((verbose? #t))
  (s-mem))
]

Note: the call to @racket[mem] must happen in the dynamic extent of
the @racket[mh-sampler]; otherwise, the wrong kind of memoization will
be used.


@section{Enumeration via Delimited Continuations}

The second technique uses delimited continuations to make a
probability-weighted tree of possibilities.

@interaction[#:eval the-eval
(enumerate
  (sum-n-flips 10))
]

This agrees with the results produced by the @racket[binomial]
distribution:

@interaction[#:eval the-eval
(enumerate
  (binomial 10 1/2))
]

The @racket[enumerate] form can be used to approximate countable
distributions by using a limit parameter; the tree search stops when
the distribution is correct to within the given limit. (FIXME: is this
description quite right?)

@interaction[#:eval the-eval
(define (geom)
  (if (flip) 0 (add1 (geom))))
(enumerate
  (geom)
  #:limit 1e-6)
]

Note that the probabilities are not quite the negative powers of 2,
because they are normalized after the search stops at
@racket[19]. There is an option to skip normalization, however:

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

The @racket[enumerate] form also supports conditioning:

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
  #:when (< 20 A 30))
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
 #:normalize? #f
 (code:comment "Need to disable limit to detect #t case")
 #:limit #f)
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
  (define f+prob (car D))
  (define f (car f+prob))
  (f))
]

The technique of reification and reflection discussed in @cite{EPP}
can reduce the complexity of enumerating probabilities. Reification is
done using @racket[enumerate] and reflection with
@racket[discrete-from-enumeration]. The following pair of programs
shows an exponential search tree reduced to a linear one using
reification and reflection.

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
      (let ([r (discrete-from-enumeration
                (enumerate (xor-flips* (sub1 n))))])
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

@(close-eval the-eval)
