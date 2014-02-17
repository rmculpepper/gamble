;; A language for probabilistic programming

#lang racket/base
(require "private/instrument.rkt"
         "private/lib.rkt"
         "private/ho-functions.rkt")
(provide (except-out (all-from-out racket/base)
                     #%module-begin #%top-interaction)
         (rename-out [instrumenting-module-begin #%module-begin]
                     [instrumenting-top-interaction #%top-interaction])
         describe-all-call-sites
         describe-call-site
         (all-from-out "private/lib.rkt")
         (all-from-out "private/ho-functions.rkt"))

#|
See private/context.rkt for discussion of Address representation.
See private/lib.rkt for library functions and syntax (flip, samplers, etc).
See private/instrument.rkt for the implementation of the call-site instrumenter.
|#

#|

TODO: avoid instrumenting *safe* applications?
  - eg known first-order functions like list, +, etc
  - potential benefits: speed, code size, size of address reps

Issue: interop w/ Racket, etc
- solved issue with for/*, other macros
- HO functions still not ok
- Idea: develop mem-like context-replacement/augmentation discipline?
  eg, (lift map) = (lambda (f xs) (map (lift* f) xs))
      where (lift* f) = (lambda (x) (app/call-site (list 'map f x) f x)) ????

|#

(module reader syntax/module-reader
  prob)
