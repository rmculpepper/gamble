;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; A language for probabilistic programming

#lang racket/base
(require (for-syntax racket/base
                     "private/instrument-analysis.rkt")
         (rename-in typed/racket/base
                    [#%module-begin typed-module-begin]
                    [#%top-interaction typed-top-interaction])
         "private/instrument.rkt")
(provide (all-from-out typed/racket/base)
         (rename-out [typed-instrumenting-module-begin #%module-begin]
                     [typed-instrumenting-top-interaction #%top-interaction]))

(define-syntax (typed-instrumenting-module-begin stx)
  (syntax-case stx ()
    [(ti-module-begin form ...)
     (with-syntax ([e-module-body
                    (analyze
                     (local-expand #'(#%plain-module-begin form ...) 'module-begin null))])
       (with-syntax ([(pmb instr-form ...)
                      (local-expand #'(instrument e-module-body #:un) 'module-begin null)])
         #'(typed-module-begin instr-form ...)))]))

(define-syntax (typed-instrumenting-top-interaction stx)
  (syntax-case stx ()
    [(ti-top-interaction . e)
     (let ([estx (local-expand #'(typed-top-interaction . e) 'top-level #f)])
       (syntax-case estx (begin)
         [(begin form ...)
          #'(begin (instrumenting-top-interaction . form) ...)]
         [form
          (with-syntax ([e-form (analyze (local-expand #'form 'top-level null))])
            #'(instrument e-form #:un))]))]))

(require "private/prob-syntax.rkt")
(provide (all-from-out "private/prob-syntax.rkt"))

(module instrumentation-types typed-racket/base-env/extra-env-lang
  (require "private/context.rkt"
           (for-syntax typed-racket/rep/type-rep))
  (type-environment
   [obs-mark (make-Continuation-Mark-Keyof Univ)]
   [observing? (-Param Univ Univ)]))

(require 'instrumentation-types)
(provide (all-from-out 'instrumentation-types))

;; FIXME: extend type environment directly; require/typed is insufficient

;; FIXME: DrRacket REPL doesn't work
;; FIXME: provide doesn't work (?)

;; FIXME: use typed reader instead
(module reader syntax/module-reader
  prob/typed)
