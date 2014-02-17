;; A language for probabilistic programming

#lang racket/base
(require (for-syntax racket/base)
         (except-in typed/racket/base #%module-begin)
         "private/instrument.rkt")
(provide (except-out (all-from-out typed/racket/base)
                     #%top-interaction)
         (rename-out [typed-instrumenting-module-begin #%module-begin]
                     [typed-instrumenting-top-interaction #%top-interaction]))

(define-syntax (typed-instrumenting-module-begin stx)
  (syntax-case stx ()
    [(ti-module-begin form ...)
     (with-syntax ([e-module-body
                    (local-expand #'(#%module-begin form ...) 'module-begin null)])
       #'(instrument e-module-body #:un))]))

(define-syntax (typed-instrumenting-top-interaction stx)
  (syntax-case stx ()
    [(ti-top-interaction . e)
     (let ([estx (local-expand #'(#%top-interaction . e) 'top-level #f)])
       (syntax-case estx (begin)
         [(begin form ...)
          #'(begin (instrumenting-top-interaction . form) ...)]
         [form
          (with-syntax ([e-form (local-expand #'form 'top-level null)])
            #'(instrument e-form #:un))]))]))

(require "private/prob-syntax.rkt")
(provide (all-from-out "private/prob-syntax.rkt"))

(module lib typed/racket/base
  (require/typed "private/lib.rkt"
                 [flip (case->
                        (-> Boolean)
                        (Real -> Boolean))]
                 [d2 (case->
                      (-> (U 0 1))
                      (Real -> (U 0 1)))]
                 [randn (Exact-Positive-Integer -> Exact-Nonnegative-Integer)]
                 ;; [mem (All (B A ... ) ((A ... -> B) -> (A ... -> B)))]
                 [mem (All (A R) (A -> R))]
                 [verbose? (Parameterof Boolean)]
                 )
  (provide flip
           d2
           randn
           mem
           verbose?))

(require 'lib)
(provide (all-from-out 'lib))

;; FIXME: extend type environment directly; require/typed is insufficient

;; FIXME: DrRacket REPL doesn't work
;; FIXME: provide doesn't work (?)

;; FIXME: use typed reader instead
(module reader syntax/module-reader
  prob/typed)
