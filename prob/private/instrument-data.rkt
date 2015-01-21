;; Copyright (c) 2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/id-table)
         "interfaces.rkt"
         "context.rkt")
(provide declare-observation-propagator
         (for-syntax observation-propagators
                     non-random-first-order-funs))

;; ------------------------------------------------------------
;; Compile-time tables

(begin-for-syntax

  ;; id-table[ (Listof ObservationPropagator) ]
  (define observation-propagators (make-free-id-table))

  ;; An ObservationPropagator
  ;; - (list '#:final-arg Identifier Identifier Identifier)
  ;; - (list '#:all-args Identifier (listof Identifier))

  (define (register-final-arg-propagator! id pred inverter scaler)
    (free-id-table-set! observation-propagators id (list '#:final-arg pred inverter scaler)))

  ;; non-random-first-order-funs : free-id-table[ #t ]
  (define non-random-first-order-funs (make-free-id-table))

  (define (register-non-random-first-order-fun! id)
    (free-id-table-set! non-random-first-order-funs id #t))
  )

;; ------------------------------------------------------------
;; Macros for adding to tables

(define-syntax (declare-non-random-first-order stx)
  (syntax-parse stx
    [(declare-non-random-first-order f:id ...)
     #'(begin-for-syntax
         (register-non-random-first-order #'f) ...)]))

(define-syntax (declare-observation-propagator stx)
  (define-syntax-class ID
    (pattern (~and _:id (~not (~literal ...)) (~not (~literal _)))))
  (syntax-parse stx
    [(declare-observation-propagator
      (op:ID arg:ID ... (~literal _))
      ok?:expr inverter:expr scaler:expr)
     #'(begin
         (define-syntax-rule (pred y arg ...) (ok? y))
         (define-syntax-rule (invert y arg ...) (inverter y))
         (define-syntax-rule (scale x arg ...) (scaler x))
         (begin-for-syntax
           (register-final-arg-propagator! #'op #'pred #'invert #'scale)))]
    [(declare-observation-propagator
      (op:ID arg:ID ... rest-arg:ID (~literal ...) (~literal _))
      ok?:expr inverter:expr scaler:expr)
     #'(begin
         (define-syntax-rule (pred y arg ... rest-arg (... ...)) (ok? y))
         (define-syntax-rule (invert y arg ... rest-arg (... ...)) (inverter y))
         (define-syntax-rule (scale x arg ... rest-arg (... ...)) (scaler x))
         (begin-for-syntax
           (register-final-arg-propagator! #'op #'pred #'invert #'scale)))]))

;; ------------------------------------------------------------

#|
observe y(σ) = v
 where y(σ) = f(x(σ))
 want to weight by
     dσ/dy                      -- note inverse!
   = (dy/dσ)^-1
   = [ df/dx * dx/dσ ]^-1
   = (df/dx)^-1 * dσ/dx
   = (df/dx)^-1 * p_x(x)
     where x = f^-1(v)

eg, if f(x) = 2x
  then weight by 1/2

eg, if f(x) = x^3
  then weight by [ 3x^2 ]^-1
  --- except x^n does not have unique inverse

eg, if f(x) = exp(x)
  then weight by [ exp(x) ]^-1
|#

;; FIXME: add predicate; if y fails predicate, then fail
;; eg, for arithmetic, should be real?, for matrix* should be matrix (of right shape?)

;; FIXME: return log(1/scale) instead?
;; Exact arithmetic mostly good for discrete cases, so doesn't matter ???

;; FIXME: real? vs number? (ie, complex?) --- scale requires real ???

(declare-observation-propagator (+ a ... _)
  real?
  (lambda (y) (- y a ...))
  (lambda (x) 1))

;; Suppose (a ...) = (a0 ai ...)
;;   then y = (- a0 ai ... x)
;;   so   x = (- a0 ai ... y) = (- a ... y)
;; Suppose (a ...) = ()
;;   then x = (- y) = (- a ... y)
;; So can handle both cases with same pattern!
(declare-observation-propagator (- a ... _)
  real?
  (lambda (y) (- a ... y))
  (lambda (x) 1))

(declare-observation-propagator (* a ... _)
  real?
  (lambda (y) (/ y a ...))
  (lambda (x) (abs (/ (* a ...)))))

(declare-observation-propagator (exp _)
  real?
  (lambda (y) (log y))
  (lambda (x) (/ (exp x)))) ;; FIXME: pass y = (exp x) too?

(declare-observation-propagator (log _)
  real?
  (lambda (y) (exp y))
  (lambda (x) (abs x)))
;; dx/dy = (dy/dx)^-1 = (1/x)^-1 = x

(begin-for-syntax
  (free-id-table-set! observation-propagators #'cons
                      (list '#:all-args #'pair? (list #'car #'cdr))))

;; FIXME: use 0 for scale of non-continuous operations?
