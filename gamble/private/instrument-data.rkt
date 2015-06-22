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
(provide define/instr-protocol
         (for-syntax instr-fun-table
                     register-instrumented-fun!
                     instr-fun)
         declare-observation-propagator
         (for-syntax observation-propagators
                     non-random-first-order-funs))

;; ============================================================
;; Instrumented function protocol

(begin-for-syntax

 ;; instr-fun-table : (free-id-table Id => (cons Id (Listof Nat)))
 (define instr-fun-table
   (make-free-id-table))

 (define (register-instrumented-fun! id id* arity)
   (free-id-table-set! instr-fun-table id (cons id* arity)))

 (define-syntax-class instr-fun
   #:attributes (instr arity)
   (pattern f:id
            #:do [(define p (free-id-table-ref instr-fun-table #'f #f))]
            #:when p
            #:with instr (car p)
            #:attr arity (cdr p)))
 )

(define-syntax (define/instr-protocol stx)
  (syntax-parse stx
    #:literals (case-lambda)
    [(_ (f:id arg:id ...) body ...)
     #'(define/instr-protocol f (case-lambda [(arg ...) body ...]))]
    [(define/instr-protocol f:id (case-lambda [(arg:id ...) body ...] ...))
     (define/with-syntax (f*) (generate-temporaries #'(f)))
     (define/with-syntax arity (map length (syntax->datum #'((arg ...) ...))))
     #'(begin (define-values (f*)
                (case-lambda
                  [(addr obs arg ...)
                   (with ([ADDR addr] [OBS obs]) body ...)]
                  ...))
              (define-values (f)
                (case-lambda
                  [(arg ...)
                   (call-with-immediate-continuation-mark OBS-mark
                      (#%plain-lambda (obs) (f* (ADDR-mark) obs arg ...)))]
                  ...))
              (begin-for-syntax*
               (register-instrumented-fun!
                (quote-syntax f)
                (quote-syntax f*)
                'arity)))]))

(define-syntax (begin-for-syntax* stx)
  (syntax-case stx ()
    [(_ expr ...)
     (case (syntax-local-context)
       [(module top-level)
        #'(begin-for-syntax expr ...)]
       [else
        #'(define-syntaxes () (begin expr ... (values)))])]))


;; ============================================================
;; Observation propagators and non-random primitives

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
  rational?
  (lambda (y) (- y a ...))
  (lambda (x) 1))

;; Suppose (a ...) = (a0 ai ...)
;;   then y = (- a0 ai ... x)
;;   so   x = (- a0 ai ... y) = (- a ... y)
;; Suppose (a ...) = ()
;;   then x = (- y) = (- a ... y)
;; So can handle both cases with same pattern!
(declare-observation-propagator (- a ... _)
  rational?
  (lambda (y) (- a ... y))
  (lambda (x) 1))

(declare-observation-propagator (* a ... _)
  rational?
  (lambda (y) (/ y a ...))
  (lambda (x) (abs (/ (* a ...)))))

(declare-observation-propagator (/ a ... _)
  (lambda (y) (and (rational? y) (not (eq? y 0))))
  (lambda (y) (/ (/ a ... 1) y))
  (lambda (x) (abs (/ (* x x) (* a ...)))))

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
