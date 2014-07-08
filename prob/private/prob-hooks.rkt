;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base racket/syntax)
         (only-in "dist.rkt" dist-sample))
(provide current-mem
         current-ERP
         current-fail
         current-label
         (struct-out spcond:equal)
         (struct-out spcond:drawn))

;; Basic memoization and ERP implementations

(define (base-mem f)
  (let ([memo-table (make-hash)])
    (define (memoized-function . args)
      (hash-ref! memo-table args (lambda () (apply f args))))
    memoized-function))

(define (base-ERP tag dist)
  (dist-sample dist))

(define (base-fail reason)
  (if reason
      (error 'fail "path failed\n  reason: ~s" reason)
      (error 'fail "path failed")))

;; ----

;; mem and ERP hooks; samplers/solvers override

;; mem : procedure -> procedure

(define current-mem (make-parameter base-mem))

;; ERP : (Sexpr Dist) -> A)
;; First arg is tag w/ ERP function name and params. Same tag should imply same dist.
;; Second is dist (or #f to represent fail).

(define current-ERP (make-parameter base-ERP))

;; fail : Any -> (escapes)
;; FIXME: Maybe change primitive from fail to fail-if (w/ boolean arg), so
;; can dynamically detect condition-free models?

(define current-fail (make-parameter base-fail))

;; ----

;; Conditions

;; Two ways to treat condition C:
;; - general: (unless C (fail))
;; - special: propagate C back to source variable(s)

;; General is trivial to implement, but sometimes useless, eg
;; condition X = x for continuous variable; rejection sampling
;; will "never" produce an acceptable sample. In those cases,
;; need to change how X is generated.

;; Special conditions:
;; - (= X value) -- X equal to given value (if prob 0 according to natural dist, reject)
;; - (~ X dist)  -- X drawn from dist (but still need to score according to natural dist)
;; Q: What scope is value/dist eval'd in?
;;    eg, Is (= X (if Y 1 2)) okay? What about (= X (if Y Z W))?
;; A: For model-level var X, should be env of X's definition, since it
;;    replaces the definition of X, roughly speaking.
;; Q: What about (if Y (= X 1) (= Z 2))? Is that a valid condition?
;;    It doesn't have clear scoping... could interpret more like
;;    (and (when Y (=! X 1)) (when (not Y) (=! Z 2))) and split apart, but bleh.

;; How to specify vars?
;; - by name (okay, need some syntactic support, skip for now)
;; - What about indexed collections?
;;   - eg, X(10,3) = c ??
;;   - eg, X(4,:) = <row-matrix-value> ??
;; For now, use 'label' form, require programmer to write explicitly.

(define current-label (make-parameter #f))

;; A Condition is (cons Label SpecialCond)
;; A Label is any value, eg symbol
;; A SpecialCond is one of
;; - (spcond:equal Any)
;; - (spcond:drawn Dist)
(struct spcond:equal (value))
(struct spcond:drawn (dist))
