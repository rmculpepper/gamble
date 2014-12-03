;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (only-in "context.rkt" obs-mark)
         (only-in "dist.rkt" dist-sample dist-pdf))
(provide verbose?
         weighted-sampler<%>
         sampler<%>
         weighted-sampler?
         sampler?
         sampler-base%
         stochastic-ctx<%>
         stochastic-ctx/run<%>
         current-stochastic-ctx
         plain-stochastic-ctx%
         plain-stochastic-ctx/run%
         current-zones
         zone-matches?
         some-zone-matches?
         current-label
         current-derivatives)

;; Defines interfaces, base classes, and parameters.

;; ============================================================
;; Debugging

;; FIXME: add logger?

(define verbose? (make-parameter #f))

;; ============================================================
;; Samplers

(define weighted-sampler<%>
  (interface ()
    sample/weight  ;; -> (cons Any PositiveReal)
    ))

;; A sampler is an applicable object taking zero arguments. When
;; applied, it produces a single sample.
(define sampler<%>
  (interface* (weighted-sampler<%>)
              ([prop:procedure (lambda (this) (send this sample))])
    sample  ;; -> Any
    ))

(define (weighted-sampler? x) (is-a? x weighted-sampler<%>))
(define (sampler? x) (is-a? x sampler<%>))

;; Automatic impl of weighted sampler from "ordinary" sampler.
(define sampler-base%
  (class* object% (sampler<%>)
    (super-new)
    (abstract sample)
    (define/public (sample/weight) (cons (sample) 1))))

;; ============================================================
;; Stochastic contexts

(define stochastic-ctx<%>
  (interface ()
    sample  ;; (Dist A) -> A
    observe-at ;; (Dist A) A -> Void
    fail    ;; Any -> (escapes)
    mem     ;; Function -> Function
    ))

(define stochastic-ctx/run<%>
  (interface (stochastic-ctx<%>)
    run     ;; (-> A) -> (U (cons 'okay A) (cons 'fail Any))
    ))

(define plain-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (super-new)

    (define/public (sample dist)
      (dist-sample dist))

    (define/public (observe-at dist val)
      ;; No ambient weight to affect; just check likelihood is non-zero.
      (when (zero? (dist-pdf dist val))
        (fail 'observation)))

    (define/public (mem f)
      (let ([memo-table (make-hash)])
        (define (memoized-function . args)
          (with-continuation-mark
              obs-mark 'ok
            (hash-ref! memo-table args (lambda () (apply f args)))))
        memoized-function))

    (define/public (fail reason)
      (if reason
          (error 'fail "failed\n  reason: ~s" reason)
          (error 'fail "failed")))
    ))

(define plain-stochastic-ctx/run%
  (class* plain-stochastic-ctx% (stochastic-ctx/run<%>)
    (init-field [escape-prompt (make-continuation-prompt-tag)])
    (super-new)

    (define/public (run thunk)
      (parameterize ((current-stochastic-ctx this))
        (call-with-continuation-prompt
         (lambda () (cons 'okay (thunk)))
         escape-prompt)))

    (define/override (fail reason)
      (abort-current-continuation
       escape-prompt
       (lambda () (cons 'fail reason))))
    ))

(define current-stochastic-ctx
  (make-parameter (new plain-stochastic-ctx%)))

;; ============================================================
;; Zones

;; A Zone is any non-false value (but typically a symbol or list with
;; symbols and numbers).

;; A ZonePattern is one of
;; - Zone  -- matches that zone
;; - #f    -- matches anything

(define current-zones (make-parameter null))

(define (zone-matches? z zp)
  (or (not zp) (equal? z zp)))

(define (some-zone-matches? zs zp)
  (or (not zp) (for/or ([z (in-list zs)]) (equal? z zp))))

;; ============================================================
;; Conditions

;; Two ways to treat condition C:
;; - general: (unless C (fail))
;; - special: propagate C back to source variable(s)

;; General is trivial to implement, but sometimes useless, eg
;; condition X = x for continuous variable; rejection sampling
;; will "never" produce an acceptable sample. In those cases,
;; need to change how X is generated.

;; Two kinds of special condition:
;; - (observe E value), where E contains a new choice
;;
;;   Need to propagate value to choice in E that it can affect; ideally,
;;   we should adjust through arithmetic, etc.
;;   eg, (observe (+ 5 (normal 0 1)) 6.3)
;;       => (observe (normal 0 1) 1.3)
;;       => (observe-at (normal-dist 0 1) 1.3)
;;
;; - (observe X value), where X is a variable (maybe indexed??)
;;
;;   This is harder, in a way; the scoping of the value expr is tricky.
;;   Need to discover these observations before evaluating X rhs,
;;   so will probably have to be restricted to "model-top-level".

;; Issues with (observe X value):
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

;; ============================================================
;; Partial derivatives

;; Each instance of a distribution (corresponding to an entry in the MCMC database)
;; may have attached a specification of the partial derivatives of its parameters with
;; respect to some of the other random variables.

;; (define Mean (label 'Mean (normal 0 1)))
;; (define Variable (label 'Variance (inverse-gamma 1 1)))
;; (define N (derivative (normal Mean (square Variance))
;;                       [(Mean)
;;                        (lambda (m) 1)]
;;                       [(Variance)
;;                        (λ (v) (* 2 v))]))
;;
;; For each parameter of normal-dist, there is a [(labels ...) expr]
;; term where the expression corresponds to (λ (label-vals ...)
;; (values dparam/dlabels ...))  that is, the expression returns as
;; many values as there are random variables that appear within the
;; parameter, and each such value represents the partial derivative of
;; the parameter expression with respect to that variable.  If a
;; parameter does not depend on any random choices, the corresponding
;; derivative may be #f (rather than [() (λ() 0)]).

;; current-derivatives : (Parameter (Vectorof Label-Derivative))
;; where
;;   Label-Derivative is (Pair (Vectorof Symbol)
;;                             (Real ... -> Real))
;;                       or #f
;; Furthermore, each Label-Derivative function takes as many
;; arguments as there are symbols in the vector.
(define current-derivatives (make-parameter #f))
