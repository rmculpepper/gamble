;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "util/density.rkt"
         (only-in "dist/base.rkt" dist-sample dist-density))
(provide sample
         observe
         dscore
         lscore
         fail
         mem
         weighted-sampler<%>
         sampler<%>
         weighted-sampler?
         sampler?
         sampler-base%
         stochastic-ctx<%>
         current-stochastic-ctx
         plain-stochastic-ctx%)

;; Defines interfaces, base classes, and parameters.

;; ============================================================
;; Samplers

(define weighted-sampler<%>
  (interface ()
    sample/weight  ;; -> (cons A PosReal) or #f
    ))

(define sampler<%>
  (interface (weighted-sampler<%>)
    sample  ;; -> A
    ))

(define (weighted-sampler? x) (is-a? x weighted-sampler<%>))
(define (sampler? x) (is-a? x sampler<%>))

;; Automatic impl of weighted sampler from "ordinary" sampler.
(define sampler-base%
  (class* object% (sampler<%>)
    (super-new)
    (define/public (sample/weight) (cons (sample) 1))
    (abstract sample)))

;; ============================================================
;; Stochastic contexts

(define stochastic-ctx<%>
  (interface ()
    sample      ;; (Dist A) Label -> A
    observe     ;; Dist[X] X -> Void
    dscore      ;; Density -> Void
    lscore      ;; LogReal Nat -> Void
    mem         ;; (X ... -> Y) -> (X ... -> Y)

    run         ;; (-> A ...) -> (U (list A ...) #f)
    fail        ;; -> escapes
    ))

(define plain-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (field [escape-prompt (make-continuation-prompt-tag)])
    (super-new)

    (define/public (sample dist _label)
      (dist-sample dist))

    ;; No ambient weight to affect; just check likelihood is non-zero.
    (define/public (dscore dn)
      (when (density-zero? dn) (fail 'dscore)))
    (define/public (lscore ll ddim)
      (dscore (density ll ddim #t)))
    (define/public (observe d v)
      (dscore (dist-density d v)))

    (define/public (mem f)
      (define memo-table (make-hash))
      (define (mf . args)
        (unless (eq? (current-stochastic-ctx) this)
          (error (or (object-name f) 'memoized-function)
                 "called in different stochastic context"))
        (hash-ref! memo-table args (lambda () (apply f args))))
      (define fname (object-name f))
      (define name
        (cond [name (string->symbol (format "memoized-~a" name))]
              [else 'memoized-function]))
      (procedure-reduce-arity mf (procedure-arity f) name))

    (define/public (run thunk)
      (parameterize ((current-stochastic-ctx this))
        (call-with-continuation-prompt
         (lambda () (call-with-values thunk list))
         escape-prompt)))

    (define/public (fail reason)
      (abort-current-continuation
       escape-prompt
       (lambda () #f)))
    ))

(define current-stochastic-ctx
  (make-parameter (new plain-stochastic-ctx%)))

;; ============================================================
;; Primitive operations

(define (sample dist [label #f])
  (send (current-stochastic-ctx) sample dist label))

(define (dscore dn) (send (current-stochastic-ctx) dscore dn))
(define (lscore ll) (send (current-stochastic-ctx) lscore ll))
(define (observe dist val) (send (current-stochastic-ctx) observe dist val))

(define (mem f) (send (current-stochastic-ctx) mem f))
(define (fail [reason #f]) (send (current-stochastic-ctx) fail reason))
