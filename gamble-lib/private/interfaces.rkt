;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "util/density.rkt"
         (only-in "dist/base.rkt" dist-sample dist-density)
         (only-in "dist/discrete.rkt" for/discrete-dist))
(provide (all-defined-out))

;; Defines interfaces, base classes, and parameters.

;; ============================================================
;; Samplers

(define weighted-sampler<%>
  (interface ()
    sample/weight  ;; -> (values A PosReal)

    burn                        ;; Nat -> Void
    generate-discrete-dist      ;; Nat -> DiscreteDist
    generate-weighted-samples   ;; Nat -> (values (Vectorof A) (Vectorof PosReal))
    ))

(define sampler<%>
  (interface (weighted-sampler<%>)
    sample  ;; -> A

    generate-samples            ;; Nat -> (Vectorof A)
    ))

(define (weighted-sampler? x) (is-a? x weighted-sampler<%>))
(define (sampler? x) (is-a? x sampler<%>))

(define weighted-sampler-base%
  (class* object% (weighted-sampler<%>)
    (super-new)

    (abstract sample/weight)

    (define/public (burn n)
      (for ([i (in-range n)])
        (sample/weight))
      (void))

    (define/public (generate-discrete-dist n)
      (for/discrete-dist ([i (in-range n)])
        (sample/weight)))

    (define/public (generate-weighted-samples n)
      (define vs (make-vector n))
      (define ws (make-vector n))
      (for ([i (in-range n)])
        (define-values (v w) (sample/weight))
        (vector-set! vs i v)
        (vector-set! ws i w))
      (values vs ws))
    ))

(define sampler-base%
  (class* weighted-sampler-base% (sampler<%>)
    (super-new)

    (define/override (sample/weight) (values (sample) 1))
    (abstract sample)

    ;; ----

    (define/public (generate-samples n)
      (define vs (make-vector n))
      (for ([i (in-range n)])
        (vector-set! vs i (sample)))
      vs)
    ))

(define (sampler->discrete-dist s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-discrete-dist n))
(define (generate-samples s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-samples n))
(define (generate-weighted-samples s n #:burn [nburn 0])
  (send s burn nburn)
  (send s generate-weighted-samples n))

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
      (error 'dscore "called outside of sampling context"))
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
        (cond [fname (string->symbol (format "memoized-~a" fname))]
              [else 'memoized-function]))
      (procedure-reduce-arity mf (procedure-arity f) name))

    (define/public (run thunk)
      (parameterize ((current-stochastic-ctx this))
        (call-with-continuation-prompt
         (lambda () (call-with-values thunk list))
         escape-prompt)))

    (define/public (fail reason)
      (unless (continuation-prompt-available? escape-prompt)
        (error 'fail "called outside of sampling context"))
      (abort-current-continuation escape-prompt (lambda () #f)))
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
