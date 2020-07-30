;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         (only-in "dist/base.rkt" dist-sample dist-pdf)
         (only-in "util/real.rkt" logspace-zero?))
(provide sample
         mem
         lscore
         nscore
         fail
         trycatch
         weighted-sampler<%>
         sampler<%>
         weighted-sampler?
         sampler?
         sampler-base%
         stochastic-ctx<%>
         stochastic-ctx/run<%>
         current-stochastic-ctx
         plain-stochastic-ctx%
         plain-stochastic-ctx/run%)

;; Defines interfaces, base classes, and parameters.

;; ============================================================
;; Samplers

(define weighted-sampler<%>
  (interface ()
    sample/weight  ;; -> (cons Any PositiveReal)
    info           ;; -> Void
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
    (abstract info)
    (define/public (sample/weight) (cons (sample) 1))))

;; ============================================================
;; Stochastic contexts

(define stochastic-ctx<%>
  (interface ()
    sample  ;; (Dist A) Address -> A
    lscore  ;; LogReal -> Void
    nscore  ;; NNReal -> Void
    fail    ;; Any -> (escapes)
    mem     ;; Function -> Function
    ))

(define stochastic-ctx/run<%>
  (interface (stochastic-ctx<%>)
    run     ;; (-> A) -> (U (cons 'okay A) (cons 'fail Any))
    ))
#|
(define stochastic-ctx/trycatch<%>
  (interface (stochastic-ctx<%>)
    trycatch ;; (-> A) (-> A) -> A
    ))
|#

(define plain-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (super-new)

    (define/public (sample dist)
      (dist-sample dist))

    ;; No ambient weight to affect; just check likelihood is non-zero.
    (define/public (lscore ll)
      (when (logspace-zero? ll) (fail 'lscore)))
    (define/public (nscore l)
      (unless (> l 0) (fail 'nscore)))
    (define/public (observe d v scale)
      (lscore (dist-pdf d v #t)))

    (define/public (mem f)
      (let ([memo-table (make-hash)])
        (define (memoized-function . args)
          (hash-ref! memo-table args (lambda () (apply f args))))
        memoized-function))

    (define/public (fail reason)
      (if reason
          (error 'fail "failed\n  reason: ~e" reason)
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
;; Primitive operations

(define (mem f) (send (current-stochastic-ctx) mem f))
(define (lscore ll) (send (current-stochastic-ctx) lscore ll))
(define (nscore l) (send (current-stochastic-ctx) nscore l))
(define (observe dist val [scale 1])
  (send (current-stochastic-ctx) observe dist val scale))
(define (fail [reason #f]) (send (current-stochastic-ctx) fail reason))
(define (sample dist [id #f]) (send (current-stochastic-ctx) sample dist id))
(define (trycatch p1 p2)
  (send (current-stochastic-ctx) trycatch p1 p2))
