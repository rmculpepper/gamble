;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     syntax/parse/experimental/eh
                     racket/syntax)
         racket/contract
         racket/match
         racket/math
         racket/pretty
         racket/dict
         racket/generic
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "../private/dist.rkt"
         "../private/dist-define.rkt"
         "../private/dist-impl.rkt"
         "../matrix.rkt")
(provide #| implicit in define-dist-type |#)

;; If every q is 0, returns 0 without evaluating e.
(define-syntax-rule (ifnz [q ...] e)
  (if (and (zero? q) ...) 0 e))

;; Multiply, but short-circuit if first arg evals to 0.
(define-syntax-rule (lazy* a b ...)
  (let ([av a]) (ifnz [av] (* av b ...))))

(define (digamma x) (m:psi0 x))

;; ----

(define-dist-type multi-normal-dist
  ([mean col-matrix?] [cov square-matrix?])
  #:pdf multi-normal-pdf
  #:sample multi-normal-sample
  #:guard (lambda (mean cov _name)
            (define n (matrix-num-rows mean))
            (unless (= n (square-matrix-size cov))
              (error 'multi-normal-dist
                     "covariance matrix has wrong number of rows\n  expected: ~s\n value: ~e"
                     n cov))
            ;; check cov is symmetric, nonnegative-definite (?)
            (values mean cov))
  #:mean mean
  #:modes (list mean) ;; FIXME: degenerate cases?
  #:variance cov)

(define-dist-type wishart-dist
  ([n real?] [V square-matrix?])
  #:pdf wishart-pdf #:sample wishart-sample
  #:guard (lambda (n V _name)
            (unless (matrix-symmetric? V)
              (error 'wishart-dist "expected symmetric matrix\n  given: ~e" V))
            (define p (square-matrix-size V))
            (unless (> n (- p 1))
              (error 'wishart-dist "expected n > p - 1\n  n: ~e\n  p: ~e" n p))
            ;; FIXME: check V positive definite
            (values n V)))

(define-dist-type inverse-wishart-dist
  ([n real?] [Vinv square-matrix?])
  #:pdf inverse-wishart-pdf #:sample inverse-wishart-sample
  #:guard (lambda (n Vinv _name)
            (unless (matrix-symmetric? Vinv)
              (error 'wishart-dist "expected symmetric matrix\n  given: ~e" Vinv))
            (define p (square-matrix-size Vinv))
            (unless (> n (- p 1))
              (error 'wishart-dist "expected n > p - 1\n  n: ~e\n  p: ~e" n p))
            ;; FIXME: check V positive definite
            (values n Vinv)))
