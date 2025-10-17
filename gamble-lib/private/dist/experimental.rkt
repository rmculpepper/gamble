;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/contract/base
         racket/match
         racket/math
         racket/flonum
         racket/vector
         racket/generic
         scramble/struct
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         (only-in math/flonum flvector->vector)
         "base.rkt"
         "../util/matrix.rkt"
         (submod "util.rkt" define)
         (submod "util.rkt" math))
(provide (all-defined-out))

(struct multi-normal-dist
  (mean   ;; Column-Matrix[n,1]
   cov    ;; Matrix[n,n] -- symmetric, positive definite
   covchl ;; Matrix[n,n] -- lower-triangular, Cholesky decomposition
   covdet ;; Flonum      -- determinant of cov
   covinv ;; Matrix[n,n] -- inverse of cov
   )
  #:property prop:auto-custom-write '(0 1)
  #:property prop:auto-equal+hash '(0 1)
  #:methods gen:dist
  [(define (-sample self)
     (match-define (multi-normal-dist mean cov covchl _ _) self)
     (define n (matrix-num-rows mean))
     (define snv (flvector->vector (m:flnormal-sample 0.0 1.0 n)))
     (matrix+ mean (matrix* covchl (->col-matrix snv))))
   (define (-pdf self x log?)
     (match-define (multi-normal-dist mean cov _ covdet covinv) self)
     (define n (matrix-num-rows mean))
     (cond [(not (col-matrix? x)) (if log? -inf.0 0)]
           [(not (= (matrix-num-rows x) n)) (if log? -inf.0 0)]
           [else
            (define lp
              (+ (* -0.5 n (log (* 2 pi)))
                 (* -0.5 (log covdet))
                 (* -0.5 (matrix11->value
                          (let ([x-mean (matrix- x mean)])
                            (matrix* (matrix-transpose x-mean) covinv x-mean))))))
            (if log? lp (exp lp))]))])


(define (make-multi-normal-dist mean cov)
  (define who 'multi-normal-dist)
  (define n (matrix-num-rows mean))
  (unless (= n (square-matrix-size cov))
    (error who "covariance matrix has wrong shape\n  expected shape: ~e\n  given shape: ~e"
           (vector n n) (array-shape n)))
  ;; FIXME: check cov is symmetric
  ;; FIXME: check cov is positive-definite
  (define covchl (matrix-cholesky cov))
  (define covdet (matrix-determinant cov))
  (define covinv (matrix-inverse cov))
  (multi-normal-dist mean cov covchl covdet covinv))

;; ----------------------------------------

(struct wishart-dist
  (V      ;; Matrix[n,n]
   n      ;; Flonum
   Vinv   ;; Matrix[n,n] -- inverse of V
   Vchl   ;; Matrix[n,n] -- lower triangular, Cholesky decomposition of V
   Vdet   ;; Flonum      -- determinant of V
   )
  #:property prop:auto-custom-write '(0 1)
  #:property prop:auto-equal+hash '(0 1)
  #:methods gen:dist
  [(define (-sample self)
     ;; Reference: http://www.math.wustl.edu/~sawyer/hmhandouts/Wishart.pdf
     ;; Technique from Odell and Feiveson (1966), referenced by Liu (2001)
     ;; 1. Sample B ~ W(Iₚ,p,n)
     ;; 2. Then L B Lᵀ ~ W(V,p,n) where V = L Lᵀ
     (match-define (wishart-dist _ n _ Vchl Vdet) self)
     (define p (square-matrix-size Vchl))
     (define B (standard-wishart-sample p n))
     (matrix* Vchl B (matrix-transpose Vchl)))
   (define (-pdf self X log?)
     (match-define (wishart-dist V n Vinv Vchl Vdet) self)
     (define p (square-matrix-size V))
     (cond [(not (and (square-matrix? X) (matrix-symmetric? X) (= (matrix-num-rows X) p)))
            (if log? -inf.0 0)]
           [else
            (define lp
              (+ (* 0.5 (- n p 1) (log (matrix-determinant X)))
                 (* -0.5 (matrix-trace (matrix* Vinv X)))
                 (* -0.5 n p (log 2))
                 (* 0.5 n (log Vdet))
                 (log-multigamma p (* 0.5 n))))
            (if log? lp (exp lp))]))])

;; make-wishart-dist : Matrix[p,p] Real -> Dist
;; where V is scale matrix, symmetric and positive definite
(define (make-wishart-dist V n)
  (define who 'wishart-dist)
  (unless (and (matrix? V) (square-matrix? V) (matrix-symmetric? V))
    (raise-argument-error who "(and/c matrix? square-matrix? matrix-symmetric?)" V))
  (define p (square-matrix-size V))
  ;; check V is positive semi-definite
  (unless (and (real? n) (> n (- p 1)))
    (raise-argument-error who "(and/c real? (>/c (sub1 (square-matrix-size V))))" n))
  (wishart-dist V (fl n)))

;; standard-wishart-sample : Nat Real -> Matrix[p,p]
;; Samples B ~ W(Iₚ,n) where p is matrix size, n is degrees of freedom.
;; Reference: http://www.math.wustl.edu/~sawyer/hmhandouts/Wishart.pdf
(define (standard-wishart-sample p n)
  ;; p5, Theorem 3.1:
  (define Vi (build-vector p (lambda (i) (chi-squared-sample (- n i)))))
  (define B (make-mutable-matrix p p 0.0))
  ;; Trick: store Nij (i<j)at Bji -- ie, upper triangle of B (not incl. diag.)
  (for ([j (in-range p)])
    (for ([i (in-range j)]
          [Nij (in-flvector (m:flnormal-sample 0.0 1.0 j))])
      (matrix-set! B j i Nij)))
  (define (N i j)
    (unless (< i j) (error 'wishart-sample "INTERNAL ERROR: ~s, ~s" i j))
    (matrix-ref B j i))
  (for ([j (in-range p)])
    (matrix-set! B j j
                 (+ (vector-ref Vi j)
                    (for/sum ([r (in-range j)])
                      (sqr (N r j)))))
    (for ([i (in-range j)])
      (matrix-set! B i j
                   (+ (* (N i j) (sqrt (vector-ref Vi i)))
                      (for/sum ([r (in-range i)])
                        (* (N r i) (N r j)))))))
  ;; Copy lower triangle to upper, overwriting Nij
  (for ([j (in-range p)])
    (for ([i (in-range j)])
      (matrix-set! B j i (matrix-ref B i j))))
  ;; Now B ~ W(Iₚ, p, n)
  B)

(define (multigamma p a)
  (* (expt pi (* 0.25 p (sub1 p)))
     (for/product ([j (in-range p)])
       (m:gamma (- a (* 0.5 j))))))

(define (log-multigamma p a)
  (+ (* 0.25 p (sub1 p) (log pi))
     (for/sum ([j (in-range p)])
       (m:log-gamma (- a (* 0.5 j))))))

(define (chi-squared-sample p)
  (flvector-ref (m:flgamma-sample (* 0.5 p) 2.0 1) 0))
