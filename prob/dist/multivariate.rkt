;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         (rename-in racket/match [match-define defmatch])
         racket/math
         racket/flonum
         math/flonum
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "../private/dist.rkt"
         "../private/interfaces.rkt"
         "../private/dist-define.rkt"
         "../private/dist-impl.rkt"
         "../matrix.rkt")
(provide #| implicit in define-dist-type |#)

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
  #:variance cov
  #:drift (lambda (value scale-factor) (cons (multi-normal-drift value cov scale-factor) 0)))

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
            (values n V))
  #:drift (lambda (value scale-factor) (wishart-drift n V value scale-factor)))

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
            (values n Vinv))
  #:drift (lambda (value scale-factor)
            (define V (memo-matrix-inverse Vinv))
            (defmatch (cons value* R-F)
              (wishart-drift n V (memo-matrix-inverse value) scale-factor))
            (cons (memo-matrix-inverse value*) R-F)))

;; ============================================================
;; Multivariate dist functions

(define-memoize1 (memo-matrix-inverse m) (matrix-inverse m))
(define-memoize1 (memo-matrix-cholesky m) (matrix-cholesky m))
(define-memoize1 (memo-matrix-determinant m) (matrix-determinant m))

;; ------------------------------------------------------------
;; Multivariate normal dist functions

(define (multi-normal-pdf mean cov x log?)
  (define k (matrix-num-rows mean))
  (cond [(not (col-matrix? x))
         (impossible log? 'multi-normal "not column matrix")]
        [(not (= (matrix-num-rows x) k))
         (impossible log? 'multi-normal "wrong number of rows")]
        [else
         (define lp
           (+ (* -0.5 k (log (* 2 pi)))
              (* -0.5 (log (matrix-determinant cov)))
              (* -0.5 (matrix11->value
                       (let ([x-mean (matrix- x mean)])
                         (matrix* (matrix-transpose x-mean)
                                  (memo-matrix-inverse cov)
                                  x-mean))))))
         (if log? lp (exp lp))]))

(define (multi-normal-sample mean cov)
  (multi-normal-drift mean cov 1.0))

(define (multi-normal-drift value cov scale-factor)
  (define n (matrix-num-rows value))
  (define A (memo-matrix-cholesky cov))
  (define snv (flvector->vector (m:flnormal-sample 0.0 (exact->inexact scale-factor) n)))
  (matrix+ value (matrix* A (->col-matrix snv))))

;; ------------------------------------------------------------
;; Wishart dist functions

(define (multigamma p a)
  (* (expt pi (* p (sub1 p) 1/4))
     (for/product ([j (in-range p)])
       (m:gamma (- a (/ j 2))))))

(define (log-multigamma p a)
  (+ (* p (sub1 p) 0.25 (log pi))
     (for/sum ([j (in-range p)])
       (m:log-gamma (- a (/ j 2))))))

(define (wishart-pdf n V X log?)
  (define p (square-matrix-size V))
  (cond [(not (and (square-matrix? X) (matrix-symmetric? X)))
         (impossible log? 'wishart "not symmetric matrix")]
        [(not (= (matrix-num-rows X) p))
         (impossible log? 'wishart "wrong size")]
        [else
         (define lp
           (+ (* 1/2 (- n p 1) (log (memo-matrix-determinant X)))
              (* -1/2 (matrix-trace (matrix* (memo-matrix-inverse V) X)))
              (* -1/2 n p (log 2))
              (* 1/2 n (log (memo-matrix-determinant V)))
              (log-multigamma p (/ n 2))))
         (if log? lp (exp lp))]))

(define (wishart-sample/naive n V)
  (define p (square-matrix-size V))
  (define p-zeros (->col-matrix (make-vector p 0.0)))
  (matrix-sum
   (for/list ([i (in-range n)])
     (define X (multi-normal-sample p-zeros V))
     (matrix* X (matrix-transpose X)))))

;; Reference: http://www.math.wustl.edu/~sawyer/hmhandouts/Wishart.pdf
(define (wishart-sample n V*)
  ;; p1: If W ~ W(I_p,p,n) and V = L*L^T, 
  ;; then L W L^T^ ~ W(V,p,n).
  (define p (square-matrix-size V*))
  (define L (memo-matrix-cholesky V*))
  ;; p5, Theorem 3.1:
  (define V (build-vector p (lambda (i) (chi-squared-sample (- n i)))))
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
                 (+ (vector-ref V j)
                    (for/sum ([r (in-range j)])
                      (sqr (N r j)))))
    (for ([i (in-range j)])
      (matrix-set! B i j
                   (+ (* (N i j) (sqrt (vector-ref V i)))
                      (for/sum ([r (in-range i)])
                        (* (N r i) (N r j)))))))
  ;; Copy lower triangle to upper, overwriting N
  (for ([j (in-range p)])
    (for ([i (in-range j)])
      (matrix-set! B j i (matrix-ref B i j))))
  ;; B is now ~ W(I_p, p, n)
  (matrix* L B (matrix-transpose L)))

(define (chi-squared-sample p)
  (flvector-ref (m:flgamma-sample (* 0.5 p) 2.0 1) 0))

(define (wishart-drift n V value scale-factor)
  (define w (/ scale-factor (+ 1 scale-factor)))
  (define F-sample (wishart-sample n V))
  (define value*
    (matrix+ (array* value (array (- 1 w)))
             (array* (array w) F-sample)))
  (define F (wishart-pdf n V F-sample #t))
  ;; Reverse direction: solve value = (1 - w) * value* + w * R-sample
  (define R-sample
    (array* (matrix- value (array* (array (- 1 w)) value*))
            (array (/ w))))
  (define R (wishart-pdf n V R-sample #t))
  (cons value* (- R F)))


;; ------------------------------------------------------------
;; Inverse Wishart dist functions

(define (inverse-wishart-pdf n Vinv X log?)
  (wishart-pdf n (memo-matrix-inverse Vinv) (memo-matrix-inverse X) log?))

(define (inverse-wishart-sample n Vinv)
  (matrix-inverse (wishart-sample n (memo-matrix-inverse Vinv))))


;; ============================================================
;; Convenience functions

(define (multi-normal mean cov)
  (sample (multi-normal-dist mean cov)))
(define (wishart n V)
  (sample (wishart-dist n V)))
(define (inverse-wishart n V)
  (sample (inverse-wishart n V)))

(provide (contract-out
          [multi-normal
           (-> col-matrix? square-matrix? any)]
          [wishart
           (-> real? square-matrix? any)]
          [inverse-wishart
           (-> real? square-matrix? any)]))
