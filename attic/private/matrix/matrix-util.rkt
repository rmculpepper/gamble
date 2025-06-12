;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang typed/racket/base
(require math/array
         math/matrix
         racket/math)
(provide matrix11->value
         array->immutable-array
         make-mutable-matrix
         matrix-set!
         matrix-symmetric?
         array-sqrt/nan
         array-sqrt/err
         matrix-cholesky
         matrix-ldl)

(: matrix11->value : (All (A) (Matrix A) -> A))
(define (matrix11->value m)
  (unless (and (square-matrix? m)
               (= 1 (square-matrix-size m)))
    (error 'matrix11->value "expected 1 by 1 matrix\n  given: ~e" m))
  (matrix-ref m 0 0))

(: array->immutable-array : (All (A) (Array A) -> (Array A)))
(define (array->immutable-array a)
  (array-map (inst values A) a))

(: make-mutable-matrix : (All (A) Index Index A -> (Mutable-Array A)))
(define (make-mutable-matrix n m a)
  ((inst array->mutable-array A) (make-matrix n m a)))

(: matrix-set! : (All (A) (Mutable-Array A) Integer Integer A -> Void))
(define (matrix-set! m i j v)
  (array-set! m (vector i j) v))

(: matrix-symmetric? : (Matrix Number) -> Boolean)
(define (matrix-symmetric? m)
  (cond [(square-matrix? m)
         (define n (square-matrix-size m))
         (for/and : Boolean ([i (in-range n)])
           (for/and : Boolean ([j (in-range i)])
             (= (matrix-ref m i j)
                (matrix-ref m j i))))]
        [else #f]))

;; ----------------------------------------

(: array-sqrt/nan : (Array Real) -> (Array Real))
(define (array-sqrt/nan a)
  (array-map sqrt/nan a))

(: array-sqrt/err : (Array Real) -> (Array Real))
(define (array-sqrt/err a)
  (array-map sqrt/err a))

(: sqrt/nan : Real -> Real)
(define (sqrt/nan x)
  (if (negative? x) +nan.0 (sqrt x)))

(: sqrt/err : Real -> Real)
(define (sqrt/err x)
  (if (negative? x)
      (error 'array-sqrt/err "got negative number: ~e" x)
      (sqrt x)))

;; ----------------------------------------

(: matrix-cholesky : (Matrix Real) -> (Matrix Real))
(define (matrix-cholesky A)
  (unless (matrix-symmetric? A)
    (error 'matrix-cholesky "expected symmetric matrix\n  given: ~e" A))
  ;; check square, symmetric
  ;; FIXME: quick check: diagonal?
  (define n (square-matrix-size A))
  (define L ((inst array->mutable-array Real) (make-matrix n n 0.0)))
  (define (real-sqrt [x : Real])
    (define r (sqrt x))
    (if (real? r)
        r
        (error 'matrix-cholesky "expected positive-definite matrix\n  given: ~e" A)))
  (for ([j (in-range n)])
    (for ([i (in-range j n)])
      (define Aij (matrix-ref A i j))
      (matrix-set!
       L i j
       (cond [(= i j)
              (real-sqrt (- Aij (for/sum : Real ([k (in-range j)])
                                  (sqr (matrix-ref L j k)))))]
             [else
              (/ (- Aij (for/sum : Real ([k (in-range j)])
                          (* (matrix-ref L i k) (matrix-ref L j k))))
                 (matrix-ref L j j))]))))
  L)

(: matrix-ldl : (Matrix Real) -> (Values (Matrix Real) (Vectorof Real)))
(define (matrix-ldl A)
  (define n (square-matrix-size A))
  (define L ((inst array->mutable-array Real) (make-matrix n n 0.0)))
  (define D ((inst make-vector Real) n 0.0))
  (for ([j (in-range n)])
    (vector-set!
     D j
     (- (matrix-ref A j j)
        (for/sum : Real ([k (in-range j)])
          (* (sqr (matrix-ref L j k)) (vector-ref D k)))))
    (for ([i (in-range (add1 j) n)])
      (matrix-set!
       L i j
       (/ (- (matrix-ref A i j)
             (for/sum : Real ([k (in-range j)])
               (* (matrix-ref L i k) (matrix-ref L j k) (vector-ref D k))))
          (vector-ref D j)))))
  (values L D))
