;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/math
         racket/flonum
         racket/vector
         math/flonum
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "../matrix.rkt"
         "dist.rkt")
(provide (all-defined-out))

;; Contains functions (pdf, cdf, inv-cdf, sample) used to implement
;; dists in prob/dist. The naming convention is peculiar to the
;; define-dist-type macro (see prob/dist).

(define-syntax-rule (define-memoize1 (fun arg) . body)
  (begin (define memo-table (make-weak-hash))
         (define (fun arg)
           (cond [(hash-ref memo-table arg #f)
                  => values]
                 [else
                  (define r (let () . body))
                  (hash-set! memo-table arg r)
                  r]))))


;; ============================================================
;; Univariate dist functions

;; ------------------------------------------------------------
;; Bernoulli dist functions
;; -- math/dist version converts exact->inexact

(define (rawbernoulli-pdf prob v log?)
  (define p
    (cond [(= v 0) (- 1 prob)]
          [(= v 1) prob]
          [else 0]))
  (convert-p p log? #f))
(define (rawbernoulli-cdf prob v log? 1-p?)
  (define p
    (cond [(< v 0) 0]
          [(< v 1) (- 1 prob)]
          [else 1]))
  (convert-p p log? 1-p?))
(define (rawbernoulli-inv-cdf prob p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (cond [(<= p (- 1 prob)) 0]
        [else 1]))
(define (rawbernoulli-sample prob)
  (if (<= (random) prob) 1 0))


;; ------------------------------------------------------------
;; Categorical weighted dist functions
;; -- Assume weights are nonnegative, normalized.

(define (rawcategorical-pdf probs k log?)
  (unless (< k (vector-length probs))
    (error 'categorical-dist:pdf "index out of bounds\n  index: ~e\n  bounds: [0,~s]"
           k (sub1 (vector-length probs))))
  (define l (vector-ref probs k))
  (if log? (log l) l))
(define (rawcategorical-cdf probs k log? 1-p?)
  (define p (for/sum ([i (in-range (add1 k))] [prob (in-vector probs)]) prob))
  (convert-p p log? 1-p?))
(define (rawcategorical-inv-cdf probs p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (let loop ([i 0] [p p])
    (cond [(>= i (vector-length probs))
           (error 'categorical-dist:inv-cdf "out of values")]
          [(< p (vector-ref probs i))
           i]
          [else
           (loop (add1 i) (- p (vector-ref probs i)))])))
(define (rawcategorical-sample probs)
  (rawcategorical-inv-cdf probs (random) #f #f))


;; ------------------------------------------------------------
;; Discrete dist support functions
;; -- Weights are not normalized

(define (raw*discrete-pdf vs ws wsum x log?)
  (define p
    (or (for/or ([v (in-vector vs)]
                 [w (in-vector ws)])
          (and (equal? x v) (/ w wsum)))
        0))
  (convert-p p log? #f))
(define (raw*discrete-cdf vs ws wsum x log? 1-p?)
  (error 'discrete-dist:cdf "undefined"))
(define (raw*discrete-inv-cdf vs ws wsum x log? 1-p?)
  (error 'discrete-dist:inv-cdf "undefined"))
(define (raw*discrete-sample vs ws wsum)
  (define p (* (random) wsum))
  (let loop ([i 0] [p p])
    (unless (< i (vector-length ws))
      (error 'discrete-dist:sample "out of values"))
    (cond [(< p (vector-ref ws i))
           (vector-ref vs i)]
          [else
           (loop (add1 i) (- p (vector-ref ws i)))])))


;; ------------------------------------------------------------
;; Inverse gamma distribution

(define (m:flinverse-gamma-pdf shape scale x log?)
  (m:flgamma-pdf shape scale (/ x) log?))
(define (m:flinverse-gamma-cdf shape scale x log? 1-p?)
  (m:flgamma-cdf shape scale (/ x) log? 1-p?))
(define (m:flinverse-gamma-inv-cdf shape scale x log? 1-p?)
  (/ (m:flgamma-inv-cdf shape scale x log? 1-p?)))
(define (m:flinverse-gamma-sample shape scale n)
  (define flv (m:flgamma-sample shape scale n))
  (for ([i (in-range n)])
    (flvector-set! flv i (/ (flvector-ref flv i))))
  flv)


;; ------------------------------------------------------------
;; Dirichlet distribution

(define-memoize1 (multinomial-beta alpha)
  (/ (for/product ([ai (in-vector alpha)]) (m:gamma ai))
     (m:gamma (for/sum ([ai (in-vector alpha)]) ai))))

(define (rawdirichlet-pdf alpha x log?)
  (define p
    (/ (for/product ([xi (in-vector x)] [ai (in-vector alpha)]) (expt xi (sub1 ai)))
       (multinomial-beta alpha)))
  (convert-p p log? #f))
(define (rawdirichlet-cdf alpha x log? 1-p?)
  (error 'dirichlet-dist:cdf "not defined"))
(define (rawdirichlet-inv-cdf alpha x log? 1-p?)
  (error 'dirichlet-dist:inv-cdf "not defined"))
(define (rawdirichlet-sample alpha)
  ;; TODO: batch gamma sampling when all alphas same?
  (define n (vector-length alpha))
  (define x (make-vector n))
  (for ([a (in-vector alpha)] [i (in-range n)])
    (vector-set! x i (flvector-ref (m:flgamma-sample a 1.0 1) 0)))
  (define gsum (for/sum ([g (in-vector x)]) g))
  (for ([i (in-range n)])
    (vector-set! x i (/ (vector-ref x i) gsum)))
  x)


;; ------------------------------------------------------------
;; Pareto distribution

(define (m:flpareto-pdf scale shape x log?)
  (define p
    (if (>= x scale)
        (/ (* shape (expt scale shape))
           (expt x (add1 shape)))
        0.0))
  (convert-p p log? #f))
(define (m:flpareto-cdf scale shape x log? 1-p?)
  (define p
    (if (> x scale)
        (- 1.0 (expt (/ scale x) shape))
        0.0))
  (convert-p p log? 1-p?))
(define (m:flpareto-inv-cdf scale shape p log? 1-p?)
  (define p* (unconvert-p p log? 1-p?))
  (* scale (expt p* (- (/ shape)))))
(define (m:flpareto-sample scale shape n)
  (define flv (make-flvector n))
  (for ([i (in-range n)])
    (flvector-set! flv i (m:flpareto-inv-cdf scale shape (random) #f #f)))
  flv)


;; ============================================================
;; Multivariate dist functions

(define-memoize1 (memo-matrix-inverse m) (matrix-inverse m))
(define-memoize1 (memo-matrix-cholesky m) (matrix-cholesky m))
(define-memoize1 (memo-matrix-determinant m) (matrix-determinant m))

;; ------------------------------------------------------------
;; Multivariate normal dist functions

(define (rawmulti-normal-pdf mean cov x log?)
  (define k (matrix-num-rows mean))
  (unless (col-matrix? x)
    (error 'multi-normal-dist:pdf
           "expected column matrix\n  given: ~e" x))
  (unless (= (matrix-num-rows x) k)
    (error 'multi-normal-dist:pdf
           "column matrix has wrong number of rows\n  expected: ~e\n  given: ~e"
           k x))
  (define p (* (expt (* 2 pi) (- (/ k 2)))
               (expt (matrix-determinant cov) -1/2)
               (exp (* -1/2 (matrix11->value
                             (let ([x-mean (matrix- x mean)])
                               (matrix* (matrix-transpose x-mean)
                                        (memo-matrix-inverse cov)
                                        x-mean)))))))
  (convert-p p log? #f))
(define (rawmulti-normal-cdf . _)
  (error 'multi-normal-dist:cdf "not defined"))
(define (rawmulti-normal-inv-cdf . _)
  (error 'multi-normal0dist:inv-cdf "not defined"))
(define (rawmulti-normal-sample mean cov)
  (define n (matrix-num-rows mean))
  (define A (memo-matrix-cholesky cov)) ;; FIXME: cache!
  (define snv (flvector->vector (m:flnormal-sample 0.0 1.0 n)))
  (matrix+ mean (matrix* A (->col-matrix snv))))


;; ------------------------------------------------------------
;; Wishart dist functions

(define (multigamma p a)
  (* (expt pi (* p (sub1 p) 1/4))
     (for/product ([j (in-range p)])
       (m:gamma (- a (/ j 2))))))

(define (rawwishart-pdf n V X log?)
  (define p (square-matrix-size V))
  (define lp
    (+ (* 1/2 (- n p 1) (log (memo-matrix-determinant X)))
       (* -1/2 (matrix-trace (matrix* (memo-matrix-inverse V) X)))
       (* -1/2 n p (log 2))
       (* 1/2 n (log (memo-matrix-determinant V)))
       (log (multigamma p (/ n 2)))))
  (if log? lp (exp lp)))

(define (rawwishart-cdf . _)
  (error 'wishart:cdf "not defined"))
(define (rawwishart-inv-cdf . _)
  (error 'wishart:inv-cdf "not defined"))

(define (rawwishart-sample/naive n V)
  (define p (square-matrix-size V))
  (define p-zeros (->col-matrix (make-vector p 0.0)))
  (matrix-sum
   (for/list ([i (in-range n)])
     (define X (rawmulti-normal-sample p-zeros V))
     (matrix* X (matrix-transpose X)))))

;; Reference: http://www.math.wustl.edu/~sawyer/hmhandouts/Wishart.pdf
(define (rawwishart-sample n V*)
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


;; ------------------------------------------------------------
;; Inverse Wishart dist functions

(define (rawinverse-wishart-pdf n Vinv X log?)
  (rawwishart-pdf n (memo-matrix-inverse Vinv) (memo-matrix-inverse X) log?))

(define (rawinverse-wishart-cdf . _)
  (error 'inverse-wishart:cdf "not defined"))
(define (rawinverse-wishart-inv-cdf . _)
  (error 'inverse-wishart:inv-cdf "not defined"))

(define (rawinverse-wishart-sample n Vinv)
  (matrix-inverse (rawwishart-sample n (memo-matrix-inverse Vinv))))


;; ============================================================
;; Improper dist functions

(define (rawimproper-pdf ldensity v log?)
  (if log? ldensity (exp ldensity)))
(define (rawimproper-cdf ldensity v log? 1-p?)
  (error 'improper-dist:cdf "not implemented"))
(define (rawimproper-inv-cdf ldensity p log? 1-p?)
  (error 'improper-dist:inv-cdf "not implemented"))
(define (rawimproper-sample ldensity)
  (error 'improper-dist:sample "not implemented"))


;; ============================================================
;; Utils

(define (validate/normalize-weights who weights)
  (unless (and (vector? weights)
               (for/and ([w (in-vector weights)])
                 (and (rational? w) (>= w 0))))
    (raise-argument-error 'categorical-dist "(vectorof (>=/c 0))" weights))
  (define weight-sum (for/sum ([w (in-vector weights)]) w))
  (unless (> weight-sum 0)
    (error 'categorical-dist "weights sum to zero\n  weights: ~e" weights))
  (if (= weight-sum 1)
      (vector->immutable-vector weights)
      (vector-map (lambda (w) (/ w weight-sum)) weights)))

(define (unconvert-p p log? 1-p?)
  (define p* (if log? (exp p) p))
  (if 1-p? (- 1 p*) p*))

(define (convert-p p log? 1-p?)
  (define p* (if 1-p? (- 1 p) p))
  (if log? (log p*) p*))

(define (filter-modes d ms)
  (define-values (best best-p)
    (for/fold ([best null] [best-p -inf.0])
        ([m (in-list ms)])
      (define m-p (dist-pdf d m))
      (cond [(> m-p best-p)
             (values (list m) m-p)]
            [(= m-p best-p)
             (values (cons m best) best-p)]
            [else
             (values best best-p)])))
  (reverse best))

(define (vector-sum v) (for/sum ([x (in-vector v)]) x))
