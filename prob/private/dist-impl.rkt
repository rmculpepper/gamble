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

(define (bernoulli-pdf prob v log?)
  (define p
    (cond [(= v 0) (- 1 prob)]
          [(= v 1) prob]
          [else 0]))
  (convert-p p log? #f))
(define (bernoulli-cdf prob v log? 1-p?)
  (define p
    (cond [(< v 0) 0]
          [(< v 1) (- 1 prob)]
          [else 1]))
  (convert-p p log? 1-p?))
(define (bernoulli-inv-cdf prob p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (cond [(<= p (- 1 prob)) 0]
        [else 1]))
(define (bernoulli-sample prob)
  (if (<= (random) prob) 1 0))


;; ------------------------------------------------------------
;; Categorical weighted dist functions
;; -- Assume weights are nonnegative, normalized.

(define (categorical-pdf probs k log?)
  (unless (< k (vector-length probs))
    (error 'categorical-dist:pdf "index out of bounds\n  index: ~e\n  bounds: [0,~s]"
           k (sub1 (vector-length probs))))
  (define l (vector-ref probs k))
  (if log? (log l) l))
(define (categorical-cdf probs k log? 1-p?)
  (define p (for/sum ([i (in-range (add1 k))] [prob (in-vector probs)]) prob))
  (convert-p p log? 1-p?))
(define (categorical-inv-cdf probs p0 log? 1-p?)
  (define p (unconvert-p p0 log? 1-p?))
  (let loop ([i 0] [p p])
    (cond [(>= i (vector-length probs))
           (error 'categorical-dist:inv-cdf "out of values")]
          [(< p (vector-ref probs i))
           i]
          [else
           (loop (add1 i) (- p (vector-ref probs i)))])))
(define (categorical-sample probs)
  (categorical-inv-cdf probs (random) #f #f))


;; ------------------------------------------------------------
;; Discrete dist support functions
;; -- Weights are not normalized

(define (discrete-pdf vs ws wsum x log?)
  (define p
    (or (for/or ([v (in-vector vs)]
                 [w (in-vector ws)])
          (and (equal? x v) (/ w wsum)))
        0))
  (convert-p p log? #f))
(define (discrete-sample vs ws wsum)
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

(define (dirichlet-pdf alpha x log?)
  (define p
    (/ (for/product ([xi (in-vector x)] [ai (in-vector alpha)]) (expt xi (sub1 ai)))
       (multinomial-beta alpha)))
  (convert-p p log? #f))
(define (dirichlet-sample alpha)
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
;; Improper dist functions

(define (improper-pdf ldensity v log?)
  (if log? ldensity (exp ldensity)))
(define (improper-sample ldensity)
  (error 'improper-dist:sample "not implemented"))


;; ============================================================
;; Drift Kernel Utils

(define (sample-normal mean stddev)
  (let ([mean (exact->inexact mean)]
        [stddev (exact->inexact stddev)])
    (flvector-ref (m:flnormal-sample mean stddev 1) 0)))

(define (drift:add-normal value scale)
  (cons (sample-normal value scale) 0))
(define (drift:mult-exp-normal value scale)
  (cons (* value (exp (sample-normal 0 scale))) 0))
(define (drift:asymmetric f value)
  (define forward-dist (f value))
  (define value* (dist-sample forward-dist))
  (define backward-dist (f value*))
  (cons value*
        (- (dist-pdf backward-dist value #t)
           (dist-pdf forward-dist value* #t))))


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

(define (filter-modes f ms)
  (define-values (best best-p)
    (for/fold ([best null] [best-p -inf.0])
        ([m (in-list ms)])
      (define m-p (f m))
      (cond [(> m-p best-p)
             (values (list m) m-p)]
            [(= m-p best-p)
             (values (cons m best) best-p)]
            [else
             (values best best-p)])))
  (reverse best))

(define (vector-sum v) (for/sum ([x (in-vector v)]) x))
