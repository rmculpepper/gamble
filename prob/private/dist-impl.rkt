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
