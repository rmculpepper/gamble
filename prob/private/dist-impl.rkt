;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/vector
         math/flonum
         (prefix-in m: math/distributions)
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

;; http://hips.seas.harvard.edu/blog/2013/01/09/computing-log-sum-exp/
;; http://machineintelligence.tumblr.com/post/4998477107/the-log-sum-exp-trick
(define (logspace+ x y)
  (let ([M (max x y)])
    (+ M (log (+ (exp (- x M)) (exp (- y M)))))))
(define (logspace-sum xs)
  (let ([M (apply max xs)])
    (+ M (log (for/sum ([x (in-list xs)]) (exp (- x M)))))))
