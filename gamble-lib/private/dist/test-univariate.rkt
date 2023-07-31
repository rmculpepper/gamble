#lang racket/base
(require rackunit
         "base.rkt"
         "univariate.rkt"
         "measurable.rkt")


(define N #e1e4)

(define %ls '(0.25 0.4 0.5 0.6 0.75))

(define (check-dist d)
  (define samples (list->vector (for/list ([i N]) (dist-sample d))))
  (define sd (samples-dist samples))

  (for ([%l %ls])
    (define dv (dist-inv-cdf d %l))
    (define sv (dist-inv-cdf sd %l))
    (eprintf " ~s vs ~s\n" dv sv)
    (abs (- dv sv)))
  (void))

(define d (normal-dist 0 1))
(check-dist d)
