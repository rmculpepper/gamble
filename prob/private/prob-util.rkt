#lang racket/base
(require math/distributions
         "prob-hooks.rkt")
(provide rejection-sample
         mem
         ERP
         flip
         d2
         randn)

;; Rejection sampling

(define (rejection-sample thunk pred [project values])
  (let ([v (thunk)])
    (if (pred v)
        (project v)
        (rejection-sample thunk pred project))))

;; mem and ERP wrappers

(define (mem f)
  (unless (procedure? f)
    (raise-argument-error 'mem "procedure?" f))
  ((current-mem) f))

(define (ERP tag sampler get-dist)
  (let ([sampler (or sampler (lambda () (sample (get-dist))))])
    ((current-ERP) tag sampler get-dist)))

;; ----

;; flip : Prob -> (U #t #f)
(define (flip [prob 1/2])
  (ERP `(flip ,prob)
       (and (= prob 1/2) (lambda () (zero? (random 2))))
       (lambda () (discrete-dist '(#t #f) (list prob (- 1 prob))))))

;; d2 : Prob -> (U 1 0)
(define (d2 [prob 1/2])
  (if (flip prob) 1 0))

;; randn : Nat -> Nat
(define (randn n)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'randn "exact-positive-integer?" n))
  (ERP `(randn ,n)
       (lambda () (random n))
       (lambda () (discrete-dist (for/list ([i n]) i)))))
