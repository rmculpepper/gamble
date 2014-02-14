#lang racket/base
(require math/distributions)
(provide current-mem
         mem
         current-ERP
         ERP
         flip
         randn)

;; mem : procedure -> procedure

(define (no-mem f)
  (error 'mem "no mem implementation"))

(define current-mem (make-parameter no-mem))

(define (mem f)
  (unless (procedure? f)
    (raise-argument-error 'mem "procedure?" f))
  ((current-mem) f))

;; ----

;; ERP : (Sexpr (-> A) (U #f (-> (Discrete-Dist A))) -> A)
;; First arg is tag w/ ERP function name and params. Same tag should imply same dist.
;; Second is sampler. Third is thunk producing discrete dist or #f. Can't both be #f.

(define (no-ERP tag sampler get-dist)
  (error 'ERP "no ERP implementation"))

(define current-ERP (make-parameter no-ERP))

(define (ERP tag sampler get-dist)
  (let ([sampler (or sampler (lambda () (sample (get-dist))))])
    ((current-ERP) tag sampler get-dist)))

;; ----

;; flip : -> (U 0 1)
(define (flip [prob 1/2])
  (ERP `(flip ,prob)
       (lambda () (random 2))
       (lambda () (discrete-dist '(0 1) (list prob (- 1 prob))))))

;; randn : Nat -> Nat
(define (randn n)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'randn "exact-positive-integer?" n))
  (ERP `(randn ,n)
       (lambda () (random n))
       (lambda () (discrete-dist (for/list ([i n]) i)))))
