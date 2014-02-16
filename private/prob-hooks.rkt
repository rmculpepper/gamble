#lang racket/base
(provide current-mem
         current-ERP)

;; Basic memoization and ERP implementations

(define (base-mem f)
  (let ([memo-table (make-hash)])
    (lambda args
      (hash-ref! memo-table args (lambda () (apply f args))))))

(define (base-ERP tag sampler get-dist)
  (sampler))

;; ----

;; mem and ERP hooks; samplers/solvers override

;; mem : procedure -> procedure

(define current-mem (make-parameter base-mem))

;; ERP : (Sexpr (-> A) (U #f (-> (Discrete-Dist A))) -> A)
;; First arg is tag w/ ERP function name and params. Same tag should imply same dist.
;; Second is sampler. Third is thunk producing discrete dist or #f. Can't both be #f.

(define current-ERP (make-parameter base-ERP))
