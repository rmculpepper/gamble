#lang racket/base
(require math/distributions)
(provide current-mem
         current-ERP)

;; Basic memoization and ERP implementations

(define (base-mem f)
  (let ([memo-table (make-hash)])
    (lambda args
      (hash-ref! memo-table args (lambda () (apply f args))))))

(define (base-ERP tag dist)
  (sample dist))

;; ----

;; mem and ERP hooks; samplers/solvers override

;; mem : procedure -> procedure

(define current-mem (make-parameter base-mem))

;; ERP : (Sexpr (Discrete-Dist A)) -> A)
;; First arg is tag w/ ERP function name and params. Same tag should imply same dist.
;; Second is dist.

(define current-ERP (make-parameter base-ERP))
