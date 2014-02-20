;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/flonum)
(provide current-mem
         current-ERP
         make-dist
         dist?
         dist-pdf
         dist-cdf
         dist-inv-cdf
         dist-sample
         dist-sample-n
         dist-enum)

;; Basic memoization and ERP implementations

(define (base-mem f)
  (let ([memo-table (make-hash)])
    (lambda args
      (hash-ref! memo-table args (lambda () (apply f args))))))

(define (base-ERP tag dist)
  (dist-sample dist))

;; ----

;; mem and ERP hooks; samplers/solvers override

;; mem : procedure -> procedure

(define current-mem (make-parameter base-mem))

;; ERP : (Sexpr (Discrete-Dist A)) -> A)
;; First arg is tag w/ ERP function name and params. Same tag should imply same dist.
;; Second is dist.

(define current-ERP (make-parameter base-ERP))

;; ----

;; Distributions from math/distributions have performance penalty in untyped code
;; (Also, no discrete-dist? predicate.)

;; A Dist is (pdist pdf cdf inv-cdf sample enum)
;; - pdf : Real Boolean -> Flonum
;; - cdf : Real Boolean Boolean -> Flonum
;; - inv-cdf : Probability -> Flonum
;; - sample : Nat -> FlVector
;; - enum : PosInt    -- {0 ... n-1}
;;        | 'lazy     -- {0 ... }
;;        | #f        -- not enumerable
(struct pdist (pdf cdf inv-cdf sample enum))

(define-syntax (make-dist stx)
  (syntax-case stx ()
    [(make-dist name #:params (param ...) #:enum enum)
     (with-syntax ([(tmp-param ...) (generate-temporaries #'(param ...))])
       #'(let ([tmp-param (exact->inexact param)] ...)     
           (make-dist* name #:params (tmp-param ...) #:enum enum)))]
    [(make-dist name #:raw-params (param ...) #:enum enum)
     #'(make-dist* name #:params (param ...) #:enum enum)]))

(define-syntax (make-dist* stx)
  (syntax-case stx ()
    [(make-dist* name #:params (param ...) #:enum enum)
     (with-syntax ([name-pdf (format-id #'name "~a-pdf" #'name)]
                   [name-cdf (format-id #'name "~a-cdf" #'name)]
                   [name-inv-cdf (format-id #'name "~a-inv-cdf" #'name)]
                   [name-sample (format-id #'name "~a-sample" #'name)]
                   [fl-pdf (format-id #'name "fl~a-pdf" #'name)]
                   [fl-cdf (format-id #'name "fl~a-cdf" #'name)]
                   [fl-inv-cdf (format-id #'name "fl~a-inv-cdf" #'name)]
                   [fl-sample (format-id #'name "fl~a-sample" #'name)])
       #'(let ([name-pdf
                (lambda (x log?) (fl-pdf param ... (exact->inexact x) log?))]
               [name-cdf
                (lambda (x log? 1-p?) (fl-cdf param ... (exact->inexact x) log? 1-p?))]
               [name-inv-cdf
                (lambda (p log? 1-p?) (fl-inv-cdf param ... (exact->inexact p) log? 1-p?))]
               [name-sample
                (lambda (n) (fl-sample param ... n))])
           (pdist name-pdf name-cdf name-inv-cdf name-sample enum)))]))

(define (dist? x)
  (pdist? x))
(define (dist-pdf d x [log? #f])
  ((pdist-pdf d) x log?))
(define (dist-cdf d x [log? #f] [1-p? #f])
  ((pdist-cdf d) x log? 1-p?))
(define (dist-inv-cdf d x [log? #f] [1-p? #f])
  ((pdist-inv-cdf d) x log? 1-p?))
(define (dist-sample d)
  (flvector-ref ((pdist-sample d) 1) 0))
(define (dist-sample-n d n)
  ((pdist-sample d) n))
(define (dist-enum d)
  (pdist-enum d))
