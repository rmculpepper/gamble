;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base racket/syntax)
         math/distributions
         racket/flonum)
(provide current-mem
         current-ERP
         dist?
         dist-pdf
         dist-cdf
         dist-inv-cdf
         dist-sample
         dist-sample-n
         dist-enum
         make-bernoulli-dist
         make-binomial-dist
         make-geometric-dist
         make-poisson-dist 
         make-beta-dist 
         make-cauchy-dist
         make-exponential-dist
         make-gamma-dist
         make-logistic-dist
         make-normal-dist
         make-uniform-dist
         make-discrete-dist)

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

;; ----

;; make-dist is a macro for the following reasons:
;; - conciseness - takes advantage of regular naming patterns
;; - error-reporting - wrapped functions get useful names

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

(define (make-bernoulli-dist prob)
  (make-dist bernoulli #:params (prob) #:enum 2))
(define (make-binomial-dist n p)
  (make-dist binomial #:params (n p) #:enum (add1 n)))
(define (make-geometric-dist p)
  (make-dist geometric #:params (p) #:enum 'lazy))
(define (make-poisson-dist mean)
  (make-dist poisson #:params (mean) #:enum 'lazy))
(define (make-beta-dist a b)
  (make-dist beta #:params (a b) #:enum #f))
(define (make-cauchy-dist mode scale)
  (make-dist cauchy #:params (mode scale) #:enum #f))
(define (make-exponential-dist mean)
  (make-dist exponential #:params (mean) #:enum #f))
(define (make-gamma-dist shape scale)
  (make-dist gamma #:params (shape scale) #:enum #f))
(define (make-logistic-dist mean scale)
  (make-dist logistic #:params (mean scale) #:enum #f))
(define (make-normal-dist mean stddev)
  (make-dist normal #:params (mean stddev) #:enum #f))
(define (make-uniform-dist min max)
  (make-dist uniform #:params (min max) #:enum #f))


;; Discrete weighted dist functions
(define (fldiscrete-pdf probs prob-sum k log?)
  (/ (list-ref probs (inexact->exact k)) prob-sum))
(define (fldiscrete-cdf probs prob-sum k log? 1-p?)
  (when (or log? 1-p?) (error 'fldiscrete-cdf "unimplemented"))
  (let ([k (inexact->exact k)])
    (/ (for/sum ([i (in-range (add1 k))] [prob (in-list probs)]) prob)
       prob-sum)))
(define (fldiscrete-inv-cdf probs prob-sum p log? 1-p?)
  (when (or log? 1-p?) (error 'fldiscrete-inv-cdf "unimplemented"))
  (let loop ([probs probs] [p (* p prob-sum)] [i 0])
    (cond [(null? probs)
           (error 'fldiscrete-inv-cdf "out of values")]
          [(< p (car probs))
           i]
          [else
           (loop (cdr probs) (- p (car probs)) (add1 i))])))
(define (fldiscrete-sample probs prob-sum n)
  (define v (make-flvector n))
  (for ([i (in-range n)])
    (flvector-set! v i (fldiscrete-inv-cdf probs prob-sum (random) #f #f)))
  v)

(define (make-discrete-dist probs)
  (let ([n (length probs)]
        [prob-sum (apply + probs)])
    (make-dist discrete #:raw-params (probs prob-sum) #:enum n)))
