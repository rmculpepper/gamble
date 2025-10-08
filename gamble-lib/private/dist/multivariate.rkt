;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/contract/base
         racket/match
         racket/math
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "base.rkt"
         (submod "util.rkt" define)
         (submod "util.rkt" math)
         (submod "util.rkt" weights))
(provide (all-defined-out))

(define-dist-struct multinomial-dist
  ([n exact-nonnegative-integer?]
   [ws vector? -multinomial-guard-weights])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (multinomial-dist n ws) self)
     (-multinomial-sample n ws))
   (define (-pdf self x log?)
     (match-define (multinomial-dist n ws) self)
     (-multinomial-pdf n ws x log?))
   ;; (define (-measure ms) #f)
   (define (-total-measure self) 1)])

(define (-multinomial-guard-weights in-ws)
  (normalize-inexact-weights 'multinomial-dist in-ws))

(define -multinomial:ws=>cndws (make-weak-hasheq))

(define (-multinomial-cndws ws)
  (hash-ref! -multinomial:ws=>cndws ws (lambda () (-multinomial-cndws* ws))))
(define (-multinomial-cndws* ws)
  ;; Not cumulative weights; cndws[k] = Pr[k | not 1..(k-1)] = Pr[k | k..]
  (define cndws (make-vector (vector-length ws)))
  (for/fold ([rem 1.0]) ([w (in-vector ws)] [i (in-naturals)])
    (vector-set! cndws i (min (/ w rem) 1.0))
    (max 0.0 (- rem w)))
  (vector->immutable-vector cndws))

(define (-multinomial-sample n ws)
  (define cndws (-multinomial-cndws ws))
  (define v (make-vector (vector-length ws)))
  (define leftover
    (for/fold ([n n]) ([w (in-vector cndws)] [i (in-naturals)])
      (define k (exact (flvector-ref (m:flbinomial-sample (fl n) w 1) 0)))
      (vector-set! v i k)
      (- n k)))
  (unless (zero? leftover) (error 'dist-sample:multinomial-dist "internal error"))
  (vector->immutable-vector v))

(define (-multinomial-pdf n ws v log?)
  (cond [(and (vector? v) (= (vector-length v) (vector-length ws)))
         (define cndws (-multinomial-cndws ws))
         (define-values (leftover ll)
           (for/fold ([n n] [ll 0.0]) ([ve (in-vector v)] [w (in-vector cndws)])
             (define l (m:flbinomial-pdf (fl n) w (fl ve) #t))
             (values (- n ve) (+ ll l))))
         (cond [(not (zero? leftover)) (if log? -inf.0 0)]
               [else (if log? ll (exp ll))])]
        [else (if log? -inf.0 0)]))


;; ----------------------------------------

(define-dist-struct dirichlet-dist
  ([alpha vector? -dirichlet-guard])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (dirichlet-dist alpha) self)
     (-dirichlet-sample alpha))
   (define (-pdf self x log?)
     (match-define (dirichlet-dist alpha) self)
     (-dirichlet-pdf alpha x log?))]
  #:methods gen:conjugate-dist
  [(define (-conjugate self xdistp xs)
     (match-define (dirichlet-dist alpha) self)
     (match xdistp
       [`(categorical-dist _)
        (define n (vector-length alpha))
        (define new-alpha (vector-copy alpha))
        (for ([x (in-vector xs)])
          (let ([index (exact x)]) ;; categorical is {0,...,n-1}
            (vector-set! new-alpha index (+ 1.0 (vector-ref new-alpha index)))))
        (dirichlet-dist (vector->immutable-vector new-alpha))]
       [_ #f]))]
  #|
  ;; #:support ;; [0,1]^n, components sum to 1
  #:mean (let ([alphasum (vector-sum alpha)])
           (for/vector ([ai (in-vector alpha)]) (/ ai alphasum)))
  #:modes (if (for/and ([ai (in-vector alpha)]) (> ai 1))
              (let ([denom (for/sum ([ai (in-vector alpha)]) (sub1 ai))])
                (list (for/vector ([ai (in-vector alpha)]) (/ (sub1 ai) denom))))
              null)
  #:variance (let* ([a0 (vector-sum alpha)]
                    [denom (* a0 a0 (add1 a0))])
               (for/vector ([ai (in-vector alpha)])
                 (/ (* ai (- a0 ai)) denom)))
  |#)

(define -dirichlet-intern-table (make-weak-hash))

(define (-dirichlet-guard in-ws)
  (cond [(hash-ref-key -dirichlet-intern-table in-ws #f)
         => values]
        [else
         (define ws (make-vector (vector-length in-ws)))
         (for ([w (in-vector in-ws)] [i (in-naturals)])
           (unless (and (rational? w) (> w 0))
             (raise-argument-error 'dirichlet-dist "(vectorof (>/c 0))" in-ws))
           (vector-set! ws i (fl w)))
         (define wsi (vector->immutable-vector ws))
         (hash-set! -dirichlet-intern-table wsi #t)
         wsi]))

(define (-dirichlet-sample alpha)
  ;; TODO: batch gamma sampling when all alphas same?
  (define n (vector-length alpha))
  (define x (make-vector n))
  (for ([a (in-vector alpha)] [i (in-range n)])
    (vector-set! x i (flvector-ref (m:flgamma-sample a 1.0 1) 0)))
  (define gsum (for/sum ([g (in-vector x)]) g))
  (for ([i (in-range n)])
    (vector-set! x i (/ (vector-ref x i) gsum)))
  (vector->immutable-vector x))

(define (-dirichlet-pdf alpha x log?)
  (cond [(and (vector? x) (= (vector-length x) (vector-length alpha)))
         (define lp
           (- (for/sum ([xi (in-vector x)] [ai (in-vector alpha)])
                (* (sub1 ai) (log xi)))
              (log-multinomial-beta alpha)))
         (if log? lp (exp lp))]
        [else (if log? -inf.0 0)]))

(define log-multinomial-beta
  (let ([memo-table (make-weak-hasheq)])
    (lambda (alpha)
      (hash-ref! memo-table alpha
                 (lambda ()
                   (- (for/sum ([ai (in-vector alpha)]) (m:log-gamma ai))
                      (m:log-gamma (for/sum ([ai (in-vector alpha)]) ai))))))))
