;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract/base
         racket/match
         racket/math
         racket/flonum
         racket/vector
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "base.rkt"
         "define.rkt"
         (submod "util.rkt" math)
         (submod "util.rkt" density)
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
   (define (-density self x log?)
     (match-define (multinomial-dist n ws) self)
     (density (-multinomial-pdf n ws x log?) 0))
   ;; (define (-measure ms) #f)
   (define (-total-measure self) 1)]
  #|
  ;; FIXME: drift by computing new multinomial-dist ??
  ;;  eg, scale-weighted average of prior weights and derived from current value?
  #:drift1 (lambda (value scale-factor)
             (multinomial-drift n weights value scale-factor))
  |#)

(define (-multinomial-guard-weights in-ws)
  (normalize-weights 'multinomial-dist in-ws #t))

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
      (define k (exact (flvector-ref (m:flbinomial-sample (inexact n) w 1) 0)))
      (vector-set! v i k)
      (- n k)))
  (unless (zero? leftover) (error 'dist-sample:multinomial-dist "internal error"))
  (vector->immutable-vector v))

(define (-multinomial-pdf n ws v log?)
  (cond [(and (vector? v) (= (vector-length v) (vector-length ws)))
         (define cndws (-multinomial-cndws ws))
         (define-values (leftover ll)
           (for/fold ([n n] [ll 0.0]) ([ve (in-vector v)] [w (in-vector cndws)])
             (define l (m:flbinomial-pdf (inexact n) w (inexact ve) #t))
             (values (- n ve) (+ ll l))))
         (cond [(not (zero? leftover)) (if log? -inf.0 0)]
               [else (if log? ll (exp ll))])]
        [else (if log? -inf.0 0)]))

#;
;; Do some number of moves based on scale-factor. Symmetric.
(define (multinomial-drift n _probs old scale-factor)
  (cond [(or (zero? n) (<= (vector-length old) 1))
         (cons old 0)]
        [else
         (define v (vector-copy old))
         (define (pick-nonempty-index)
           (define i (random (vector-length v)))
           (if (zero? (vector-ref v i))
               (pick-nonempty-index)
               i))
         (define (pick-other-index i)
           (define j (random (sub1 (vector-length v))))
           (if (>= j i) (add1 j) j))
         (for ([_a (in-range (inexact->exact (ceiling (* n scale-factor))))])
           (define i (pick-nonempty-index))
           (define j (pick-other-index i))
           (vector-set! v i (sub1 (vector-ref v i)))
           (vector-set! v j (add1 (vector-ref v j))))
         (cons v 0)]))


;; ----------------------------------------

(define-dist-struct dirichlet-dist
  ([alpha vector? -dirichlet-guard])
  #:methods gen:dist
  [(define (-sample self)
     (match-define (dirichlet-dist alpha) self)
     (-dirichlet-sample alpha))
   (define (-pdf self x log?)
     (match-define (dirichlet-dist alpha) self)
     (-dirichlet-pdf alpha x log?))
   (define (-density self x log?)
     (match-define (dirichlet-dist alpha) self)
     (density (-dirichlet-pdf alpha x log?) (vector-length alpha) log?))]
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
  #:conjugate (lambda (data-d data)
                (match data-d
                  [`(categorical-dist _)
                   (define n (vector-length alpha))
                   (define countv (make-vector n 0))
                   (for ([x (in-vector data)] [i (in-range n)])
                     (vector-set! countv i (add1 (vector-ref countv i))))
                   (dirichlet-dist (vector-map + alpha countv))]
                  [_ #f]
  ;; DRIFT: (1 - eps) * value + eps * Dir(alpha)
  ;; ie, weighted avg of current value and new Dirichlet draw
  ;; Q: for alpha, should use either same parameters, OR could use uniform (1 ...)???
  ;; ** OR **: take current value, multiply by f(scale-factor),
  ;; use that as Dirichlet param, draw
  ;; NOTE: not symmetric!
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
           (vector-set! ws i (inexact w)))
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
