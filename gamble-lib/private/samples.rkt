;; Copyright (c) 2016 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/vector
         racket/match
         racket/math
         "dist.rkt"
         "util/real.rkt"
         (submod "dist/util.rkt" search)
         (only-in math/statistics stddev))
(provide (all-defined-out))

;; ------------------------------------------------------------
;; Empirical CDF

;; samples->empirical-cdf : SampleFrame -> (Real -> Real)
(define (samples->empirical-cdf sf)
  (define vs (hash-ref sf 'value))
  (define lws (hash-ref sf 'log-weight #f))
  (cond [lws
         (define maxlw (for/fold ([maxlw -inf.0]) ([lw (in-vector lws)]) (max maxlw lw)))
         (define-values (svs slws) (vectors-sort vs lws))
         ;; slws is logspace weights
         (for/fold ([s 0.0] [c 0.0]) ([i (in-naturals)] [lw (in-vector slws)])
           (define-values (s* c*) (compensated+ (exp (- lw maxlw)) s c))
           (vector-set! slws i s*)
           (values s* c*))
         ;; slws is now cumulative linear weights
         (sorted->empirical-cdf svs slws (exp maxlw))]
        [else (sorted->empirical-cdf (vector-sort vs <))]))

(define (vector->empirical-cdf vs [ws #f])
  (cond [ws
         (define-values (svs sws) (vectors-sort vs ws))
         (for/fold ([s 0.0] [c 0.0]) ([i (in-naturals)] [w (in-vector sws)])
           (define-values (s* c*) (compensated+ w s c))
           (vector-set! sws i s*)
           (values s* c*))
         ;; sws is now cumulative linear weights
         (sorted->empirical-cdf svs sws)]
        [else (sorted->empirical-cdf (vector-sort vs <))]))

(define (sorted->empirical-cdf svs [scws #f] [factor 1])
  (define (ecdf x)
    (define k (binary-search/greatest-leq svs x))
    (if k (* factor (if scws (vector-ref scws k) (/ (add1 k) (vector-length svs)))) 0))
  ecdf)

;; vectors-sort : (Vectorof X) (Vectorof Y) (X X -> Boolean)
;;             -> (values (Vectorof X) (Vectorof Y))
(define (vectors-sort xs ys [x<? <])
  (define n (vector-length xs))
  (define sxs (make-vector n))
  (for ([x (in-vector xs)] [y (in-vector ys)] [i (in-naturals)])
    (vector-set! sxs i (cons x y)))
  (vector-sort! sxs < #:key car)
  (define sys (make-vector n))
  (for ([c (in-vector sxs)] [i (in-naturals)])
    (vector-set! sxs i (car c))
    (vector-set! sys i (cdr c)))
  (values sxs sys))

;; ------------------------------------------------------------
;; Kolmogorov-Smirov statistic

;; KS : (Vectorof Real) (U Dist (Vectorof Real)) -> Real
;; Correct for continuous cdf, may miss supremum for discrete dist.
(define (KS v1 v2)
  (cond [(vector? v2)
         (let ([v1 (vector-sort v1 <)]
               [v2 (vector-sort v2 <)])
           (max (KS* v1 (sorted->empirical-cdf v2))
                (KS* v2 (sorted->empirical-cdf v1))))]
        [else
         (let ([v1 (vector-sort v1 <)])
           (KS* v1 (lambda (x) (dist-cdf v2 x))))]))

(define (KS* v cdf)
  (define n (vector-length v))
  (for/fold ([m 0])
            ([x (in-vector v)]
             [i (in-naturals 1)])
    (define cdfx (cdf x))
    (max m
         (abs (- (/ i n) cdfx))
         (abs (- (/ (sub1 i) n) cdfx)))))

;; ------------------------------------------------------------
;; Kernel Density Estimation

;; kde : (Vectorof Real) (Vectorof Real) Real
;;    -> (values (-> Real Real) (U Real #f) (U Real #f))
(define (kde uvs uws h0)
  (unless (= (vector-length uvs) (vector-length uws))
    (error 'kde "weights vector has wrong length"))
  (define n (vector-length uvs))
  (define svs (make-vector n))
  (define sws (make-vector n))
  (for ([i (in-naturals)] [v (in-vector uvs)] [w (in-vector uws)])
    (vector-set! svs i (cons (fl v) (fl w))))
  (vector-sort! svs < #:key car)
  (for ([i (in-naturals)] [vw (in-vector svs)])
    (vector-set! svs i (car vw))
    (vector-set! sws i (cdr vw)))
  (define wsum (for/sum ([w (in-vector sws)]) w))
  (define h (* h0 (silverman-bandwidth svs sws wsum)))
  (define max-dist
    (for/fold ([m -inf.0]) ([w (in-vector sws)]) (max m (weight-max-dist w h))))
  (define c (/ 1.0 (* (sqrt pi) h)))
  ;; The range of non-zero KDE values
  (define x-min (- (vector-ref svs 0) max-dist))
  (define x-max (+ (vector-ref svs (sub1 n)) max-dist))
  ;; Parameters for fast-gauss
  ;; Make the KDE functions
  (define kde/windowed (make-kde/windowed svs h sws max-dist))
  (define (the-kde x)
    (cond [(< x x-min)  0.0]
          [(> x x-max)  0.0]
          [else (* c (kde/windowed (fl x)))]))
  (values the-kde x-min x-max))

;; make-kde/windowed : (Vectorof Flonum) Flonum (Vectorof Flonum) Flonum
;;                  -> (Flonum -> Flonum)
(define ((make-kde/windowed xs h ws max-dist) y)
  (cond [(vector-find-index (lambda (x) (<= (abs (- x y)) max-dist)) xs)
         => (lambda (i)
              (define j (or (vector-find-index (lambda (x) (> (abs (- x y)) max-dist)) xs i)
                            (vector-length xs)))
              (for/sum ([x (in-vector xs i j)] [w (in-vector ws i j)])
                (define z (/ (- x y) h))
                (* w (exp (- (sqr z))))))]
        [else 0.0]))

;; vector-find-index : (A -> Boolean) (Vectorof A) -> Nat/#f
(define (vector-find-index pred? xs [start 0])
  (for/or ([i (in-naturals start)] [x (in-vector xs start)])
    (and (pred? x) i)))

;; weight-max-dist : Real Real -> Real
;; Returns the maximum distance at which unnormalized kernel (with weight w and
;; width h) will contribute at least EPS to the sum.
(define (weight-max-dist w h)
  (define EPS 1e-06)
  (define a (/ w EPS))
  (if (> a 1.0) (* h (* (sqrt 2.0) (sqrt (log a)))) 0.0))

;; silverman-bandwidth : (Vectorof Real) -> Real
(define (silverman-bandwidth xs ws wsum)
  (define n (vector-length xs))
  (define-values (cw q25 q75)
    (for/fold ([cw 0] [q25 -inf.0] [q75 -inf.0])
              ([x (in-vector xs)] [w (in-vector ws)])
      (let ([cw (+ cw w)])
        (values cw
                (if (< cw (* wsum 0.25)) x q25)
                (if (< cw (* wsum 0.75)) x q75)))))
  (define iqr (- q75 q25))
  (define m (min (stddev xs) (/ iqr 1.349)))
  (/ (* 0.9 m) (expt n 1/5)))

#|
;; ISV (Improved Sheather-Jones)
;; https://arxiv.org/pdf/1011.2602

(define xi (expt (/ (- (* 6 (sqrt 2)) 3) 7) 2/5))

(define (isv-bandwidth xs ws wsum)
  (define n (vector-length xs))
  (define (gamma l z)
    __)
  (let loop ([z epsilon.0])
    (define zn (* xi (gamma l z)))
    (if (< (abs (- zn z)) epsilon.0) zn (loop zn))))
|#
