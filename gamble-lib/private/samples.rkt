;; Copyright (c) 2016 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/vector
         racket/match
         racket/math
         "base.rkt"
         "dist.rkt"
         "util/real.rkt"
         (submod "dist/util.rkt" search)
         (only-in math/statistics stddev))
(provide (all-defined-out))

;; samples-count : SampleFrame -> Nat
(define (samples-count sf)
  (vector-length (hash-ref sf 'value)))

;; samples-fmap : SampleFrame[X] (X -> Y) -> SampleFrame[Y]
(define (samples-fmap sf f)
  (let ([vv (hash-ref sf 'value)])
    (hash-set sf 'value (vector->immutable-vector (vector-map f vv)))))

;; samples-resample : SampleFrame Nat [Mode] -> SampleFrame
(define (samples-resample sf n #:mode [mode 'systematic])
  (define dd (samples->discrete-dist sf))
  (hash 'value (discrete-dist-resample dd n #:mode mode)))

;; ------------------------------------------------------------
;; Empirical CDF

;; samples->empirical-cdf : SampleFrame -> (Real -> Real)
(define (samples->empirical-cdf sf #:normalize? [normalize? #f])
  (define vs (hash-ref sf 'value))
  (define lws (hash-ref sf 'log-weight #f))
  (cond [lws
         (define maxlw (for/fold ([maxlw -inf.0]) ([lw (in-vector lws)]) (max maxlw lw)))
         (define-values (svs slws) (vectors-sort vs lws))
         ;; slws is logspace weights
         (define wsum
           (for/fold ([s 0.0] [c 0.0] #:result s)
                     ([i (in-naturals)] [lw (in-vector slws)])
             (define-values (s* c*) (compensated+ (exp (- lw maxlw)) s c))
             (vector-set! slws i s*)
             (values s* c*)))
         ;; slws is now cumulative linear weights
         (sorted->empirical-cdf svs slws (if normalize? (/ wsum) (exp maxlw)))]
        [else (sorted->empirical-cdf (vector-sort vs <) #f (/ (vector-length vs)))]))

(define (vector->empirical-cdf vs [ws #f] #:normalize? [normalize? #f])
  (cond [ws
         (define-values (svs sws) (vectors-sort vs ws))
         (define wsum
           (for/fold ([s 0.0] [c 0.0] #:result s)
                     ([i (in-naturals)] [w (in-vector sws)])
             (define-values (s* c*) (compensated+ w s c))
             (vector-set! sws i s*)
             (values s* c*)))
         ;; sws is now cumulative linear weights
         (sorted->empirical-cdf svs sws (if normalize? (/ wsum) 1))]
        [else (sorted->empirical-cdf (vector-sort vs <) #f (/ (vector-length vs)))]))

(define (sorted->empirical-cdf svs [scws #f] [factor 1])
  (define (ecdf x)
    (define k (binary-search/greatest-leq svs x))
    (if k (* factor (if scws (vector-ref scws k) (add1 k))) 0))
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

;; CDF = (Real -> Real), monotonic nondecreasing

;; Risk of false rejection.
(define DEFAULT-ALPHA 0.05)

;; samples-KS-statistic : SampleFrame (U Dist CDF SampleFrame) -> Real
(define (samples-KS-statistic sf1 ref2)
  (define ecdf1 (samples->empirical-cdf sf1 #:normalize? #t))
  (define vs1 (vector-sort (hash-ref sf1 'value) <))
  (cond [(dist? ref2)
         (define (cdf2 x) (dist-cdf ref2 x))
         (KS1 ecdf1 vs1 cdf2 (real-dist? ref2))]
        [(procedure? ref2)
         (KS1 ecdf1 vs1 ref2 #t)]
        [(hash? ref2)
         (define ecdf2 (samples->empirical-cdf ref2 #:normalize? #t))
         (define vs2 (vector-sort (hash-ref ref2 'value) <))
         (max (KS1 ecdf1 vs1 ecdf2 #f)
              (KS1 ecdf2 vs2 ecdf1 #f))]))

;; KS1 : CDF (Vectorof Real) CDF Boolean -> Real
(define (KS1 ecdf xs ref-cdf continuous?)
  (for/fold ([m 0] [prev-ex 0] #:result m) ([x (in-vector xs)])
    (define ex (ecdf x))
    (define rx (ref-cdf x))
    (values (max (if continuous? (max m (abs (- prev-ex rx))) m)
                 (abs (- ex rx)))
            ex)))

;; samples-KS-test : SampleFrame (U Dist CDF SampleFrame) -> Boolean
(define (samples-KS-test sf1 ref2 [alpha DEFAULT-ALPHA])
  (define ks (samples-KS-statistic sf1 ref2))
  (define n1 (samples-count sf1))
  (cond [(hash? ref2)
         (define n2 (samples-count ref2))
         (<= ks (KS2-threshold n1 n2 alpha))]
        [else
         (<= ks (KS1-threshold n1 alpha))]))

;; KS1-threshold : Nat Real -> Boolean
(define (KS1-threshold n [alpha DEFAULT-ALPHA])
  (define (c alpha)
    ;; 0.200 -> 1.07
    ;; 0.150 -> 1.14
    ;; 0.100 -> 1.22
    ;; 0.050 -> 1.36
    ;; 0.025 -> 1.48
    ;; 0.010 -> 1.63
    ;; 0.005 -> 1.73
    ;; 0.001 -> 1.95
    (KS1-solve-significance alpha 1.0 4.0 8))
  (/ (c alpha) (sqrt n)))

;; KS1-significance : Real -> Real
(define (KS1-significance t)
  (define ITERS 10)
  (for/sum ([k (in-range 1 (add1 ITERS))])
    (* 2.0
       (expt -1 (sub1 k))
       (exp (* -2 k k t t)))))

;; KS1-solve-significance : Real Real Real Nat -> Real
;; Find t s.t. (KS-significance t) ~= a. Via binary search.
(define (KS1-solve-significance alpha lo hi iters)
  (let loop ([lo lo] [hi hi] [los (KS1-significance lo)] [his (KS1-significance hi)] [iters iters])
    (define mid (* 0.5 (+ lo hi)))
    (if (zero? iters)
        mid
        (let ([mids (KS1-significance mid)])
          (if (< mids alpha)
              (loop lo mid los mids (sub1 iters))
              (loop mid hi mids his (sub1 iters)))))))

;; KS2-threshold : Nat Nat Real -> Boolean
(define (KS2-threshold n1 n2 [alpha DEFAULT-ALPHA])
  (define (c alpha)
    ;; 0.200 -> 1.073
    ;; 0.150 -> 1.138
    ;; 0.100 -> 1.224
    ;; 0.050 -> 1.358
    ;; 0.025 -> 1.48
    ;; 0.010 -> 1.628
    ;; 0.005 -> 1.731
    ;; 0.001 -> 1.949
    (sqrt (* -0.5 (log (* 0.5 alpha)))))
  (* (c alpha) (sqrt (/ (+ n1 n2) (* n1 n2)))))

;; ------------------------------------------------------------
;; G-test

;; samples-G-statistic : SampleFrame FiniteDist -> Real
(define (samples-G-statistic sf ref)
  ;; If sf has values outside of ref's support, infinite error.
  (define dd (samples->discrete-dist sf #:normalize? #t))
  (define N (samples-count sf))
  (-dist-G N dd ref))

;; samples-G-test : SampleFrame FiniteDist [Nat Real] -> Boolean
(define (samples-G-test sf ref [df #f] [alpha DEFAULT-ALPHA])
  (define dd (samples->discrete-dist sf #:normalize? #t))
  (define N (samples-count sf))
  (define G (-dist-G N dd ref))
  (define df* (or df (sub1 (dist-count ref))))
  (<= G (G-threshold df* alpha)))

;; G-threshold : Nat Real -> Real
(define (G-threshold df alpha)
  (PC2-threshold df alpha))

;; -dist-G : Nat DiscreteDist FiniteDist -> Real
(define (-dist-G N dd ref)
  (define-values (g p2s)
    (for/fold ([g 0.0] [p2s 0.0] [p2c 0.0] #:result (values g p2s))
              ([(v w1) (in-dist dd)])
      (define p2 (fl (dist-pdf ref v)))
      (define-values (p2s* p2c*) (compensated+ p2 p2s p2c))
      (values (+ g (* w1 (- (log w1) (log p2))))
              p2s* p2c*)))
  (* 2.0 N (+ g (- (dist-total-measure ref) p2s))))

;; ------------------------------------------------------------
;; Pearson's chi-squared test

;; samples-PC2 : SampleFrame FiniteDist -> Real
(define (samples-PC2 sf ref)
  ;; If sf has values outside of ref's support, infinite error.
  (define dd (samples->discrete-dist sf #:normalize? #t))
  (define N (samples-count sf))
  (-dist-PC2 N dd ref))

;; samples-PC2-test : SampleFrame FiniteDist [Nat Real] -> Boolean
;; df = "degrees of freedom" = (ncategories - 1 - nparameters-estimated-from-data)
(define (samples-PC2-test sf ref [df #f] [alpha DEFAULT-ALPHA])
  (define dd (samples->discrete-dist sf #:normalize? #t))
  (define N (samples-count sf))
  (define chi2 (-dist-PC2 N dd ref))
  (define df* (or df (sub1 (dist-count ref))))
  (<= chi2 (PC2-threshold df* alpha)))

;; PC2-threshold : Nat Real -> Real
(define (PC2-threshold df alpha)
  (dist-inv-cdf (chi2-dist df) alpha #f #t))

;; -dist-PC2 : Nat DiscreteDist FiniteDist -> Real
(define (-dist-PC2 N dd ref)
  (define-values (chi2 p2s)
    (for/fold ([chi2 0.0] [p2s 0.0] [p2c 0.0] #:result (values chi2 p2s))
              ([(v w1) (in-dist dd)])
      (define p2 (fl (dist-pdf ref v)))
      (define-values (p2s* p2c*) (compensated+ p2 p2s p2c))
      (eprintf "~s, ~s, ~s; ~s\n" w1 p2 (/ (sqr (- w1 p2)) p2) p2s*)
      (values (+ chi2 (/ (sqr (- w1 p2)) p2))
              p2s* p2c*)))
  (* N (+ chi2 (- (dist-total-measure ref) p2s))))

(define (chi2-dist df) (gamma-dist (/ df 2.0) 2.0))


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
