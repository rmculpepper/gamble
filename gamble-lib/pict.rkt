;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/contract
         racket/match
         racket/vector
         (rename-in plot/pict [density plot-density])
         "private/dist.rkt"
         "private/samples.rkt")
(provide (contract-out
          [dist->pict
           (-> dist? any)]
          [samples->pict
           (-> hash? any)])) ;; FIXME

(define ITEM-HEIGHT 50)

(define (dist->pict dist)
  (cond [(and (numeric-dist? dist)
              (not (bernoulli-dist? dist)))
         (define-values (xmin0 xmax0)
           (match (dist-support dist)
             [(integer-range lo hi) (values lo hi)]
             [(real-range lo hi) (values lo hi)]
             [_ (values -inf.0 +inf.0)]))
         (define xmin (if (rational? xmin0) xmin0 (floor (sub1 (dist-inv-cdf dist 0.01)))))
         (define xmax (if (rational? xmax0) xmax0 (ceiling (add1 (dist-inv-cdf dist 0.99)))))
         (define (pdf x) (dist-pdf dist x))
         (define (cdf x) (dist-cdf dist x))
         (define pdfp (function-interval pdf (lambda (x) 0)))
         (define pts
           (cond [(integer-dist? dist)
                  (points #:color "blue" ;;  #:size 10 #:sym 'fullcircle
                          (for/list ([x (in-range xmin (add1 xmax))])
                            (list x (dist-pdf dist x))))]
                 [else null]))
         (do-pict xmin xmax cdf (list pdfp pts))]
        [(and (discrete-dist? dist) (> (dist-count dist) 2)
              (for/and ([v (in-dist-values dist)]) (real? v)))
         (define vs (discrete-dist-values dist))
         (define ws (discrete-dist-weights dist))
         (real-samples->pict vs ws #f #f)]
        [(finite-dist? dist)
         (define vws (for/list ([(v w) (in-dist dist)]) (list v w)))
         (vws->pict vws)]
        [else (error 'dist->pict "unsupported")]))

(define (samples->pict sf)
  (define vs (hash-ref sf 'value))
  (define lws (hash-ref sf 'log-weight #f))
  (cond [(and (> (vector-length vs) 2)
              (for/and ([v (in-vector vs)]) (real? v)))
         (real-samples->pict vs lws #t #t)]
        [else
         (if lws
             (vws->pict (for/list ([v (in-vector vs)] [lw (in-vector lws)]) (cons v (exp lw))))
             (vws->pict (for/list ([v (in-vector vs)]) (cons v 1))))]))

(define (real-samples->pict vs ws log-weight? normalize?)
  (define-values (xmin xmax)
    (for/fold ([xmin +inf.0] [xmax -inf.0] #:result (round-minmax xmin xmax))
              ([v (in-vector vs)])
      (values (min xmin v) (max xmax v))))
  (define kde (vector-kde vs ws log-weight? normalize?))
  (define pdfp
    (let ([kde1 (lambda (x) (kde x 0.5))]
          [kde2 (lambda (x) (kde x 1.0))]
          [kde3 (lambda (x) (kde x 2.0))])
      (list (function kde1 #:color "blue" #:alpha 0.25)
            (function kde2 #:color "blue" #:alpha 0.50)
            (function kde3 #:color "blue" #:alpha 0.25))))
  (define cdf (vector->empirical-cdf vs ws log-weight? normalize?))
  (define pts
    (points #:color "blue"
            (for/list ([v (in-vector vs)] [w (in-vector ws)]) (list v w))))
  (do-pict xmin xmax cdf (list pdfp pts)))

(define (do-pict xmin xmax cdf parts)
  (plot-pict
   #:x-min xmin #:x-max xmax #:y-min 0 #:x-label #f #:y-label #f
   (list parts
         (cond [cdf (list (hrule 1.0 #:style 'dot #:color "lightgray")
                          (function cdf xmin xmax #:color "darkred"))]
               [else null]))))

(define (vws->pict vws)
  (define wsum (for/sum ([vw (in-list vws)]) (cadr vw)))
  (define maxw (if (zero? wsum) 1.0 wsum))
  (plot-pict
   #:height (* ITEM-HEIGHT (length vws))
   #:x-max maxw #:y-min 0 #:x-label #f #:y-label #f
   (discrete-histogram vws #:invert? #t)))

(define (round-minmax xmin xmax)
  (define xdiff (- xmax xmin))
  (cond [(>= xdiff 3)
         (values (floor xmin) (ceiling xmax))]
        [(>= xdiff 0.5)
         (values (* 0.1 (floor (* xmin 10)))
                 (* 0.1 (ceiling (* xmax 10))))]
        [else (values xmin xmax)]))
