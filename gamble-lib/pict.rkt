;; Copyright 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         (rename-in plot/pict [density plot-density])
         "private/dist.rkt"
         "private/samples.rkt")
(provide dist->pict)

(define ITEM-HEIGHT 50)

(define (dist->pict dist)
  (cond [(real-dist? dist)
         (define-values (xmin xmax)
           (match (dist-support dist)
             [(integer-range lo hi) (values lo hi)]
             [(real-range lo hi) (values (floor lo) (ceiling hi))]
             [_ (values (floor (sub1 (dist-inv-cdf dist 0.01)))
                        (ceiling (add1 (dist-inv-cdf dist 0.99))))]))
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
         (define-values (xmin xmax)
           (for/fold ([xmin +inf.0]
                      [xmax -inf.0]
                      #:result (round-minmax xmin xmax))
                     ([v (in-dist-values dist)])
             (values (min xmin v) (max xmax v))))
         (define pdfp
           (let-values ([(kde1 _xmin1 _xmax1) (kde vs ws 0.5)]
                        [(kde2 _xmin2 _xmax2) (kde vs ws 1.0)]
                        [(kde3 _xmin3 _xmax3) (kde vs ws 2.0)])
             (list (function kde1 #:color "blue" #:alpha 0.25)
                   (function kde2 #:color "blue" #:alpha 0.50)
                   (function kde3 #:color "blue" #:alpha 0.25)))
           #;
           (list
            (plot-density vs 0.5 ws #:color "blue" #:alpha 0.25)
            (plot-density vs 1.0 ws #:color "blue" #:alpha 0.50)
            (plot-density vs 2.0 ws #:color "blue" #:alpha 0.25)))
         (define cdf (vector->empirical-cdf vs ws))
         (define pts
           (points #:color "blue"
                   (for/list ([v (in-vector vs)] [w (in-vector ws)]) (list v w))))
         (do-pict xmin xmax cdf (list pdfp pts))]
        [(finite-dist? dist)
         (finite-dist->pict dist)]
        [else (error 'dist->pict "unsupported")]))

(define (do-pict xmin xmax cdf parts)
  (plot-pict
   #:x-min xmin #:x-max xmax #:y-min 0 #:x-label #f #:y-label #f
   (list parts
         (cond [cdf (list (hrule 1.0 #:style 'dot #:color "lightgray")
                          (function cdf xmin xmax #:color "darkred"))]
               [else null]))))

(define (finite-dist->pict dist)
  (define vws (for/list ([(v w) (in-dist dist)]) (list v w)))
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
