#lang racket/base
(require (rename-in (except-in plot plot plot3d)
                    [plot-pict plot]
                    [plot3d-pict plot3d])
         data/order)
(provide hist)

(define (hist xs
              #:invert? [invert? #f])
  (define table (make-hash))
  (for ([x xs])
    (hash-set! table x (add1 (hash-ref table x 0))))
  (define entries (hash-map table vector))
  (define sorted-entries (sort entries (order-<? datum-order)))
  (plot (discrete-histogram sorted-entries #:invert? invert?)))
