#lang racket/base
(require plot
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
