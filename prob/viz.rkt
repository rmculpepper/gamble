;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (rename-in plot)
         data/order)
(provide hist
         hist-pict)

(define (hist xs #:invert? [invert? #f])
  (hist* xs invert? plot))
(define (hist-pict xs #:invert? [invert? #f])
  (hist* xs invert? plot-pict))

(define (hist* xs invert? plot)
  (define table (make-hash))
  (for ([x xs])
    (hash-set! table x (add1 (hash-ref table x 0))))
  (define entries (hash-map table vector))
  (define sorted-entries (sort entries (order-<? datum-order)))
  (plot (discrete-histogram sorted-entries #:invert? invert?)))
