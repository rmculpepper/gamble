#lang racket/base
(require racket/contract/base)

;; ============================================================
(require "private/dist/base.rkt")

(provide dist/sample?
         dist-sample
         dist?
         dists-same-type?
         dist-density
         dist-pdf
         dist-cdf
         dist-inv-cdf
         dist-total-mass
         dist-energy
         dist-Denergy
         dist-support
         dist-enum
         dist-conjugate
         (struct-out integer-range)
         (struct-out real-range)
         integer-dist?
         real-dist?
         finite-dist?
         dist-mean
         dist-median
         dist-variance
         dist-modes)

;; ============================================================
(require "private/dist/transformer.rkt")
(provide (all-from-out "private/dist/transformer.rkt"))

;; ============================================================
(require "private/dist/univariate.rkt")
(provide (all-from-out "private/dist/univariate.rkt"))

;; ============================================================
(require "private/dist/discrete.rkt")
(provide (rename-out [discrete-dist:m discrete-dist]
                     [make-discrete-dist:m make-discrete-dist])
         discrete-dist?
         discrete-dist-vs
         discrete-dist-ws
         discrete-dist-of
         in-dist
         (contract-out
          [alist->discrete-dist
           (->* [list?] [#:normalize? any/c] any)]
          [normalize-discrete-dist
           (-> discrete-dist? any)]
          [discrete-dist->inexact
           (-> discrete-dist? any)]))