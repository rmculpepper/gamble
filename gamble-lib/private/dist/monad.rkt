;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require "base.rkt"
         "discrete.rkt"
         "univariate.rkt")
(provide (all-defined-out))

#;
(provide
 (contract-out
  [dist-unit (-> any/c any)]
  [dist-fmap (-> finite-dist? (-> any/c any/c) any)]
  [dist-bind (-> finite-dist? (-> any/c finite-dist?) any)]
  [dist-bindx (-> finite-dist? (-> any/c finite-dist?) any)]
  [dist-filter (-> finite-dist? (-> any/c boolean?) any)]
  [dist-join (-> finite-dist? finite-dist?)]))

;; dirac : X -> FDist[X]
(define (dirac v [w 1]) (discrete-dist (hash v w) w))
(define (dist-unit v) (dirac v))

;; dist-fmap : FDist[X] (X -> Y) -> FDist[Y]
(define (dist-fmap d f)
  (for/discrete-dist ([(v w) (in-dist d)])
    (values (f v) w)))

;; dist-bind : FDist[X] (X -> FDist[Y]) -> FDist[Y]
(define (dist-bind d f)
  (for*/discrete-dist ([(v w) (in-dist d)]
                       [(vv ww) (in-dist (f v))])
    (values vv (* w ww))))

;; dist-bindx : FDist[X] (X -> FDist[Y]) -> FDist[(list X Y)]
(define (dist-bindx d f)
  (for*/discrete-dist ([(v w) (in-dist d)]
                       [(vv ww) (in-dist (f v))])
    (values (list v vv) (* w ww))))

;; dist-score : FDist[X] (X -> NNReal) -> FDist[X]
(define (dist-score d score)
  #;(dist-bind d (lambda (v) (dirac v (score v))))
  (for/discrete-dist ([(v w) (in-dist d)])
    (values v (* w (score v)))))

;; dist-filter : FDist[X] (X -> Boolean) -> FDist[X]
(define (dist-filter d pred)
  (for/discrete-dist ([(v w) (in-dist d)] #:when (pred v))
    (values v w)))

;; dist-join : FDist[FDist[X]] -> FDist[X]
(define (dist-join d)
  #;(dist-bind d values)
  (for*/discrete-dist ([(v w) (in-dist d)]
                       [(vv ww) (in-dist v)])
    (values vv (* w ww))))
