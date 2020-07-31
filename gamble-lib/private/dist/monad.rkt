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
(define (dirac v) (discrete-dist (vector-immutable v) '#(1) 1))
(define (dist-unit v) (dirac v))

;; dist-fmap : FDist[X] (X -> Y) -> FDist[Y]
(define (dist-fmap d f)
  (alist->discrete-dist
   (for/list ([(v w) (in-dist d)])
     (cons (f v) w))))

;; dist-bind : FDist[X] (X -> FDist[Y]) -> FDist[Y]
(define (dist-bind d f)
  (alist->discrete-dist
   (for*/list ([(v w) (in-dist d)]
               [(v* w*) (in-dist (f v))])
     (cons v* (* w w*)))))

;; dist-bindx : FDist[X] (X -> FDist[Y]) -> FDist[(list X Y)]
(define (dist-bindx d f)
  (alist->discrete-dist
   (for*/list ([(v w) (in-dist d)]
               [(v* w*) (in-dist (f v))])
     (cons (list v v*) (* w w*)))))

;; dist-filter : FDist[X] (X -> Boolean) -> FDist[X]
(define (dist-filter d pred)
  (alist->discrete-dist
   (for*/list ([(v w) (in-dist d)]
               #:when (pred v))
     (cons v w))))

;; dist-join : FDist[FDist[X]] -> FDist[X]
(define (dist-join d)
  (for ([(v w) (in-dist d)])
    (unless (finite-dist? v)
      (error 'dist-join
             "expected finite distribution of finite distributions\n  given: ~e"
             d)))
  (dist-bind d values))
