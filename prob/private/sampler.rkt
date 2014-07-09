;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class)
(provide weighted-sampler<%>
         sampler<%>
         sampler-base%)

;; ============================================================
;; Sampler interfaces

(define weighted-sampler<%>
  (interface ()
    sample/weight))

;; A sampler is an applicable object taking zero arguments. When
;; applied, it produces a single sample.
(define sampler<%>
  (interface* (weighted-sampler<%>)
              ([prop:procedure (lambda (this) (send this sample))])
    sample))

;; ============================================================
;; Utility base classes

;; Automatic impl of weighted sampler from "ordinary" sampler.
(define sampler-base%
  (class* object% (sampler<%>)
    (super-new)
    (abstract sample)
    (define/public (sample/weight) (cons (sample) 1))))
