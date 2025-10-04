;; Copyright 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         "../private/util/dnum.rkt")
(provide dnum?
         (contract-out
          [dnum-logspace? (-> dnum? boolean?)]
          [dnum-linear? (-> dnum? boolean?)]
          [linear-dnum (-> (>=/c 0) dnum?)]
          [logspace-dnum (-> flonum? dnum?)]
          [dnum->linear-real (-> dnum? real?)]
          [dnum->logspace-real (-> dnum? real?)]
          [dnum-zero? (-> dnum? boolean?)]
          [dnum+ (-> dnum? dnum? dnum?)]
          [dnum- (-> dnum? dnum? dnum?)]
          [dnum* (-> dnum? dnum? dnum?)]
          [dnum/ (-> dnum? dnum? dnum?)]
          [dnum-sum (-> (listof dnum?) dnum?)]
          [dnum-product (-> (listof dnum?) dnum?)]
          [dnum=? (-> dnum? dnum? boolean?)]
          [dnum<? (-> dnum? dnum? boolean?)]
          [dnum<=? (-> dnum? dnum? boolean?)]))
