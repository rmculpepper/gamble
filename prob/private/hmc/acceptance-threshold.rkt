;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.
#lang racket/base

(require "system.rkt")

(provide hmc-acceptance-threshold)

;; hmc-acceptance-threshold : HMC-System HMC-System -> (Values Real Real)
;;
;; Compute the acceptance ratio for a move from the current system
;; to the proposed system.
;; Returns (values next-energy threshold) where next-energy is energy of the
;; proposed system and threshold is the acceptance threshold.
(define (hmc-acceptance-threshold curr-sys next-sys)
  (define next-energy (hmc-system-energy next-sys))
  (define curr-energy (hmc-system-energy curr-sys))
  
  ; (x0 + p0) - (x* + p*)
  
  (define threshold
    ; Do I need to worry about cancellation here?
    (- curr-energy next-energy))
  
  (values next-energy threshold))
