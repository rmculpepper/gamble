;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/pretty
         "../private/dist.rkt"
         "../private/dist-define.rkt"
         "discrete.rkt")
(provide make-mixture-dist*)

(define-dist-type mixture-dist
  ([mix discrete-dist?])
  #:pdf mixture-pdf
  #:sample mixture-sample
  ;; FIXME: define CDF if all have CDF?
  #:guard (lambda (mix _name)
            ;; check all density or all mass
            (normalize-discrete-dist mix)))

(define (make-mixture-dist* ds ws)
  (mixture-dist (make-discrete-dist* ds ws)))

(define (mixture-pdf mix x log?)
  (define p
    (for/sum ([d (in-vector (discrete-dist-values mix))]
              [w (in-vector (discrete-dist-weights mix))])
      (* w (dist-pdf d x #f))))
  (if log? (log p) p))

(define (mixture-sample mix)
  (define d (dist-sample mix))
  (dist-sample d))
