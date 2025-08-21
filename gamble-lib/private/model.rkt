;; Copyright (c) 2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "base.rkt"
         "model/addr.rkt"
         "model/instrument.rkt")
(provide model*)

(define-syntax (model* stx)
  (syntax-parse stx
    [(_ #:auto-label e:expr ...)
     #'(model
        (lambda (ctx)
          (with-get-ADDR base-addr
            (with-ctx ctx
              (let ([run (instrument-expr (let () e ...))])
                (run base-addr))))))]
    [(_ e:expr ...)
     #'(model
        (lambda (ctx)
          (with-ctx ctx
            (let () e ...))))]
    ))
