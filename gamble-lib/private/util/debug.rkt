;; Copyright 2016 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(provide verbose?
         with-verbose>
         vprintf)

;; ============================================================
;; Debugging

;; FIXME: add logger?

(define verbose? (make-parameter #f))
(define verbose-indent (make-parameter 0))

(define-syntax-rule (with-verbose> body ...)
  (parameterize ((verbose-indent (+ (verbose-indent) 2))) body ...))

(define-syntax-rule (vprintf fmt arg ...)
  (when #t
    (when (verbose?)
      (eprintf "# ~a" (make-string (verbose-indent) #\space))
      (eprintf fmt arg ...))))
