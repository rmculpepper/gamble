#lang racket/base
(require (for-syntax racket/base) racket/include)

(define-syntax (include-contents stx)
  (cond [(equal? (version) "6.1")
         #'(include "core-6.1.inc")]
        [else
         #'(include "core-HEAD.inc")]))

(include-contents)
