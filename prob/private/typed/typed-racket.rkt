#lang racket/base
(require (for-syntax racket/base) racket/include)

(define-syntax (include-contents stx)
  (cond [(member (version) '("6.1" "6.1.1"))
         #'(include "typed-racket-6.1.inc")]
        [else
         #'(include "typed-racket-HEAD.inc")]))

(include-contents)
