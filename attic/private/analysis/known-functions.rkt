;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base)
         racket/runtime-path)
(provide classify-function)

;; ========================================

;; Function classification wrt Address

;; A function is non-random-first-order (NRFO) if no stochastic effect
;; (sample, fail, mem, or observe-at) is ever executed in the dynamic
;; extent of a call to that function. In practice, we relax that to
;; "never under reasonable circumstances"---for example, calling
;; 'equal?' can in principle call arbitrary code, but we assume that
;; equality predicates will not call stochastic effects.

;; If a function call is NRFO we can skip the dynamic protocol for
;; address tracking and observation propagation.

;; TODO: add common non-kernel Racket functions
;; TODO: static analysis for locally-defined functions

;; classify-function : id -> (U 'safe 'unsafe 'unknown)
(define (classify-function f-id)
  (if (function-non-random-first-order? f-id)
      'non-random-first-order
      'unsafe))

(define-runtime-module-path-index mod:kernel ''#%kernel)
(define-runtime-module-path-index mod:paramz ''#%paramz)
(define-runtime-module-path-index mod:unsafe ''#%unsafe)
(define-runtime-module-path-index mod:dist/univariate "../dist/univariate.rkt")
(define-runtime-module-path-index mod:dist/discrete "../dist/discrete.rkt")

(define (safe-modules-and-exceptions)
  `([,mod:kernel ,HO-kernel-procedures]
    [,mod:paramz ()]
    [,mod:unsafe ()]
    [,mod:dist/univariate ()]
    [,mod:dist/discrete ()]))

;; f-id must be a function identifier
(define (function-non-random-first-order? f-id)
  (let ([b (identifier-binding f-id)])
    (and (list? b)
         (let* ([def-mpi (car b)]
                [def-name (cadr b)]
                [resolved (module-path-index-resolve def-mpi)])
           (for/or ([safe-mod+exceptions (in-list (safe-modules-and-exceptions))])
             (and (equal? resolved (module-path-index-resolve (car safe-mod+exceptions)))
                  (not (memq def-name (cadr safe-mod+exceptions)))))))))

;; functions defined in kernel, known to be unsafe
;; FIXME: double-check
(define HO-kernel-procedures
  '(;; omit indirect HO functions, like make-struct-type, chaperone-*, impersonate-*
    apply
    map
    for-each
    andmap
    ormap
    call-with-values
    call-with-escape-continuation
    call/ec
    call-with-current-continuation
    call/cc
    call-with-continuation-barrier
    call-with-continuation-prompt
    call-with-composable-continuation
    abort-current-continuation
    call-with-semaphore
    call-with-semaphore/enable-break
    call-with-immediate-continuation-mark
    time-apply
    dynamic-wind
    hash-map
    hash-for-each
    call-with-input-file
    call-with-output-file
    with-input-from-file
    with-output-to-file
    eval
    eval-syntax
    call-in-nested-thread
    ))

#|
To get list of '#%kernel exports:
(define (simplify e) (match e [`(just-meta ,n (rename '#%kernel ,x ,_)) x] [_ #f]))
(define knames
  (filter symbol?
          (map simplify
               (cdr (syntax->datum (expand '(require (rename-in '#%kernel))))))))
|#
