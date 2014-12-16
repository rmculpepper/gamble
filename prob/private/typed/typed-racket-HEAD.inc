;; UNTESTED
;; Copied from pkgs/typed-racket-lib/typed-racket/typed-racket.rkt

#lang racket/base

(require
 (for-syntax racket/base racket/lazy-require
             typed-racket/standard-inits)
 (for-syntax typed-racket/utils/timing)) ;; only for timing/debugging

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         with-type
         (for-syntax do-standard-inits))

(define-syntax-rule (drivers [name sym] ...)
  (begin
    (begin-for-syntax
      (lazy-require ("core.rkt" (sym ...))))
    (define-syntax (name stx)
      (do-time (format "Calling ~a driver" 'name))      
      (do-time (format "Loaded core ~a" 'sym))
      (begin0 (sym stx)
              (do-time "Finished, returning to Racket")))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
