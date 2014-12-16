;; Adapted from pkgs/typed-racket-lib/typed-racket/typed-racket.rkt
;; Based on 6.1, but seems to be close enough for 6.1.1 too.

(require
 (for-syntax racket/base racket/lazy-require
             typed-racket/standard-inits)
 (for-syntax typed-racket/utils/timing) ;; only for timing/debugging
 ;; the below requires are needed since they provide identifiers
 ;; that may appear in the residual program
 ;; TODO: figure out why this are needed here and not somewhere else
 (submod typed-racket/private/type-contract predicates)
 typed-racket/utils/utils
 (for-syntax typed-racket/utils/utils)
 typed-racket/utils/any-wrap unstable/contract racket/contract/parametric)

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
