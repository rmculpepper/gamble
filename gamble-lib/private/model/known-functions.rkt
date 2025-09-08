;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require syntax/id-table
         racket/match
         racket/runtime-path)
(provide register-function!
         function-may-call-erp?
         constant-folding-procedure-id?)

;; FClass is one of
;; - #f -- No stochastic effect is expected to occur within the dynamic extent
;;         of a call to the function, under reasonable circumstances.
;;         For example, `equal?` may trigger custom comparison functions, which
;;         could have stochastic effects, but we classify `equal?` as effect-free.
;; - (true value) -- Stochastic effects are possible.

;; Stochastic effects are `sample` and `mem`. If a function call is effect-free,
;; we can skip the dynamic protocol for address tracking.

;; TODO: add common non-kernel Racket functions
;; TODO: static analysis for locally-defined functions

;; function-table : free-id-table[ FClass ]
(define function-table (make-free-id-table))

(define (register-function! id fclass)
  (free-id-table-set! function-table id fclass))

;; function-may-call-erp? : Syntax -> FClass
(define (function-may-call-erp? f-stx)
  (cond [(identifier? f-stx)
         (free-id-table-ref function-table f-stx
                            (lambda () (function-may-call-erp* f-stx)))]
        [else 'unknown]))
(define (function-may-call-erp* f-id)
  (match (identifier-binding f-id)
    [(list* def-mpi def-name _)
     (define def-mod
       (resolved-module-path-name
        (module-path-index-resolve def-mpi)))
     (cond [(equal? def-mod ''#%runtime)
            (and (hash-ref runtime-info def-name #f) 'runtime)]
           [else #f])]
    [else 'unknown]))

(define runtime-info
  ((lambda (syms) (for/fold ([h (hasheq)]) ([sym (in-list syms)]) (hash-set h sym #t)))
   '(abort-current-continuation
     andmap
     apply
     assert-unreachable
     byte-pregexp
     byte-regexp
     bytes-close-converter
     bytes-convert
     bytes-convert-end
     bytes-open-converter
     call-in-continuation
     call-in-nested-thread
     call-with-composable-continuation
     call-with-continuation-barrier
     call-with-continuation-prompt
     call-with-current-continuation
     call-with-escape-continuation
     call-with-immediate-continuation-mark
     call-with-input-file
     call-with-output-file
     call-with-semaphore
     call-with-semaphore/enable-break
     call-with-values
     chaperone-box
     chaperone-channel
     chaperone-continuation-mark-key
     chaperone-evt
     chaperone-hash
     chaperone-of?
     chaperone-procedure
     chaperone-procedure*
     chaperone-prompt-tag
     chaperone-struct
     chaperone-struct-type
     chaperone-vector
     chaperone-vector*
     checked-procedure-check-and-extract
     dynamic-wind
     for-each
     hash-for-each
     hash-map
     ;hash-ref
     ;hash-ref-key
     map
     ormap
     ;pregexp
     ;regexp
     ;regexp-replace
     ;regexp-replace*
     ;stencil-vector-ref
     stencil-vector-update
     sync
     sync/enable-break
     sync/timeout
     sync/timeout/enable-break
     thread
     time-apply
     will-execute
     will-try-execute
     with-input-from-file
     with-output-to-file
     )))

#|
To get list of '#%runtime exports:
(let-values ([(vars stxs) (module->exports ''#%runtime)])
  (map car (cdr (assoc 0 vars))))
|#

;; ============================================================

(module constant-folding-expand-ct racket/base
  (require (for-syntax racket/base)
           "../dist.rkt")
  (begin-for-syntax
    (define expand-constant-folding-ids
      (syntax->list
       #'(;; dist
          dist?
          dist-pdf
          dist-density
          enumerable-dist?
          finite-dist?
          real-dist?
          dist-cdf
          continuous-dist?
          integer-dist?
          ;; monad
          dist-unit
          ;; discrete
          boolean-dist
          make-discrete-dist
          ;; univariate
          beta-dist
          cauchy-dist
          exponential-dist
          gamma-dist
          logistic-dist
          normal-dist
          uniform-dist
          triangle-dist
          pareto-dist
          student-t-dist
          geometric-dist
          poisson-dist
          bernoulli-dist
          binomial-dist
          negative-binomial-dist
          categorical-dist
          ))))
  (define-syntax (define/provide-expanded-ids stx)
    (syntax-case stx ()
      [(_ name)
       (with-syntax ([(eid ...)
                      (map (lambda (id) (local-expand id 'expression null))
                           expand-constant-folding-ids)])
         #'(begin-for-syntax
             (define name (syntax->list (quote-syntax (eid ...))))
             (provide name)))]))
  (define/provide-expanded-ids expanded-constant-folding-ids))

(module constant-folding-ct racket/base
  (require (for-template racket/base racket/fixnum racket/flonum
                         (submod ".." constant-folding-expand-ct))
           syntax/id-table)
  (provide (all-defined-out))
  (define constant-folding-ids
    (syntax->list
     #'(;; Boolean
        boolean? eq? eqv? equal? equal-always? not immutable?
        ;; Numeric
        number? complex? real? rational?
        exact-integer? exact-positive-integer? exact-nonnegative-integer?
        inexact-real? fixnum? flonum? integer? exact? inexact?
        zero? positive? negative? even? odd? exact->inexact inexact->exact
        add1 sub1 + - * / quotient remainder modulo
        abs min max round floor ceiling truncate sqrt
        = < <= > >=
        log expt exp sin cos tan asin acos atan
        ;; racket/flonum
        fl+ fl- fl* fl/ flabs fl= fl< fl<= fl> fl>= flmin flmax
        flround flfloor flceiling fltruncate
        flsin flcos fltan flasin flacos flatan flexp fllog flsqrt flexpt
        ;; racket/fixnum
        fx+ fx- fx* fxquotient fxremainder fxmodulo fxabs
        fxand fxior fxxor fxlshift fxrshift
        fx= fx< fx<= fx> fx>= fxmin fxmax
        ;; Characters
        char? char->integer integer->char
        char=? char<? char<=? char>? char>=?
        ;; Symbols
        symbol? symbol-interned? symbol-unreadable? symbol<?
        ;; Keywords
        keyword? keyword<?
        ;; Pairs and lists
        null? pair? cons car cdr cadr cddr caddr cdddr list? list list*
        length list-ref list-tail append reverse
        ;; Vectors
        vector? vector vector-immutable vector-length
        list->vector vector->list vector->immutable-vector
        ;; Boxes
        box? box box-immutable
        ;; Hashes
        hash? hash-equal? hash-eq? hash-eqv? hash-equal-always? hash-strong? hash-weak?
        hash hashalw hasheq hasheqv hash-count
        ;; Procedures
        procedure?
        ;; Void
        void?
        ;; Other
        values
        )))
  (define constant-folding-table (make-free-id-table))
  (for ([id (in-list constant-folding-ids)])
    (free-id-table-set! constant-folding-table id #t))
  (for ([id (in-list expanded-constant-folding-ids)])
    (free-id-table-set! constant-folding-table id #t))
  (define (constant-folding-procedure-id? id)
    (free-id-table-ref constant-folding-table id #f)))
(require (submod "." constant-folding-ct))

(module constant-folding-rt racket/base
  (require (for-syntax racket/base (submod ".." constant-folding-ct)))
  (provide (all-defined-out))
  (define constant-folding-hash
    (let-syntax ([cfh (lambda (stx)
                        (with-syntax ([(cfid ...) constant-folding-ids])
                          #'(hash (~@ cfid #t) ...)))])
      (cfh)))
  (define (constant-folding-procedure? proc)
    (hash-ref constant-folding-hash proc #f)))
