;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require syntax/id-table
         racket/match
         racket/runtime-path)
(provide function-may-call-erp?
         constant-folding-procedure-id?)

;; For the purpose of this module, stochastic effects are `sample` and `mem`.

;; Within model, if we have a call (f e ...), where f has no static addr-accepting
;; variant registered (that is, no static protocol). The following possibilities exist:
;; - f is an instrumented function => NEEDS dynamic protocol
;; - f is a non-instrumented function
;;   - f never has stochastic effects at all (eg, +, cons) => OK either way
;;   - f calls an instrumented stochastic function only in non-tail position (eg, map)
;;     => OKERR either way, addr link broken by non-tail context (as expected)
;;   - f calls an instrumented stochastic function in tail position
;;     => if dynamic protocol used, addr link preserved (AMBIVALENT)
;;        otherwise:
;;       - if call to f is in tail position wrt instrumented function
;;          => inherits wrong address, BAD
;;       - if call to f is in non-tail position wrt instrumented function
;;          => addr link broken by non-tail context (INCONSISTENT)

;; Conclusions:
;; - Accept that non-instrumented functions that call instrumented functions in
;;   in tail context inherit address link, and make that behave consistently.
;; - Then always allowed to insert dynamic protocol.
;; - Allowed to OMIT dynamic protocol only if
;;   - ('no-effect) f is known to never have stochastic effects (eg, +, cons)
;;   - ('no-tail) f is known to never call instrumented function in tail position (eg, map)


;; function-may-call-erp? : Syntax -> Boolean
(define (function-may-call-erp? f-stx)
  (cond [(identifier? f-stx)
         (define-values (def-mod def-name)
           (id->def-mod+name f-stx))
         (eprintf "may-effect: ~v ~v\n" def-mod def-name)
         (cond [(hash-ref may-effect def-mod #f)
                => (match-lambda
                     [(cons default table)
                      (eprintf "  => ~v\n" (hash-ref table def-name default))
                      (hash-ref table def-name default)])]
               [else #t])]
        [else #t]))

;; constant-folding-procedure-id? : Identifier -> Boolean
(define (constant-folding-procedure-id? id)
  (define-values (def-mod def-name) (id->def-mod+name id))
  (eprintf "cfold: ~v ~v\n" def-mod def-name)
  (cond [(free-id-table-ref constant-folding-table id #f) #t]
        [(hash-ref constant-folding def-mod #f)
         => (match-lambda
              [(cons default table)
               (hash-ref table def-name default)])]
        [else #f]))

;; ============================================================

(define (id->def-mod+name id)
  (match (identifier-binding id)
    [(list* def-mpi def-name _)
     (values (resolved-module-path-name
              (module-path-index-resolve def-mpi))
             def-name)]
    [_ (values #f #f)]))

(define (list->hashset xs)
  (for/fold ([h (hasheq)]) ([x (in-list xs)]) (hash-set h x #t)))

;; may-effect:runtime : (Hasheq Symbol #t) -- set of functions from #%runtime
;; module that might call an argument in tail position.
(define may-effect:runtime
  (list->hashset
   '(;;abort-current-continuation
     andmap             ;; last element in tail position
     apply
     byte-pregexp       ;; failure handler
     byte-regexp        ;; failure handler
     call-in-continuation
     call-in-nested-thread
     call-with-composable-continuation
     ;; call-with-continuation-barrier
     ;; call-with-continuation-prompt
     call-with-current-continuation
     call-with-escape-continuation
     call-with-immediate-continuation-mark
     call-with-input-file
     call-with-output-file
     call-with-values
     ;; checked-procedure-check-and-extract
     ;; dynamic-wind
     hash-ref           ;; failure handler
     hash-ref-key       ;; failure handler
     ormap              ;; last element in tail position
     pregexp            ;; failure handler
     regexp             ;; failure handler
     sync
     sync/enable-break
     sync/timeout
     sync/timeout/enable-break
     thread
     will-execute
     will-try-execute
     with-input-from-file
     with-output-to-file
     )))

;; may-effect : (Hash CanonicalModulePath (cons Boolean (Hasheq Symbol Boolean)))
(define may-effect
  (hash '#%runtime (cons #f may-effect:runtime)
        '(lib "gamble/private/dist/base.rkt") (cons #f (hasheq))
        '(lib "gamble/private/dist/discrete.rkt") (cons #f (hasheq))
        '(lib "gamble/private/dist/multinomial.rkt") (cons #f (hasheq))
        '(lib "gamble/private/dist/transformer.rkt") (cons #f (hasheq))
        '(lib "gamble/private/dist/univariate.rkt") (cons #f (hasheq))))

;; constant-folding:runtime : (Hasheq Symbol #t) -- set of functions from
;; #%runtime module that can be constant-folded.
(define constant-folding:runtime
  (list->hashset
   '(;; Boolean
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

;; constant-folding : (Hash CanonicalModulePath (cons Boolean (Hasheq Symbol Boolean)))
(define constant-folding
  (hash '#%runtime (cons #f constant-folding:runtime)))

#|
To get list of '#%runtime exports:
(let-values ([(vars stxs) (module->exports ''#%runtime)])
  (map car (cdr (assoc 0 vars))))
|#

;; ============================================================

(define constant-folding-table (make-free-id-table))
(for ([id (in-list expanded-constant-folding-ids)])
  (free-id-table-set! constant-folding-table id #t))

(module constant-folding-ct racket/base
  (require (for-syntax racket/base)
           racket/flonum
           racket/fixnum
           "../dist.rkt")
  (begin-for-syntax
    (define expand-constant-folding-ids
      (syntax->list
       #'(;; racket/flonum
          fl+ fl- fl* fl/ flabs fl= fl< fl<= fl> fl>= flmin flmax
          flround flfloor flceiling fltruncate
          flsin flcos fltan flasin flacos flatan flexp fllog flsqrt flexpt
          ;; racket/fixnum
          fx+ fx- fx* fxquotient fxremainder fxmodulo fxabs
          fxand fxior fxxor fxlshift fxrshift
          fx= fx< fx<= fx> fx>= fxmin fxmax
          ;; ----------------------------------------
          ;; dist
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
          boolean-dist boolean-dist?
          make-discrete-dist discrete-dist?
          ;; univariate
          beta-dist beta-dist?
          cauchy-dist cauchy-dist?
          exponential-dist exponential-dist?
          gamma-dist gamma-dist?
          logistic-dist logistic-dist?
          normal-dist normal-dist?
          uniform-dist uniform-dist?
          triangle-dist triangle-dist?
          pareto-dist pareto-dist?
          student-t-dist student-t-dist?
          geometric-dist geometric-dist?
          poisson-dist poisson-dist?
          bernoulli-dist bernoulli-dist?
          binomial-dist binomial-dist?
          negative-binomial-dist negative-binomial-dist?
          categorical-dist categorical-dist?
          ))))
  (define-syntax (define/provide-expanded-ids stx)
    (syntax-case stx ()
      [(_ name)
       (with-syntax ([(eid ...)
                      (filter identifier?
                              (map (lambda (id) (local-expand id 'expression null))
                                   expand-constant-folding-ids))])
         #'(begin-for-syntax
             (define name (syntax->list (quote-syntax (eid ...))))
             (provide name)))]))
  (define/provide-expanded-ids expanded-constant-folding-ids))
(require (for-template (submod "." constant-folding-ct)))
