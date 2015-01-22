;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang s-exp typed-racket/base-env/extra-env-lang

;; Type environment extension needed by matrix-base.rkt

;; Types for racket/serialize:

(require racket/serialize
         (for-syntax (only-in typed-racket/rep/type-rep make-Name)))

(begin-for-syntax
  (define -serialize-info (make-Name #'serialize-info null #f #f))
  (define -deserialize-info (make-Name #'deserialize-info null #f #f)))

(define (make-deserialize-info* make)
  (make-deserialize-info
   (lambda args (make (list->vector args)))
   (lambda () (error 'deserialize "cycle not permitted"))))

(type-environment
 [prop:serializable
  -Struct-Type-Property]
 ;; HACK: Specialize vector to one element, for ease of typing.
 ;; Maybe can use polydots to do better?
 [make-serialize-info
  (-poly (a b)
         (-> (-> a b)
             (Un (-Syntax -Symbol)
                  (-pair -Symbol -Module-Path-Index))
             -Boolean
             -Pathlike
             -serialize-info))]
 [make-deserialize-info*
  (-poly (a)
         (-> (-> Univ a)
             -deserialize-info))]
 )
