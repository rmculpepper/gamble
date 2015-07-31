;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang s-exp typed-racket/base-env/extra-env-lang

;; Type environment extension needed by matrix-base.rkt

;; Types for racket/serialize:

(require racket/serialize
         (for-syntax (only-in typed-racket/rep/type-rep make-Name make-Opaque)))

(begin-for-syntax
  (define -serialize-info (make-Opaque #'serialize-info?))
  (define -deserialize-info (make-Opaque #'deserialize-info?)))

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

;; ------------------------------------------------------------

(begin-for-syntax
  ;; Box for unsafe export of deserialize info
  ;; Filled by matrix-base.rkt, see also matrix-syntax.rkt
  (provide array-deserialize-info-box)
  (define array-deserialize-info-box (box #f)))
