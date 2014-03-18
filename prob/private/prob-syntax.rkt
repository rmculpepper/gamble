;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
         "prob-hooks.rkt"
         "prob-util.rkt"
         "prob-mh.rkt"
         "prob-enum.rkt")
(provide rejection-sampler
         mh-sampler
         enumerate)

(define-syntax (rejection-sampler stx)
  (syntax-parse stx
    [(rejection-query def:expr ... result:expr
                      (~optional (~seq #:when condition:expr)))
     (template
      (lambda ()
        (rejection-sample
         (lambda () def ... (begin0 result (unless (?? condition #t) (fail)))))))]))

(define (rejection-sample thunk)
  (let ([v (let/ec escape
             (parameterize ((current-fail (lambda (r) (escape (cons 'fail r)))))
               (cons 'succeed (thunk))))])
    (case (car v)
      [(succeed) (cadr v)]
      [(fail) (rejection-sample thunk)])))

;; ----

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sample def:expr ... result:expr (~optional (~seq #:when condition:expr)))
     (template
      (mh-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))))]))

;; ----

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr
                (~or (~optional (~seq #:when condition:expr))
                     (~optional (~seq #:limit limit:expr))
                     (~optional (~seq #:normalize? normalize?)))
                ...)
     (template
      (enumerate*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (?? (?@ #:limit limit))
       (?? (?@ #:normalize? normalize?))))]))
