;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
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
        (rejection-sample (lambda () def ... (cons result (?? condition #t)))
                          cdr car)))]))

(define (rejection-sample thunk pred [project values])
  (let ([v (thunk)])
    (if (pred v)
        (project v)
        (rejection-sample thunk pred project))))

;; ----

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sample def:expr ... result:expr (~optional (~seq #:when condition:expr)))
     (template
      (mh-sampler* (lambda () def ... (cons result (?? condition #t))) cdr car))]))

;; ----

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr
                (~or (~optional (~seq #:when condition:expr))
                     (~optional (~seq #:limit limit:expr))
                     (~optional (~seq #:normalize? normalize?)))
                ...)
     (template
      (enumerate* (lambda () def ... (cons result (?? condition #t))) cdr car
                  (?? (?@ #:limit limit))
                  (?? (?@ #:normalize? normalize?))))]))
