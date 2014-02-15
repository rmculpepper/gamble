#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
         "prob.rkt"
         "prob-db.rkt"
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

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sample def:expr ... result:expr (~optional (~seq #:when condition:expr)))
     (template
      (mh-sampler* (lambda () def ... (cons result (?? condition #t))) cdr car))]))

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr
                (~optional (~seq #:when condition:expr))
                (~optional (~seq #:limit limit:expr)))
     (template
      (enumerate* (lambda () def ... (cons result (?? condition #t))) cdr car
                  (?? (?@ #:limit limit))))]))
