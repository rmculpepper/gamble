#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "prob.rkt"
         "prob-db.rkt"
         "prob-enum.rkt")
(provide rejection-sampler
         mh-sampler
         enumerate)

(define-syntax (rejection-sampler stx)
  (syntax-parse stx
    [(rejection-query def:expr ... result:expr #:when condition:expr)
     #'(lambda ()
         (rejection-sample (lambda () def ... (cons result condition))
                           cdr car))]))

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sample def:expr ... result:expr #:when condition:expr)
     #'(mh-sampler* (lambda () def ... (cons result condition)) cdr car)]))

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr #:when condition:expr)
     #'(enumerate* (lambda () def ... (cons result condition)) cdr car)]))
