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
         enumerate
         importance-sampler
         label)

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
      [(succeed) (cdr v)]
      [(fail) (rejection-sample thunk)])))

;; ----

(begin-for-syntax
 (define-syntax-class special-condition
   (pattern ((~datum =) label value:expr)
            #:with e #'(cons `label (spcond:equal value)))
   (pattern ((~datum ~) label dist:expr)
            ;; FIXME: contract on dist
            #:with e #'(cons `label (spcond:drawn dist)))))

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sampler def:expr ... result:expr
                 (~optional (~seq #:when condition:expr))
                 (~seq #:cond sp:special-condition)
                 ...)
     (template
      (mh-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (list sp.e ...)))]))

;; ----

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr
                (~or (~optional (~seq #:when condition:expr))
                     (~seq #:cond sp:special-condition)
                     (~optional (~seq #:limit limit:expr))
                     (~optional (~seq #:normalize? normalize?)))
                ...)
     (template
      (enumerate*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))
       (list sp.e ...)
       (?? (?@ #:limit limit))
       (?? (?@ #:normalize? normalize?))))]))

(define-syntax (importance-sampler stx)
  (syntax-parse stx
    [(importance-sample def:expr ... result:expr (~optional (~seq #:when condition:expr)))
     (template
      (importance-sampler*
       (lambda () def ... (begin0 result (unless (?? condition #t) (fail))))))]))

;; ----

(define-syntax-rule (label l e)
  (parameterize ((current-label l)) e))
