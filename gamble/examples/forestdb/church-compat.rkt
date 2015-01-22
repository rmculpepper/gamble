#lang gamble
(require (for-syntax racket/base
                     syntax/parse))
(provide all
         any
         fold
         pair
         sum
         uniform-draw
         multinomial
         rejection-query
         enumeration-query)

(define (all ls) (andmap values ls))
(define (any ls) (ormap values ls))
(define (fold f init lst) (foldl f init lst))
(define (pair a b) (cons a b))
(define (sum ls) (apply + ls))

(define-syntax rejection-query
  (syntax-parser
    [(_ def/expr ... result-expr #:when condition)
     #'((rejection-sampler def/expr ... result-expr #:when condition))]))

(define-syntax enumeration-query
  (syntax-parser
    [(_ def/expr ... result-expr #:when condition)
     #'(discrete-dist->lists
        (enumerate def/expr ... result-expr #:when condition #:limit #f))]))

(define (discrete-dist->lists dd)
  (list (vector->list (discrete-dist-values dd))
        (for/list ([v (discrete-dist-values dd)])
          (dist-pdf dd v))))

(define (uniform-draw xs)
  (discrete* xs))

(define (multinomial xs ws)
  (discrete* xs ws))
