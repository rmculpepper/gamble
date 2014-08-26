#lang prob
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
         enumeration-query
         factor)

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

;; hack to weight a trace by an arbitrary (log) factor
(define (factor l)
  (define p (exp l))
  (define half-len (* 1/2 (/ p)))
  (observe-at (uniform-dist (- half-len) half-len) 0))

(define (uniform-draw xs)
  (discrete* xs))

(define (multinomial xs ws)
  (discrete* xs ws))
