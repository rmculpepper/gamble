;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang s-exp "instrumenting-lang.rkt"
(require (only-in racket/base [map racket:map]))
(provide map
         filter
         andmap
         ormap
         foldr
         foldl
         build-list)

(define map
  (case-lambda
    [(f xs)
     (unless (procedure? f)
       (raise-argument-error 'map "procedure?" 0 f xs))
     (unless (list? xs)
       (raise-argument-error 'map "list?" 1 f xs))
     (let loop ([xs xs])
       (cond [(pair? xs)
              (cons (f (car xs)) (loop (cdr xs)))]
             [else null]))]
    [(f xs ys)
     (unless (procedure? f)
       (raise-argument-error 'map "procedure?" 0 f xs ys))
     (unless (list? xs)
       (raise-argument-error 'map "list?" 1 f xs ys))
     (unless (list? xs)
       (raise-argument-error 'map "list?" 2 f xs ys))
     (unless (= (length xs) (length ys))
       (error 'map "lists have unequal length"))
     (let loop ([xs xs] [ys ys])
       (cond [(pair? xs)
              (cons (f (car xs) (car ys)) (loop (cdr xs) (cdr ys)))]
             [else null]))]
    [(f xs0 . xss)
     (let ([xss (cons xs0 xss)])
       (unless (procedure? f)
         (apply raise-argument-error 'map "procedure?" 0 f xss))
       (for ([i (in-naturals 1)]
             [xs (in-list xss)])
         (unless (list? xs)
           (apply raise-argument-error 'map "list?" i f xss))
         (unless (= (length xs) (length (car xss)))
           (error 'map "lists have unequal length")))
       (let loop ([xss xss])
         (cond [(pair? (car xss))
                (cons (apply f (racket:map car xss))
                      (loop (racket:map cdr xss)))]
               [else null])))]))

(define (filter f xs)
  (unless (procedure? f)
    (raise-argument-error 'filter "procedure?" 0 f xs))
  (unless (list? xs)
    (raise-argument-error 'filter "list?" 1 f xs))
  (let loop ([xs xs])
    (cond [(pair? xs)
           (if (f (car xs))
               (cons (car xs) (loop (cdr xs)))
               (loop (cdr xs)))]
          [else null])))

(define (andmap f xs)
  (unless (procedure? f)
    (raise-argument-error 'andmap "procedure?" 0 f xs))
  (unless (list? xs)
    (raise-argument-error 'ormap "list?" 1 f xs))
  (let loop ([xs xs])
    (cond [(pair? xs)
           (and (f (car xs)) (loop (cdr xs)))]
          [else #t])))

(define (ormap f xs)
  (unless (procedure? f)
    (raise-argument-error 'ormap "procedure?" 0 f xs))
  (unless (list? xs)
    (raise-argument-error 'ormap "list?" 1 f xs))
  (let loop ([xs xs])
    (cond [(pair? xs)
           (or (f (car xs)) (loop (cdr xs)))]
          [else #f])))

(define (foldr combine base xs)
  (unless (procedure? combine)
    (raise-argument-error 'foldr "procedure?" 0 combine base xs))
  (unless (list? xs)
    (raise-argument-error 'foldr "list?" 2 combine base xs))
  (let loop ([xs xs])
    (cond [(pair? xs)
           (combine (car xs) (loop (cdr xs)))]
          [else base])))

(define (foldl combine base xs)
  (unless (procedure? combine)
    (raise-argument-error 'foldl "procedure?" 0 combine base xs))
  (unless (list? xs)
    (raise-argument-error 'foldl "list?" 2 combine base xs))
  (let loop ([xs xs] [base base])
    (cond [(pair? xs)
           (loop (cdr xs) (combine (car xs) base))]
          [else base])))

(define (build-list n f)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'build-list "exact-nonnegative-integer?" 0 n f))
  (unless (procedure? f)
    (raise-argument-error 'build-list "procedure?" 1 n f))
  (let loop ([i 0])
    (if (< i n)
        (cons (f i) (loop (add1 i)))
        null)))
