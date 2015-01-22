#lang gamble

(require racket/class
         gamble/viz
         (prefix-in p: plot))

#| Linear regression using Hamiltonian Monte Carlo |#

(define (indexed-label sym idx)
  (string->symbol (format "~s-~s" sym idx)))


; given slope, intercept and error variance, make a point y = a*x+b+e
(define ((make-point a b e) x)
  (+ (* x a) b
     (normal 0 e)))

(define (make-lr a b e n)
  (define ys (build-vector n (make-point a b e)))
  
  (define model
    (hmc-sampler
     (define A (label 'A (derivative (normal 0 10) #f #f)))
     (define B (label 'B (derivative (normal 0 10) #f #f)))
     (define E (label 'E 
                      (add1 (derivative (gamma 1 1) #f #f))))
   
     (define (data x y)
       (define Y-x (indexed-label 'Y x))
       (label Y-x
              (derivative (observe-sample (normal-dist (+ (* A x) B)
                                                       E)
                                      y)
                          [(A B) (λ (a b) (values x 1))]
                          [(E) (λ (e) 1)]
                          )))

     (for ([x n])
       (data x (vector-ref ys x)))

     (vector A B)
     #:epsilon 0.009
     #:L 100
     ))
  
  (values ys model))

(define-values (ys lr) (make-lr -20 12 1 10))

(define pts 
  (for/list ([x (in-naturals)]
             [y ys])
    (vector x y)))

(define (pts-rndr) (p:points pts))

(define (line-rndr v)
  (define a (vector-ref v 0))
  (define b (vector-ref v 1))
  (p:function (λ (x) (+ b (* x a))) #f #f #:alpha 0.05))

(define (show-me vs)
  (define rs (map line-rndr vs))
  (p:plot (cons (pts-rndr) rs)))

(define (example)
  (begin0
    (show-me (repeat lr 100))
    (send lr info)))
