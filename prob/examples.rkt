;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang prob
(require racket/format racket/class prob/viz)

(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (bernoulli) (sum-n-flips (sub1 n)))))

(define s-mh (mh-sampler (sum-n-flips 10)))

#|
;; To inspect, enter the following commands at the repl:
(parameterize ((verbose? #t))
  (eprintf "** Generating first sample\n")
  (define sample1 (s-mh))
  (eprintf "** Generating second sample\n")
  (define sample2 (s-mh))
  (list sample1 sample2))

;; During evaluation, prints lines like
;; - NEW flip: <address>
;; or 
;; - REUSED flip: <address>
;; to indicate new or reused ERP choices.

;; The two samples should be at most 1 apart, because all but one
;; flip should have been reused.
|#

;; tail-recursive version
(define (sum-n-flips* n)
  (let loop ([n n] [acc 0])
    (if (zero? n)
        acc
        (loop (sub1 n) (+ acc (bernoulli))))))

(define s-mh* (mh-sampler (sum-n-flips* 10)))

;;(hist (repeat (lambda () (sum-n-flips 10)) 1000))
;;(hist (repeat s-mh 1000))
;;(hist (repeat s-mh* 1000))

;; ----

;; ** interaction with untranslated code causes problems

;; map is now okay; comes from private/ho-functions.rkt w/ instrumented call sites
(define (ok-map n)
  (apply + (map (lambda (n) (bernoulli)) (for/list ([i n]) i))))
#|
(parameterize ((verbose? #t))
    ((mh-sampler (ok-map 10))))
|#

;; racket's map is bad; causes context collisions
(require (only-in racket/base [map racket:map]))
(define (bad-map n)
  (apply + (racket:map (lambda (n) (bernoulli)) (for/list ([i n]) i))))
#|
(parameterize ((verbose? #t))
    ((mh-sampler (bad-map 10))))
|#

;; BUT for/* is now okay; annotator rewrites its applications
(define (ok-for n)
  (apply + (for/list ([i n]) (bernoulli))))
#|
(parameterize ((verbose? #t))
    ((mh-sampler (ok-for 10))))
|#

;; ----

;; ** mem

(define (make-mem-mh n)
  (mh-sampler
   ;; Note: need to call mem within mh-sampler, not outside
   (define get-the-flip (mem (lambda (n) (bernoulli))))
   (for/sum ([i n]) (get-the-flip (modulo i 5)))))
#|
(parameterize ((verbose? #t))
  ((make-mem-mh 10)))
;; first 5 NEW, second 5 MEMOIZED
|#

;; ----

(define (geom p)
  (if (flip p) 0 (add1 (geom p))))
#|
(enumerate (geom 1/2) #:limit 1e-3)
(enumerate (geom 1/2) #:limit 1e-9 #:normalize? #f)
|#

#|
;; enumeration and mem
(enumerate
 (define f (mem (lambda (n) (bernoulli))))
 (list (f 1) (f 2) (f 1) (f 2)))
;; Should produce 4 values, each with prob 0.25.

(enumerate
 (define A (flip))
 (define B (flip))
 A
 #:when (or A B))

(enumerate
 (define A (geom 1/2))
 (define B (* 1/2 (geom 1/2)))
 A
 #:when (> A B))

(enumerate 
   (define A (geom 1/2))
   A
   #:when (< 10 A 20))

(enumerate
 (define (drunk-flip)
   (if (flip 0.9)
       (fail) ;; dropped the coin
       (flip .5)))
 (define (drunk-andflips n)
   (cond [(zero? n)
          #t]
         [else
          (and (drunk-flip)
               (drunk-andflips (sub1 n)))]))
 (drunk-andflips 10)
 #:normalize? #f
 ;; Need to disable limit to detect #t case
 #:limit #f)
|#

;; Nested enumerations
#|
(enumerate
 (define A (flip))
 (define B
   (enumerate
    (define C (flip))
    (define D (flip))
    (or C D)
    #:when (or (and C D) A)))
 (list A B))

;; with mem (trivial)
(enumerate
 (define A (mem flip))
 (define B
   (enumerate
    (define C (flip))
    (define D (flip))
    (or C D)
    #:when (or (and C D) (A))))
 (list (A) B))

;; memoized function escaping context produces error
(enumerate
 (define B
   (enumerate
    (mem flip)))
 ((caar B)))
|#

#|
(define (other-color c) (if (eq? c 'blue) 'green 'blue))
(enumerate
 (define num-balls (max 1 (poisson 6)))
 (define color (mem (lambda (b) (discrete (list 'blue 'green)))))
 (define drawn (mem (lambda (d) (discrete num-balls))))
 (define (obs-color d)
   (if (flip 0.8) 
       (color (drawn d))
       (other-color (color (drawn d)))))
 num-balls
 #:when (equal? (map obs-color '(1 2 3 4))
                '(blue blue blue green)))

(enumerate
 (define num-balls (max 1 (poisson 6)))
 (define colors (for/list ([b num-balls]) (discrete '(blue green))))
 (define drawn (for/list ([d 4]) (discrete num-balls)))
 (define (obs-color d)
   (if (flip 0.8) 
       (list-ref colors (list-ref drawn d))
       (other-color (list-ref colors (list-ref drawn d)))))
 num-balls
 #:when (equal? (for/list ([d 4]) (obs-color d))
                '(blue blue blue green))
 #:limit 1e-1)
|#

#|
;; reify/reflect = enumerate and discrete/enumeration
(define (xor a b)
  (and (or a b) (not (and a b))))

(define (xor-flips n)
  (let loop ([n n])
    (if (zero? n)
        #f
        (xor (flip) (loop (sub1 n))))))

(time (enumerate (xor-flips 12)))

(define (xor-flips* n)
  (let loop ([n n])
    (if (zero? n)
        #f
        (let ([r (discrete-from-enumeration
                  (enumerate (loop (sub1 n))))])
          (xor (flip) r)))))

(time (enumerate (xor-flips* 120)))
|#

#|
;; mem generalizes EPP letlazy
(define (flips-all-true n)
  (enumerate
   (define Flips (for/list ([i n]) (flip)))
   (andmap values Flips)))

(time (flips-all-true 12))

(define (flips-all-true* n)
  (enumerate
   (define LFlips (for/list ([i n]) (mem flip)))
   (andmap (lambda (p) (p)) LFlips)))

(time (flips-all-true* 12))
|#

;; ----

;; "Special Conditioning"
#|
;; Note: syntax is undocumented, still in flux
(define s-c
  (mh-sampler
   (define R (normal 10 3))
   (define S (label 'S (normal R 1)))
   R
   #:cond (= S 9)))
(sampler->mean+variance s-c 1000)

;; Works for enumerate also!
(enumerate
 (define A (label 'A (geom 1/2)))
 (define B (label 'B (normal A 1)))
 A
 #:cond (= B 10))
|#

;; ----

(define (cmp prog [iters 1000])
  (values
   (discrete-dist-error
    (sampler->discrete-dist (mh-sampler (prog)) iters)
    (enumerate (prog)))
   (discrete-dist-error
    (sampler->discrete-dist (rejection-sampler (prog)) iters)
    (enumerate (prog)))))

;; ----

#|
;; Alternative formulation of CD problem, using observe-at directly.
(define s-c
  (mh-sampler
   (define R (normal 10 1))
   (observe-at (normal-dist R 1) 9)
   R))
(sampler->mean+variance s-c 1000)
|#

;; ----

(define (make-lr a b e n)
  (define ys (build-vector n (lambda (x) (+ (* x a) b (normal 0 e)))))
  (mh-sampler
   (define A (normal 0 10))
   (define B (normal 0 10))
   (define E (add1 (gamma 1 1)))
   (define (data x y)
     (observe-at (normal-dist (+ (* A x) B) E) y))
   (for ([x n])
     (data x (vector-ref ys x)))
   (vector A B E)))
(define lr (make-lr 3 12 1 100))

;;(for ([i #e1e4]) (void (lr))) ;; burn in
;;(send lr MAP-estimate #e1e3)
;;(send lr MAP-estimate #e1e4)
