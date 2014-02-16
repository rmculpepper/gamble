#lang s-exp "pl3.rkt"
(require "private/viz.rkt")

;; Unlike pl1, doesn't print instrumented applications.
(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (flip) (sum-n-flips (sub1 n)))))

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
        (loop (sub1 n) (+ acc (flip))))))

(define s-mh* (mh-sampler (sum-n-flips* 10)))

;;(hist (repeat (lambda () (sum-n-flips 10)) 1000))
;;(hist (repeat s-mh 1000))
;;(hist (repeat s-mh* 1000))

;; ----

;; ** interaction with untranslated code causes problems

;; map is still bad; causes context collisions
(define (bad-map n)
  (apply + (map (lambda (n) (flip)) (for/list ([i n]) i))))
#|
(parameterize ((verbose? #t))
    ((mh-sampler (bad-map 10))))
|#

;; BUT for/* is now okay; annotator rewrites its applications
(define (ok-for n)
  (apply + (for/list ([i n]) (flip))))
#|
(parameterize ((verbose? #t))
    ((mh-sampler (ok-for 10))))
|#

;; ----

;; ** mem

(define (make-mem-mh n)
  (mh-sampler
   ;; Note: need to call mem within mh-sampler, not outside
   (define get-the-flip (mem (lambda (n) (flip))))
   (for/sum ([i n]) (get-the-flip (modulo i 5)))))
#|
(parameterize ((verbose? #t))
  ((make-mem-mh 10)))
;; first 5 NEW, second 5 MEMOIZED
|#

;; ----

(define (geom p)
  (if (zero? (flip p)) 0 (add1 (geom p))))
#|
(enumerate (geom 1/2) #:limit 1e-3)
(enumerate (geom 1/2) #:limit 1e-9 #:normalize? #f)
|#

#|
;; enumeration and mem
(enumerate
 (define f (mem (lambda (n) (flip))))
 (list (f 1) (f 2) (f 1) (f 2)))
;; Should produce 4 values, each with prob 0.25.

(enumerate
 (define A (flip))
 (define B (flip))
 A
 #:when (> (+ A B) 0))

(enumerate
 (define A (geom 1/2))
 (define B (* 1/2 (geom 1/2)))
 A
 #:when (> A B))

(enumerate 
   (define A (geom 1/2))
   A
   #:when (< 10 A 20))
|#

(enumerate
 (define (drop-coin?) (flip 0.9))
 (define (drunk-flips n)
   (cond [(zero? n)
          #t]
         [(drop-coin?)
          'failed]
         [else
          (define x (flip))
          ;;(printf "flip ~s was ~s\n" n x)
          (and x (drunk-flips (sub1 n)))]))
 (define A (drunk-flips 10))
 (eq? A #t)
 #:when (not (eq? A 'failed))
 #:normalize? #f
 ;; Need to increase limit to detect #t case:
 #:limit 1e-20)
