#lang s-exp "pl3.rkt"

;; Unlike pl1, doesn't print instrumented applications.

(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (flip) (sum-n-flips (sub1 n)))))

#|
;; To inspect, enter the following commands at the repl:
(apply/reset sum-n-flips 10)
(apply/reset sum-n-flips 10) ;; should produce same answer, because all flips reused
(apply/reset sum-n-flips 12) ;; should be >= last answer, because first 10 flips reused
(print-db last-db)
(print-db current-db)


;; During evaluation, prints lines like
;; - NEW flip: <address>
;; or 
;; - REUSED flip: <address>
;; to indicate new or reused ERP choices.
|#

;; tail-recursive version
(define (sum-n-flips* n)
  (let loop ([n n] [acc 0])
    (if (zero? n)
        acc
        (loop (sub1 n) (+ acc (flip))))))

;;(apply/reset sum-n-flips 10)
;;(apply/reset sum-n-flips* 10)

;; ----

;; ** interaction with untranslated code causes problems

;; map is still bad; causes context collisions
(define (bad-map n)
  (apply + (map (lambda (n) (flip)) (for/list ([i n]) i))))

;; BUT for/* is now okay; annotator rewrites its applications
(define (ok-for n)
  (apply + (for/list ([i n]) (flip))))

;;(apply/reset bad-map 10)
;;(apply/reset ok-for 10)

;; ----

;; ** mem

(define get-the-flip (mem (lambda (n) (flip))))

;; for/*, map, etc ok here only because calling memoized function!
(define (get-the-flips n)
  (for/sum ([i n]) (get-the-flip (modulo i 5))))
(define (get-the-flips* n)
  (apply + (map get-the-flip (for/list ([i n]) (modulo i 5)))))

;;(apply/reset get-the-flips 15)
;;(apply/reset get-the-flips* 15)

;; ----

(define (geom p)
    (if (zero? (flip p)) 0 (add1 (geom p))))

(enumerate-possibilities
 (lambda () (geom 1/2))
 #:limit 1e-6) ;; prune away any path that has prob < 1e-6
