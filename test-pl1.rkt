#lang s-exp "pl1.rkt"

(define (sum-n-flips n)
  (if (zero? n)
      0
      (+ (flip) (sum-n-flips (sub1 n)))))

#|
;; To inspect, do:
(apply/reset sum-n-flips 10)
(apply/reset sum-n-flips 10) ;; should produce same answer, because all flips reused
(apply/reset sum-n-flips 12) ;; should be >= last answer, because first 10 flips reused
last-log
current-log
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

;; map is bad; causes context collisions
(define (bad-map n)
  (apply + (map (lambda (n) (flip)) (for/list ([i n]) i))))

;; for/* is bad; causes context collisions
(define (bad-for n)
  (apply + (for/list ([i n]) (flip))))

;;(apply/reset bad-map 10)
;;(apply/reset bad-for 10)

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

