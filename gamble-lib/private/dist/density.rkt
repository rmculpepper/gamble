;; Copyright 2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         "../util/real.rkt")
(provide (all-defined-out))

;; Density = (density NNReal/#f LogReal/#f Nat)
;; Either the realspace or logspace component must be present.

(struct density (?n ?l ddim) #:prefab)

(define (make-density el ll ddim)
  (cond [(if el (zero? el) (= ll -inf.0)) zero-density]
        [else (density el ll ddim)]))

(define (density-n d)
  (or (density-?n d) (exp (density-?l d))))
(define (density-l d)
  (or (density-?l d) (ilog (density-?n d))))

(define (density-proper d)
  (if (density-zero? d) zero-density d))
(define (density-complete d)
  (match d
    [(density (? real?) (? real?) _) d]
    [(density el ll ddim)
     (density (or el (exp ll)) (or ll (ilog el)) ddim)]))

(define zero-density (density 0 -inf.0 +inf.0))

(define (density-zero? d)
  (match-define (density el ll _) d)
  (if el (zero? el) (= ll -inf.0)))

(define (density<? d1 d2)
  (let ([d1 (density-proper d1)] [d2 (density-proper d2)])
    (match-define (density n1 l1 ddim1) d1)
    (match-define (density n2 l2 ddim2) d2)
    (or (> ddim1 ddim2)
        (and (= ddim1 ddim2)
             (cond [(and l1 l2) (< l1 l2)]
                   [(and n1 n2) (< n1 n2)]
                   [else (< (or l1 (ilog n1)) (or l2 (ilog n2)))])))))

(define (density<=? d1 d2)
  (let ([d1 (density-proper d1)] [d2 (density-proper d2)])
    (match-define (density n1 l1 ddim1) d1)
    (match-define (density n2 l2 ddim2) d2)
    (or (> ddim1 ddim2)
        (and (= ddim1 ddim2)
             (cond [(and l1 l2) (<= l1 l2)]
                   [(and n1 n2) (<= n1 n2)]
                   [else (<= (or l1 (ilog n1)) (or l2 (ilog n2)))])))))

(define (density+ d1 d2 #:exact? [ex? #f])
  (match-define (density el1 ll1 ddim1) d1)
  (match-define (density el2 ll2 ddim2) d2)
  (cond [(density-zero? d1) d2]
        [(density-zero? d2) d1]
        [(= ddim1 ddim2)
         (cond [(and el1 el2 (if ex? (and (exact? el1) (exact? el2)) #t))
                (density (+ el1 el2) (and ll1 ll2 (logspace+ ll1 ll2)) ddim1)]
               [else
                (density #f (logspace+ (or ll1 (ilog el1)) (or ll2 (ilog el2))) ddim1)])]
        [(< ddim1 ddim2) d1]
        [else d2]))

(define (density-sum ds [ws (in-cycle '(1))])
  (for/fold ([accn 0] [accl -inf.0] [accddim +inf.0]
             #:result (make-density accn accl accddim))
            ([d ds] [w ws] #:when (not (density-zero? d)))
    (match-define (density nl ll ddim) d)
    (cond [(= ddim accddim)
           (cond [(and accn nl)
                  (define nl* (+ accn (* w nl)))
                  (define ll* (and accl ll (logspace+ accl (+ ll (ilog w)))))
                  (values nl* ll* ddim)]
                 [else
                  (define ll* (logspace+ (or accl (ilog accn)) (+ (or ll (ilog nl)) (ilog w))))
                  (values #f ll* ddim)])]
          [(< ddim accddim)
           (values (and nl (* w nl)) (and ll (+ ll (ilog w))) ddim)]
          [else
           (values accn accl accddim)])))

(define (density* d1 d2)
  (match-define (density el1 ll1 ddim1) d1)
  (match-define (density el2 ll2 ddim2) d2)
  (cond [(and el1 el2)
         (density (* el1 el2) (and ll1 ll2 (+ ll1 ll2)) (+ ddim1 ddim2))]
        [else
         (density #f (+ (or ll1 (ilog el1)) (or ll2 (ilog el2))) (+ ddim1 ddim2))]))

(define (density-product ds)
  (for/fold ([accn 1] [accl 0.0] [accddim 0]
             #:result (make-density accn accl accddim))
            ([d ds])
    (match-define (density nl ll ddim) d)
    (cond [(and accn nl)
           (values (* accn nl) (and accl ll (+ accl ll)) (+ accddim ddim))]
          [else
           (values #f (+ (or accl (ilog accn)) (or ll (ilog nl))) (+ accddim ddim))])))

(define (ilog x) (log (exact->inexact x))) ;; avoid error on exact 0
