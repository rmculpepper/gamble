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
  (or (density-?l d) (log (density-?n d))))

(define (density-complete d)
  (match d
    [(density (? real?) (? real?) _) d]
    [(density el ll ddim)
     (density (or el (exp ll)) (or ll (log el)) ddim)]))

(define zero-density (density 0 -inf.0 0))

(define (density-zero? d)
  (match-define (density el ll _) d)
  (if el (zero? el) (= ll -inf.0)))

(define (density+ d1 d2)
  (match-define (density el1 ll1 ddim1) d1)
  (match-define (density el2 ll2 ddim2) d2)
  (cond [(density-zero? d1) d2]
        [(density-zero? d2) d1]
        [(= ddim1 ddim2)
         (cond [(and el1 el2)
                (density (+ el1 el2) (and ll1 ll2 (logspace+ ll1 ll2)) ddim1)]
               [else
                (density #f (logspace+ (or ll1 (log el1)) (or ll2 (log el2))) ddim1)])]
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
                  (define ll* (and accl ll (logspace+ accl (+ ll (log w)))))
                  (values nl* ll* ddim)]
                 [else
                  (define ll* (logspace+ (or accl (log accn)) (+ (or ll (log nl)) (log w))))
                  (values #f ll* ddim)])]
          [(< ddim accddim)
           (values (and nl (* w nl)) (and ll (+ ll (log w))) ddim)]
          [else
           (values accn accl accddim)])))

(define (density* d1 d2)
  (match-define (density el1 ll1 ddim1) d1)
  (match-define (density el2 ll2 ddim2) d2)
  (cond [(and el1 el2)
         (density (* el1 el2) (and ll1 ll2 (+ ll1 ll2)) (+ ddim1 ddim2))]
        [else
         (density #f (+ (or ll1 (log el1)) (or ll2 (log el2))) (+ ddim1 ddim2))]))
