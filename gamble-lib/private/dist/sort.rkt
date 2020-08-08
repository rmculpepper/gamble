;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(provide value-cmp
         value<
         value<=)

;; ============================================================
;; Orders over general Racket values

;; Copied and modified from Racket's data/order.

#|
value-cmp:
    exact real
  < inexact real (+nan.0 > +inf.0)
  < complex
  < string
  < bytes
  < keyword
  < symbol
  < bool
  < char
  < path
  < null
  < pair
  < vector
  < box
  < prefab-struct
  < fully-transparent-struct
  < other values

-- NOTE: restriction to reals NOT EQUIV to <,= (separates exact, inexact)
|#

(define EQUAL-DEPTH-LIMIT 4)

(define (atomic? x)
  (or (number? x)
      (string? x)
      (bytes? x)
      (keyword? x)
      (symbol? x)
      (boolean? x)
      (char? x)
      (path? x)
      (null? x)))

(define (value< x y)
  (case (value-cmp x y)
    [(<) #t]
    [else #f]))

(define (value<= x y)
  (case (value-cmp x y)
    [(< =) #t]
    [else #f]))

;; ----

;; value-cmp : Any Any -> (U Ordering ???)  
;; If x or y is cyclic, naive recursive cmp could diverge. So use
;; equal? (does cycle detection).
(define (value-cmp x y)
  (cond [(atomic-cmp x y) => values]
        [else (gen-cmp x y 0)]))

;; atomic-cmp : Any Any -> (U Ordering #f)
;; Try comparing as atomic values; returns #f if neither value is atomic.
;; (No cycles possible.)
(define (atomic-cmp x y)
  (cond [(eq? x y) '=]
        #|
        [(T? x) ...]
         ;; at this point, Type(x) > T
        [(T? y)
         ;; Type(x) > T = Type(y), so:
         '>]
        Assumes arguments are legal.
        |#
        [(real? x)
         (if (real? y)
             ;; exact < inexact
             (cond [(and (exact? x) (exact? y))
                    (cmp* < = x y)]
                   [(exact? x) ;; inexact y
                    '<]
                   [(exact? y) ;; inexact x
                    '>]
                   [(and (eqv? x +nan.0) (eqv? y +nan.0))
                    '=]
                   [(eqv? x +nan.0)
                    '>]
                   [(eqv? y +nan.0)
                    '<]
                   [else ;; inexact x, inexact y
                    (cmp* < = x y)])
             '<)]
        [(real? y) '>]
        [(complex? x)
         (if (complex? y)
             (lexico (atomic-cmp (real-part x) (real-part y))
                     (atomic-cmp (imag-part x) (imag-part y)))
             '<)]
        [(complex? y) '>]
        [(string? x)
         (if (string? y)
             (cmp* string<? string=? x y)
             '<)]
        [(string? y) '>]
        [(bytes? x)
         (if (bytes? y)
             (cmp* bytes<? bytes=? x y)
             '<)]
        [(bytes? y) '>]
        [(keyword? x)
         (if (keyword? y)
             (cmp* keyword<? eq? x y)
             '<)]
        [(keyword? y) '>]
        [(symbol? x)
         (if (symbol? y)
             (cmp* symbol<? eq? x y)
             '<)]
        [(symbol? y) '>]
        [(boolean? x)
         (if (boolean? y)
             (cond [(eq? x y) '=]
                   [y '<]
                   [else '>])
             '<)]
        [(boolean? y) '>]
        [(char? x)
         (if (char? y)
             (cmp* char<? char=? x y)
             '<)]
        [(char? y)
         '>]
        [(path-for-some-system? x)
         (if (path-for-some-system? y)
             (cmp* bytes<? bytes=? (path->bytes x) (path->bytes y))
             '<)]
        [(path-for-some-system? y)
         '>]
        [(null? x)
         (if (null? y)
             '=
             '<)]
        [(null? y) '>]
        [else #f]))

;; May diverge on cyclic data.
(define (gen-cmp x y depth)
  (define (recur x* y*)
    (gen-cmp x* y* (add1 depth)))
  (cond [(atomic-cmp x y) => values]
        ;; If deep enough, start checking equal? to detect cycles.
        [(and (> depth EQUAL-DEPTH-LIMIT) (equal? x y)) '=]
        ;; Non-atomic cases
        [(pair? x)
         (if (pair? y)
             (lexico (recur (car x) (car y)) (recur (cdr x) (cdr y)))
             '<)]
        [(pair? y) '>]
        [(vector? x)
         (if (vector? y)
             (vector-cmp x y 0 (add1 depth))
             '<)]
        [(vector? y) '>]
        [(box? x)
         (if (box? y)
             (recur (unbox x) (unbox y))
             '<)]
        [(box? y) '>]
        [(prefab-struct-key x)
         (if (prefab-struct-key y)
             (lexico (recur (prefab-struct-key x) (prefab-struct-key y))
                     ;; FIXME: use struct-ref to avoid allocation?
                     (vector-cmp (struct->vector x) (struct->vector y) 1 (add1 depth)))
             '<)]
        [(prefab-struct-key y)
         '>]
        [(fully-transparent-struct-type x)
         => (lambda (xtype)
              (cond [(fully-transparent-struct-type y)
                     => (lambda (ytype)
                          ;; could also do another lexico with object-name first
                          (lexico (object-cmp xtype ytype)
                                  ;; FIXME: use struct-ref to avoid allocation?
                                  (vector-cmp (struct->vector x) (struct->vector y)
                                              1 (add1 depth))))]
                    [else '<]))]
        [(fully-transparent-struct-type y)
         '>]
        ;; Unknown structure; impose order.
        [else (object-cmp x y)]))

(define-syntax-rule (cmp* <? =? xe ye)
  (let ([x xe] [y ye])
    (if (=? x y) '= (if (<? x y) '< '>))))

(define-syntax-rule (lexico c1 c2)
  (case c1
    ((<) '<)
    ((=) c2)
    ((>) '>)))

(define (vector-cmp x y i depth)
  (cond [(< i (vector-length x))
         (if (< i (vector-length y))
             (lexico (gen-cmp (vector-ref x i) (vector-ref y i) depth)
                     (vector-cmp x y (add1 i) depth))
             '>)]
        [(< i (vector-length y))
         '<]
        [else '=]))

;; fully-transparent-struct-type : any -> struct-type or #f
(define (fully-transparent-struct-type x)
  (parameterize ((current-inspector weak-inspector))
    (let-values ([(x-type x-skipped?) (struct-info x)])
      (and (not x-skipped?) x-type))))

;; weak inspector controls no struct types;
;; so if it can inspect, must be transparent
(define weak-inspector (make-inspector))

;; Impose an arbitrary (but consistent) ordering on objects. Use
;; equal-hash-code for common fast path. Fall back to table when
;; comparing objects with same hash code but not equal. That should
;; hopefully be rare.
(define object-order-table (make-weak-hash))
(define object-order-next 0)
(define (object-cmp x y)
  (lexico
   (cmp* < = (equal-hash-code x) (equal-hash-code y))
   (if (equal? x y)
       '=
       (let ([xi (hash-ref object-order-table x #f)]
             [yi (hash-ref object-order-table y #f)])
         (cond [(and xi yi)
                (if (< xi yi) '< '>)]
               [xi '<]
               [yi '>]
               [else ;; neither one is in table; we only need to add one
                (hash-set! object-order-table x object-order-next)
                (set! object-order-next (add1 object-order-next))
                '<])))))
