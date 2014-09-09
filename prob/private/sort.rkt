;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/vector)
(provide combine-duplicates
         heap-sort!
         value-cmp
         value<
         value<=)

;; ============================================================
;; Sort and de-duplicate

;; Does not modify vs or ws.
(define (combine-duplicates vs ws)
  (cond [(sorted-nodups? vs) (values vs ws)]
        [else (combine-duplicates* (vector-copy vs) (vector-copy ws))]))

;; Modifies vs, ws.
(define (combine-duplicates* vs ws)
  (heap-sort! vs ws value<=)
  ;; visit : Nat Nat Any -> Nat
  ;; Returns number of distinct entries.
  ;; Process element at i and write to either
  ;;   - prev, if vs[i] = vprev, or
  ;;   - prev+1, otherwise
  (define (visit i prev vprev)
    (cond [(>= i (vector-length vs))
           (add1 prev)]
          [else
           (define vi (vector-ref vs i))
           (define wi (vector-ref ws i))
           (cond [(equal? vi vprev)
                  (vector-set! ws prev (+ (vector-ref ws prev) wi))
                  (visit (add1 i) prev vprev)]
                 [(positive? wi)
                  (unless (= i (add1 prev)) ;; avoid redundant vector-set!s
                    (vector-set! vs (add1 prev) vi)
                    (vector-set! ws (add1 prev) wi))
                  (visit (add1 i) (add1 prev) vi)]
                 [else
                  (visit (add1 i) prev vprev)])]))
  (define len* (visit 1 0 (vector-ref vs 0)))
  (if (= len* (vector-length vs))
      (values vs ws)
      (values (vector-copy vs 0 len*) (vector-copy ws 0 len*))))

(define (sorted-nodups? vs)
  (for/and ([i (in-range 1 (vector-length vs))])
    (define prev (vector-ref vs (sub1 i)))
    (define curr (vector-ref vs i))
    (value< prev curr)))

;; ============================================================
;; Heapsort

;; Copied and modified from data/heap.
;; Adapted to permute 2nd vector like 1st.

;; A VT is a binary tree represented as a vector.
(define (vt-root) 0)
(define (vt-parent n) (quotient (sub1 n) 2))
(define (vt-leftchild n) (+ (* n 2) 1))
(define (vt-rightchild n) (+ (* n 2) 2))
(define (vt-root? n) (zero? n))
(define (vt-leftchild? n) (odd? n))
(define (vt-rightchild? n) (even? n))
(define (vt-ref v i) (and v (vector-ref v i)))
(define (vt-set! v i x) (when v (vector-set! v i x)))
(define (vt-swap! v i j)
  (when v
    (let ([tmp (vector-ref v i)])
      (vector-set! v i (vector-ref v j))
      (vector-set! v j tmp))))

;; heap-sort! : Vector (U Vector #f) (A A -> Boolean) -> Void
(define (heap-sort! v w <=?)
  (define size (vector-length v))
  (for ([n (in-range (sub1 size) -1 -1)])
    (heapify-down <=? v w n size))
  (for ([last (in-range (sub1 size) 0 -1)])
    (vt-swap! v 0 last)
    (vt-swap! w 0 last)
    (heapify-down <=? v w 0 last))
  ;; min-heap produces descending order; reverse to get ascending
  (for ([n (in-range (quotient size 2))])
    (vt-swap! v n (- size 1 n))
    (vt-swap! w n (- size 1 n))))

(define (heapify-down <=? v w n size)
  (let ([left (vt-leftchild n)]
        [right (vt-rightchild n)])
    (when (< left size)
      (define child
        (if (< right size)
            (if (<=? (vector-ref v left) (vector-ref v right))
                left
                right)
            left))
      (unless (<=? (vector-ref v n) (vector-ref v child))
        (vt-swap! v n child)
        (vt-swap! w n child)
        (heapify-down <=? v w child size)))))

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
