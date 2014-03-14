;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         data/order
         "pairingheap.rkt"
         "prob-hooks.rkt"
         "util.rkt")
(provide (all-defined-out))

;; A (EnumTree A) is one of
;; - (only A)
;; - (split (listof (cons Prob (-> (EnumTree A)))))
(struct only (answer))
(struct split (subs))

;; explore : (EnumTree A) (A -> Boolean) (A -> B) Prob
;;        -> hash[B => Prob] Prob Prob
;; Limit means: explore until potential accepted dist is < limit unaccounted for,
;; ie, unexplored < limit * accepted. Thus, limit is nonlocal; use BFS.
(define (explore tree pred project limit)
  (define prob-unexplored 1) ;; prob of all unexplored paths
  (define prob-accepted 0) ;; prob of all accepted paths
  (define table (hash)) ;; hash[B => Prob], probs are not normalized

  ;; h: heap[(cons Prob (-> (EnumTree A)))]
  (define (entry->=? x y)
    (>= (car x) (car y)))
  (define h (heap entry->=?)) ;; "min-heap", but want max prob, so >=

  ;; add : A Prob table prob-unexplored prob-accepted
  ;;    -> (values table prob-unexplored prob-accepted)
  (define (add a p table prob-unexplored prob-accepted)
    ;; (eprintf "- consider add A=~s\n" a)
    (cond [(positive? p)
           (let ([prob-unexplored (- prob-unexplored p)])
             (cond [(pred a)
                    (let* ([b (project a)]
                           [prob-accepted (+ p prob-accepted)]
                           [table (hash-set table b (+ p (hash-ref table b 0)))])
                      ;; (eprintf "- add B=~s\n" b)
                      (values table prob-unexplored prob-accepted))]
                   [else
                    (values table prob-unexplored prob-accepted)]))]
          [else
           (when #t ;; (verbose?)
             (eprintf "WARNING: bad prob ~s for ~s\n" p (project a)))
           (values table prob-unexplored prob-accepted)]))

  ;; traverse-tree : (EnumTree A) Prob ... -> (values h table prob-unexplored prob-accepted)
  (define (traverse-tree et prob-of-tree h table prob-unexplored prob-accepted)
    (match et
      [(only a)
       (let-values ([(table prob-unexplored prob-accepted)
                     (add a prob-of-tree table prob-unexplored prob-accepted)])
         (values h table prob-unexplored prob-accepted))]
      [(split subs)
       (for/fold ([h h] [table table] [prob-unexplored prob-unexplored] [prob-accepted prob-accepted])
           ([sub (in-list subs)])
         (match sub
           [(cons p lt)
            (let ([p* (* prob-of-tree p)])
              (cond [(positive? p)
                     (values (heap-insert h (cons p* lt)) table prob-unexplored prob-accepted)]
                    [else
                     (when (verbose?)
                       (eprintf "WARNING: bad prob ~s in path\n" p*))
                     (values h table prob-unexplored prob-accepted)]))]))]))

  ;; heap-loop : ... -> ...
  (define (heap-loop h table prob-unexplored prob-accepted)
    (cond [(and limit (< prob-unexplored (* limit prob-accepted)))
           ;; Done!
           (when (positive? (heap-count h))
             (when (verbose?)
               (eprintf "stopping with ~s unexplored path(s)\n" (heap-count h))))
           (done h table prob-unexplored prob-accepted)]
          [(zero? (heap-count h))
           ;; explored all paths
           (done h table prob-unexplored prob-accepted)]
          [else
           (let* ([sub (heap-find-min/max h)]
                  [h (heap-delete-min/max h)])
             ;; (eprintf "- picked ~s\n" sub)
             (let-values ([(h table prob-unexplored prob-accepted)
                           (traverse-tree ((cdr sub)) (car sub)
                                          h table prob-unexplored prob-accepted)])
               (heap-loop h table prob-unexplored prob-accepted)))]))

  (define (done h table prob-unexplored prob-accepted)
    (values table prob-unexplored prob-accepted))

  ;; FIXME: prob-unexplored can drop below zero because of floating-point
  ;; inaccuracy. Any remedy?
  (let-values ([(h table prob-unexplored prob-accepted)
                (traverse-tree tree 1 h table prob-unexplored prob-accepted)])
    (heap-loop h table prob-unexplored prob-accepted)))

(define (tabulate table prob-accepted #:normalize? [normalize? #t])
  (define entries
    (for/list ([(val prob) (in-hash table)])
      (list val (if normalize? (/ prob prob-accepted) prob))))
  (sort entries (order-<? datum-order)))

;; ============================================================

(define (dist->vals+probs tag dist)
  (let ([enum (dist-enum dist)])
    (cond [(eq? enum 'lazy)
           (lazy-dist->vals+probs tag dist 0)]
          [(eq? enum #f)
           (error 'ERP "cannot enumerate non-integer distribution: ~s" tag)]
          [else ;; positive integer
           (define-values (vals probs)
             (for/lists (vals probs) ([i (in-range enum)])
               (values i (dist-pdf dist i))))
           (values vals probs #f)])))

(define TAKE 10)

(define (lazy-dist->vals+probs tag dist start)
  (match tag
    [(list* 'continue-special _ (and base-tag (list* 'geometric _)))
     ;; Special case: for geometric, no need to resume
     (lazy-dist->vals+probs base-tag dist 0)]
    [(list* 'continue-special start base-tag)
     (lazy-dist->vals+probs base-tag dist start)]
    [_
     (define scale (- 1 (dist-cdf dist (sub1 start))))
     (define-values (vals probs)
       (for/lists (vals probs) ([i (in-range start (+ start TAKE))])
         (values i (/ (dist-pdf dist i) scale))))
     (values vals probs
             (cons (/ (- 1 (dist-cdf dist (+ start TAKE -1))) scale)
                   `(continue-special ,(+ start TAKE) . ,tag)))]))
