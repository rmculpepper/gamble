;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/match
         "../dist/base.rkt"
         "../dist/discrete.rkt"
         "../util/debug.rkt"
         "../util/real.rkt"
        "lazy-tree.rkt"
         "pairingheap.rkt")
(provide enumerate
         enumerate*)

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate (~or (~optional (~seq #:limit limit:expr))
                     (~optional (~seq #:normalize? normalize?)))
                ...
                def:expr ... result:expr)
     #'(enumerate*
        (lambda () def ... result)
        (~? limit #f)
        (~? normalize? #t))]))


;; == Overview ==
;;
;; Enumeration using lazy tree of probability-labeled possibilities,
;; based on delimited continuations (only for discrete ERPs).
;;
;; For enumeration to work:
;;
;;  - All paths must either terminate or evaluate infinitely many ERPS.
;;    (and every ERP must generate at least two values with nonzero prob.)
;;
;;  - The predicate must accept a nonempty set of paths.
;;
;; Without knowing structure of paths and conditioning predicate, can't
;; make smart distinctions between incomplete paths.
;;
;; Would be nice if we could tell whether a path was viable or not wrt
;; condition. Seems like it would require drastic changes to model of
;; computation (eg, like symbolic execution) to support non-trivial
;; conditions. Would that be a profitable place to spend effort?

;; ============================================================

;; enumerate* : (-> A) etc -> (Listof (List A Prob))
(define (enumerate* thunk limit normalize?)
  (vprintf "Enumerating\n")
  (define tree (reify-tree thunk))
  (define table (explore tree limit))
  (when (and normalize? (zero? (hash-count table)))
    (error 'enumerate "condition accepted no paths"))
  (alist->discrete-dist (hash-map table cons)
                        #:normalize? normalize?))

;; ----------------------------------------

;; FIXME: complement limit instead of p!
(define (logspace-compl a)
  (log (- 1.0 (exp a))))

;; An ExpST is (expst Prob Prob Hash[A => Prob] Nat)
;; where Prob is Real or Real+ depending on logspace?,
;; and dim is density dimension of every entry in table.
(struct expst (explored accepted table table-ddim) #:transparent)

(define (expst->logspace st)
  (match-define (expst explored accepted table table-ddim) st)
  (expst (log (exact->inexact explored)) (log (exact->inexact accepted))
         (table->logspace table) table-ddim))

(define (table->logspace table)
  (for/hash ([(k v) (in-hash table)])
    (values k (log (exact->inexact v)))))

;; A (HeapEntry A) is (list* Prob Nat (-> (EnumTree A))).
(define (entry->=? x y) (>= (car x) (car y)))

(define (heap->logspace h)
  (for/fold ([new-h (heap entry->=?)])
            ([entry (in-list (heap->list h))])
    (heap-insert new-h (cons (log (car entry)) (cdr entry)))))

;; explore : (EnumTree A) (U #f Real>0) -> Hash[A => Real+]
;; * Starts with logspace? = #f, but switches to logspace if an inexact
;;   density is ever seen.
;; * Limit means: explore until potential accepted dist is < limit unaccounted for,
;;   ie, unexplored < limit * accepted. Thus, limit is nonlocal; use BFS.
;;   Limit is always realspace, not logspace. If no limit, prob-{explored,accepted}
;;   can be meaningless. Note: Cannot observe continuous dist if limit given.
(define (explore tree limit)
  (define initial-st (expst 0 0 '#hash() (if limit 0 +inf.0)))
  (define initial-heap
    ;; lib provides "min-heap", but want max prob, so use >= comparison
    (heap entry->=?))
  (define initial-ddim 0)
  (define initial-p 1)
  (explore* limit #f initial-heap initial-st tree initial-ddim initial-p))

;; explore* : (EnumTree A) (U #f Real>0) ExpSt Heap Nat -> Hash[A => Real+]
;; - st   : ExpST, rep of Prob depends on logspace?
;; - h    : heap[(cons Prob (-> (EnumTree A)))]
;; - ddim : Nat, density dimension
(define (explore* limit logspace? h st tree tree-ddim tree-p)
  ;; type Prob = Real
  ;; The representation of probabilities in this code is determined by
  ;; the logspace? argument.  If logspace? is true, then values
  ;; represent log of prob; if false, values represent prob directly.
  (define (->p a) ;; inexact to avoid error on exact 0
    (when (negative? a) (error '->p "negative: ~e" a))
    (if logspace? (log (exact->inexact a)) a))
  (define (p->real a)
    (if logspace? (exp a) a))
  (define (p+ a b)
    (if logspace? (logspace+ a b) (+ a b)))
  (define (pcompl a)
    (if logspace? (logspace-compl a) (- 1 a)))
  (define (p* a b)
    (if logspace? (+ a b) (* a b)))
  (define pzero (->p 0))

  (define (switch-to-logspace h st tree tree-ddim tree-p)
    (explore* limit #t (heap->logspace h) (expst->logspace st) tree tree-ddim (log tree-p)))

  ;; add : A Prob Nat ExpST -> ExpST
  ;; Don't bother to watch for underflow, since we switch to logspace
  ;; for inexact numbers, and logspace is hard to underflow.
  (define (add a p ddim st)
    ;; (eprintf "! add: ~v w/p prob ~s\n" a p)
    (match-define (expst prob-explored prob-accepted table table-ddim) st)
    (cond [(> ddim table-ddim) ;; ie, infinitesimally likely
           ;; (eprintf "! add: infinitely unlikely (got ~s, table has ~s)\n" ddim table-ddim)
           st]
          [(< ddim table-ddim) ;; ie, infinitely more likely than table
           ;; Can only happen if continuous obs allowed; therefore,
           ;; explored and accepted probs don't really matter.
           ;; (eprintf "! add: infinitely likely (got ~s, table has ~s)\n" ddim table-ddim)
           (expst p p (hash a p) ddim)]
          [else
           (let* ([prob-explored (if limit (p+ prob-explored p) 0)]
                  [prob-accepted (if limit (p+ prob-accepted p) 0)]
                  [table (hash-set table a (p+ p (hash-ref table a pzero)))])
             (expst prob-explored prob-accepted table table-ddim))]))

  ;; add-explored : ExpST Prob -> ExpST
  (define (add-explored st p)
    (cond [limit
           (match-define (expst prob-explored prob-accepted table table-ddim) st)
           (let ([prob-explored (p+ prob-explored p)])
             (expst prob-explored prob-accepted table table-ddim))]
          [else st]))

  ;; traverse-tree : (EnumTree A) Prob Heap Nat ExpSt -> Answer
  (define (traverse-tree h st tree tree-ddim tree-p)
    (match tree
      [(only a)
       (heap-loop h (add a tree-p tree-ddim st))]
      [(? split? et)
       (define subs (split->subtrees tree logspace?))
       (cond [(and (not logspace?) (for/or ([sub (in-list subs)]) (inexact? (car sub))))
              ;; Switch to logspace!
              (switch-to-logspace h st tree tree-ddim tree-p)]
             [else
              (define h*
                (for/fold ([h h]) ([sub (in-list subs)])
                  (match-define (cons p lt) sub)
                  (let ([p (p* tree-p p)])
                    (heap-insert h (list* p tree-ddim lt)))))
              (heap-loop h* st)])]
      [(weight dist val scale k)
       (define-values (p ddim) (dist-density dist val logspace?))
       (when (and limit (not (zero? ddim)))
         ;; limit applies to mass; it can't handle densities
         (error 'enumerate "cannot use both #:limit and observe with continuous distribution"))
       (let ([p (dist-pdf dist val logspace?)])
         (cond [(and (not logspace?) (inexact? p))
                ;; Switch to logspace!
                (switch-to-logspace h st tree tree-ddim tree-p)]
               [else
                (let* ([tree-ddim (+ tree-ddim ddim)]
                       [scale (if (zero? ddim) 1 scale)] ;; scale does not apply to mass
                       [scaled-p (p* (->p scale) p)]
                       [tree-p (p* tree-p scaled-p)]
                       [st (add-explored st (p* tree-p (pcompl scaled-p)))])
                  (traverse-tree h st (k) tree-ddim tree-p))]))]
      [(failed reason)
       (heap-loop h (add-explored st tree-p))]))

  ;; heap-loop : Heap ExpST -> Answer
  (define (heap-loop h st)
    (match-define (expst prob-explored prob-accepted table table-ddim) st)
    (cond [;;     unexplored < limit * accepted
           ;; ie, (1 - explored) < limit * accepted
           ;; ie, 1 < limit * accepted + explored
           ;; ie, limit * accepted + explored > 1
           (and limit (> (p+ prob-explored (p* (->p limit) prob-accepted)) (->p 1)))
           ;; Done!
           (unless (heap-empty? h)
             (vprintf "stopping with ~s unexplored path(s)\n" (heap-count h)))
           (done table)]
          [(heap-empty? h)
           ;; explored all paths
           (done table)]
          [else
           (let* ([sub (heap-find-min/max h)]
                  [h (heap-delete-min/max h)])
             (match-define (list* prob ddim k) sub)
             (traverse-tree h st (k) ddim prob))]))

  ;; done : Hash Real Real -> Hash[A => Real+]
  (define (done table)
    (cond [logspace?
           (define m (for/fold ([m -inf.0]) ([(k v) (in-hash table)]) (max m v)))
           ;; FIXME: scale max to 1 first
           (for/hash ([(k v) (in-hash table)])
             (values k (exp (- v m))))]
          [else table]))

  (traverse-tree h st tree tree-ddim tree-p))

#|
Maybe new abstraction, alternative to sampler: enumerator. Produces
stream (finite or infinite) of weighted partitions of
distribution. (Note: values may may repeat, just add probs.) Can't
normalize unless stream is finite or we truncate it.
|#
