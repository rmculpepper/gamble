;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/match
         "../dist/base.rkt"
         "../dist/discrete.rkt"
         "../dist/density.rkt"
         "../util/debug.rkt"
         "../util/real.rkt"
        "lazy-tree.rkt"
         "pairingheap.rkt")
(provide enumerate
         enumerate*)

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate def:expr ... result:expr)
     #'(enumerate* (lambda () def ... result))]))

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

;; enumerate* : (-> A) -> (DiscreteDist A)
(define (enumerate* thunk)
  (vprintf "Enumerating\n")
  (define tree (reify-tree thunk))
  (define tbl (explore tree))
  (table->discrete-dist tbl))

;; ----------------------------------------

;; explore : (EnumTree A) -> Hash[A => Real+]
(define (explore tree)
  (define initial-table (table (hash) +inf.0))
  ;; heap lib provides "min-heap", but want max prob, so use >= comparison
  (define initial-heap (heap entry->=?))
  (explore/tree initial-heap initial-table tree (density 1 0 0)))

;; A Table[X] is (table Hash[X => Density] Nat), where ddim is the ddim
;; of all entries in the hash.
(struct table (h ddim) #:transparent)

;; table->discrete-dist : Table[X] -> DiscreteDist[X]
(define (table->discrete-dist tbl)
  (match-define (table h _) tbl)
  (for/discrete-dist ([(v dn) (in-hash h)])
    (values v (density-n dn))))

;; table-add : Table[X] X Density -> Table[X]
(define (table-add tbl v dn)
  (let ([dn (density-proper dn)])
    (match-define (table th tddim) tbl)
    (define ddim (density-ddim dn))
    (cond [(< ddim tddim)
           (table (hash v dn) ddim)]
          [(= ddim tddim)
           (cond [(hash-ref th v #f)
                  => (lambda (dn0)
                       (table (hash-set th v (density+ dn0 dn)) tddim))]
                 [else (table (hash-set th v dn) tddim)])]
          [else tbl])))

;; A (HeapEntry A) is (cons Density (-> (EnumTree A))).
(define (entry->=? x y) (density<=? (car y) (car x)))

;; explore/tree : Heap Table (EnumTree A) Density -> Table
(define (explore/tree hp tbl tree tree-dn)
  (match tree
    [(only a)
     (explore/heap hp (table-add tbl a tree-dn))]
    [(? split? et)
     (define hp*
       (for/fold ([hp hp]) ([sub (in-list (split->subtrees tree))])
         (match-define (cons dn k) sub)
         (heap-insert hp (cons (density* tree-dn dn) k))))
     (explore/heap hp* tbl)]
    [(weight dn k)
     (explore/tree hp tbl (k) (density* tree-dn dn))]
    [(failed reason)
     (explore/heap hp tbl)]))

;; explore/heap : Heap Table -> Table
(define (explore/heap hp tbl)
  (cond [(heap-empty? hp) tbl]
        [else
         (let ([sub (heap-find-min/max hp)]
               [hp (heap-delete-min/max hp)])
           (match-define (cons dn k) sub)
           (explore/tree hp tbl (k) dn))]))
