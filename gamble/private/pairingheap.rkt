;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Adapted from (planet krhari/pfds:1:5/pairingheap)
;; http://planet.racket-lang.org/package-source/krhari/pfds.plt/1/5/

#lang racket/base
(provide heap
         heap-empty?
         heap-count
         heap-insert
         heap-merge
         heap-find-min/max
         heap-delete-min/max)

;; If comparator is <, then heap is a min-heap.

;; A (PairingHeap A) is (PairingHeap Comparator (IntHeap A))
;; An (IntHeap A) is '() | (Tree A)
;; A (Tree A) is (Tree A (ListOf (IntHeap A)))
(struct Tree (elem heaps))
(struct PairingHeap (comparer heap))

;; heap-count : (PairingHeap A) -> Integer
(define (heap-count pheap)
  (let loop ([ih (PairingHeap-heap pheap)])
    (cond [(null? ih) 0]
          [else (apply + 1 (map loop (Tree-heaps ih)))])))
  
;; An empty heap
(define empty null)

;; Checks for empty
;; heap-empty? : (All (A) ((PairingHeap A) -> Boolean))
(define (heap-empty? pheap)
  (null? (PairingHeap-heap pheap)))

;; heap : (All (A) ((A A -> Boolean) A * -> (PairingHeap A)))
;; Heap constructor function
(define (heap comparer . lst)
  (for/fold ([h (PairingHeap comparer null)]) ([a (in-list lst)])
    (heap-insert h a)))

;; heap-insert : (All (A) ((PairingHeap A) A -> (PairingHeap A)))
;; Inserts an element into the heap
(define (heap-insert pheap elem)
  (let ([comparer (PairingHeap-comparer pheap)])
    (PairingHeap comparer
                 (in-merge (Tree elem null) 
                           (PairingHeap-heap pheap)
                           comparer))))

;; heap-merge : (All (A) ((PairingHeap A) (PairingHeap A) -> (PairingHeap A)))
;; Merges two given heaps
(define (heap-merge heap1 heap2)
  (let ([comparer (PairingHeap-comparer heap1)])
    (PairingHeap comparer
                 (in-merge (PairingHeap-heap heap1) 
                           (PairingHeap-heap heap2) 
                           comparer))))

;; in-merge : (All (A) ((IntHeap A) (IntHeap A) (A A -> Boolean) -> (IntHeap A)))
(define (in-merge heap1 heap2 comparer)
  (cond
    [(null? heap2) heap1]
    [(null? heap1) heap2]
    [else (in-merge-helper heap1 heap2 comparer)]))

;; in-merge-helper : (All (A) ((Tree A) (Tree A) (A A -> Boolean) -> (IntHeap A)))
(define (in-merge-helper tree1 tree2 comparer)
  (let ([tr1-elm (Tree-elem tree1)]
        [tr2-elm (Tree-elem tree2)]
        [tr1-heaps (Tree-heaps tree1)]
        [tr2-heaps (Tree-heaps tree2)])
    (if (comparer tr1-elm tr2-elm)
        (Tree tr1-elm (cons tree2 tr1-heaps))
        (Tree tr2-elm (cons tree1 tr2-heaps)))))

;; heap-find-min/max : (All (A) ((PairingHeap A) -> A))
;; Returns min or max element of the heap
(define (heap-find-min/max pheap)
  (let ([heap (PairingHeap-heap pheap)]
        [comparer (PairingHeap-comparer pheap)])
    (if (null? heap)
        (error 'find-min/max "heap is empty")
        (Tree-elem heap))))

;; heap-delete-min/max  : (All (A) ((PairingHeap A) -> (PairingHeap A)))
;; Deletes an element of the heap
(define (heap-delete-min/max pheap)
  (let ([heap (PairingHeap-heap pheap)]
        [comparer (PairingHeap-comparer pheap)])
    (if (null? heap)
        (error 'delete-min/max "heap is empty")
        (PairingHeap comparer
                     (merge-pairs (Tree-heaps heap) comparer)))))

;; merge-pairs : (All (A) ((Listof (IntHeap A)) (A A -> Boolean) -> (IntHeap A)))
;; A helper for delete-min/max
(define (merge-pairs lst comparer)
  (cond
    [(null? lst) empty]
    [(null? (cdr lst)) (car lst)]
    [else (in-merge (in-merge (car lst) (cadr lst) comparer) 
                    (merge-pairs (cddr lst) comparer) 
                    comparer)]))
