#lang racket/base
(require racket/match
         math/distributions
         racket/control
         unstable/markparam
         "prob.rkt")
(provide enumerate-possibilities
         enum-ERP
         enum-mem)

#|
Enumeration based on delimited continuations (only for discrete ERPs)

1. Naive approach has limitiation that all paths must terminate
(not just "terminate with probability 1").

2. Slightly smarter: keep track of prob of path so far; prune away
paths with prob below some limit.

Problem: What if very many distinct paths with individually very low
probs? That is, what if prob of computed paths sums to significantly
less than 1?

Problem: What if conditioning focuses on pruned paths? Need to make
pruning prob adaptive wrt conditioning.

Without knowing structure of paths and conditioning predicate, can't
make smart distinctions between paths: can only do things like
iterative deepening (perhaps via laziness).

Would be nice if we could tell whether a path was viable or not wrt
condition. Seems like it would require drastic changes to model of
computation (eg, like symbolic execution) to support non-trivial
conditions. Would that be a profitable place to spend effort?
|#

#|
For mem, have a global store that gets rewound after each ERP choice
exploration.

Use markparam to store current global store. (Racket parameters don't
work well with delimited continuations.)

What should it mean for a memoized function to escape the enumeration
in which it was created? (The current implementation doesn't do
anything reasonable, probably.)
|#

;; ctag must be distinct from tag used to delimit continuation mark
(define ctag (make-continuation-prompt-tag))

;; A (EnumDist A) is one of
;; - (only A)
;; - (split (listof (list Prob (EnumDist A))))
(struct only (answer))
(struct split (subs))

;; current-global-memo-table : (parameterof (boxof (hash[list => result])))
(define current-global-memo-table (mark-parameter))

(define (enumerate-possibilities thunk #:limit [limit 1e-6])
  (let ([memo-table
         (cond [(current-global-memo-table) => unbox]
               [else (hash)])])
    (parameterize ((current-ERP enum-ERP)
                   (current-mem enum-mem))
      (mark-parameterize ((current-global-memo-table (box memo-table)))
        (flatten-enum-dist
         (call-with-continuation-prompt
          (lambda () (only (thunk)))
          ctag
          (lambda (f) (f 1 limit))))))))

(define (enum-ERP tag _sampler get-dist)
  (define g (gensym '@))
  (eprintf "ERP ~s, ~s\n" tag g)
  (let* ([dist (get-dist)]
         [vals (discrete-dist-values dist)]
         [probs (discrete-dist-probs dist)]
         [memo-table (unbox (current-global-memo-table))])
    (eprintf "  memo-table: ~s\n" memo-table)
    (call-with-composable-continuation
     (lambda (k)
       (abort-current-continuation
        ctag
        (lambda (current-path-prob limit)
          (split (for/list ([val (in-list vals)]
                            [prob (in-list probs)]
                            #:when (> (* prob current-path-prob) limit))
                   (eprintf "** ~s trying ~s\n" g val)
                   (list prob
                         (mark-parameterize ((current-global-memo-table (box memo-table)))
                           (call-with-continuation-prompt
                            (lambda () (k val))
                            ctag
                            (lambda (f) (f (* prob current-path-prob) limit))))))))))
     ctag)))

(define (flatten-enum-dist ed)
  (define prob-table (make-hash)) ;; a => prob
  (define seen '())
  (define (add! a p)
    (unless (hash-has-key? prob-table a)
      (set! seen (cons a seen)))
    (hash-set! prob-table a (+ p (hash-ref prob-table a 0))))
  (let loop ([ed ed] [p 1])
    (match ed
      [(only a) (add! a p)]
      [(split subs)
       (for/list ([sub (in-list subs)])
         (match sub
           [(list p* ed*)
            (loop ed* (* p p*))]))]))
  (for/list ([a (in-list (reverse seen))])
    (list a (hash-ref prob-table a))))

;; ----

;; use global-memo-table to effects can be unwound
(define (enum-mem f)
  (lambda args
    (eprintf "calling memoized function: ~s\n" args)
    (let ([b (current-global-memo-table)]
          [key (cons f args)])
      (eprintf "memo table is ~s\n" (unbox b))
      (cond [(hash-has-key? (unbox b) key)
             (eprintf "key found\n")
             (hash-ref (unbox b) key)]
            [else
             (eprintf "key not found\n")
             (let ([v (apply f args)])
               ;; NOTE: outer b might be stale, if f called ERP!
               (let ([b (current-global-memo-table)])
                 (set-box! b (hash-set (unbox b) key v))
                 v))]))))
