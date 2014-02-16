#lang racket/base
(require racket/match
         data/heap
         data/order
         math/distributions
         racket/control
         unstable/markparam
         "prob.rkt")
(provide enumerate*
         enum-ERP
         enum-mem)

#|
Enumeration based on delimited continuations (only for discrete ERPs)

For enumeration to work:

 - All paths must either terminate or evaluate infinitely many ERPS.
   (and every ERP must generate at least two values with nonzero prob.)

 - The predicate must accept a nonempty set of paths.

Without knowing structure of paths and conditioning predicate, can't
make smart distinctions between incomplete paths.

Would be nice if we could tell whether a path was viable or not wrt
condition. Seems like it would require drastic changes to model of
computation (eg, like symbolic execution) to support non-trivial
conditions. Would that be a profitable place to spend effort?
|#

#|
For mem, have a global store that gets rewound after each ERP choice
exploration.

Use markparam to store current global store. (Racket parameters don't
work well with delimited continuations.) Since prompts also delimit
CCM, need a separate prompt tag (sigh... it's complicated).

What should it mean for a memoized function to escape the enumeration
in which it was created? (The current implementation doesn't do
anything reasonable, probably.)
|#

;; TODO:
;; - laziness
;; - add limit for prob of explored paths; error if not met
;; - maybe option to replace integer-valued distributions with
;;   discrete approximations (cf, geometric dist & repeated flip).

;; BUG/LIMITATION:
;; - delimited continuations captured by ERP *must not* use include
;;   uses of parameterize, because Racket's parameterize is not correct
;;   wrt delimited control (this is a known Racket WONTFIX bug)
;;   (ok to use parameterize, just not in captured part of continuation)
;;   FIXME: possible to detect and issue error??

;; Use default prompt tag for mark-parameter, ctag for delimited
;; continuations.
(define ctag (make-continuation-prompt-tag))

;; A (EnumTree A) is one of
;; - (only A)
;; - (split (listof (cons Prob (-> (EnumTree A)))))
(struct only (answer))
(struct split (subs))

;; current-global-memo-table : (parameterof (boxof (hash[list => result])))
(define current-global-memo-table (mark-parameter))

(define (enumerate* thunk pred [project values] #:limit [limit 1e-6])
  (explore (let ([memo-table
                  (cond [(current-global-memo-table)
                         ;; means recursive enumerate ...
                         => unbox]
                        [else (hash)])])
             (call-with-enum-context memo-table (lambda () (only (thunk)))))
           pred project limit))

;; explore : (EnumTree A) (A -> Boolean) (A -> B) Prob -> (listof (list Prob B))
;; Limit means: explore until potential accepted dist is < limit unaccounted for,
;; ie, unexplored < limit * accepted. Thus, limit is nonlocal; use BFS.
(define (explore tree pred project limit)
  (define prob-unexplored 1) ;; prob of all unexplored paths
  (define prob-accepted 0) ;; prob of all accepted paths
  (define table (make-hash)) ;; hash[B => Prob]
  (define seen '()) ;; (listof B), reversed
  ;; add! : A Prob -> void
  (define (add! a p)
    (set! prob-unexplored (- prob-unexplored p))
    (when (pred a)
      (let ([b (project a)])
        (unless (hash-has-key? table b)
          (set! seen (cons b seen)))
        (set! prob-accepted (+ p prob-accepted))
        (hash-set! table b (+ p (hash-ref table b 0))))))
  ;; h: heap[(cons Prob (-> (EnumTree A)))]
  (define (entry->=? x y)
    (>= (car x) (car y)))
  (define h (make-heap entry->=?)) ;; Racket has "min-heaps", but want max prob, so >=

  (define (traverse-tree et prob-of-tree)
    (match et
      [(only a)
       (add! a prob-of-tree)]
      [(split subs)
       (for ([sub (in-list subs)])
         (match sub
           [(cons p lt)
            (heap-add! h (cons (* prob-of-tree p) lt))]))]))

  (traverse-tree tree 1)
  (let loop ()
    (cond [(< prob-unexplored (* limit prob-accepted))
           ;; Done!
           (void)]
          [(zero? (heap-count h))
           ;; shouldn't happen; either internal error or pred accepted no paths
           (error 'enumerate "predicate accepted no paths")]
          [else
           (define sub (heap-min h)) ;; actually max prob
           (heap-remove-min! h)
           (traverse-tree ((cdr sub)) (car sub))
           (loop)]))

  ;; Finished exploring; now tabulate.
  (when #t
    (eprintf "enumerate: unexplored rate: ~s\n" prob-unexplored))
  (when #t
    (eprintf "enumerate: accept rate: ~s\n" prob-accepted))
  (tabulate table prob-accepted))

(define (tabulate table prob-accepted)
  (define entries
    (for/list ([(val prob) (in-hash table)])
      (list val (/ prob prob-accepted))))
  (sort entries (order-<? datum-order)))

;; ----

(define (enum-ERP tag _sampler get-dist)
  ;; (define g (gensym '@))
  (let* ([dist (get-dist)]
         [vals (discrete-dist-values dist)]
         [probs (discrete-dist-probs dist)]
         [memo-table (unbox (current-global-memo-table))])
    (call-with-composable-continuation
     (lambda (k)
       (abort-current-continuation
        ctag
        (lambda ()
          (split (for/list ([val (in-list vals)]
                            [prob (in-list probs)])
                   (cons prob
                         (lambda ()
                           (call-with-enum-context memo-table (lambda () (k val))))))))))
     ctag)))

;; calls thunk with memo-table in fresh box, parameters set up
(define (call-with-enum-context memo-table thunk)
  (parameterize ((current-ERP enum-ERP)
                 (current-mem enum-mem))
    (mark-parameterize ((current-global-memo-table (box memo-table)))
      (call-with-continuation-prompt
       thunk
       ctag))))

;; ----

;; use global-memo-table to effects can be unwound
(define (enum-mem f)
  (lambda args
    (let ([b (current-global-memo-table)]
          [key (cons f args)])
      (cond [(hash-has-key? (unbox b) key)
             (hash-ref (unbox b) key)]
            [else
             (let ([v (apply f args)])
               ;; NOTE: outer b might be stale, if f called ERP!
               (let ([b (current-global-memo-table)])
                 (set-box! b (hash-set (unbox b) key v))
                 v))]))))
