;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require racket/match
         racket/class
         racket/stream
         "dist/base.rkt"
         "dist/discrete.rkt"
         "base.rkt"
         "util/dnum.rkt"
         "util/real.rkt")
(provide enumerate)

;; enumerate : (Model A) -> (Discrete-Dist A)
(define (enumerate mdl
                   #:stop [quit-weight 0]
                   #:discretize [discretize #f]
                   #:normalize? [normalize? #f])
  (unless (model? mdl) (raise-argument-error 'enumerate "model?" mdl))
  (define ctx (new enumerate-stochastic-ctx% (discretize discretize)))
  (define (init-thunk) (send ctx run-top mdl))
  (define quit-dnum (linear-dnum quit-weight))
  (define dh (simple-enumerate init-thunk quit-dnum))
  (hash->discrete-dist (for/fold ([h (hash)]) ([(v dn) (in-hash dh)])
                         (hash-set h v (dnum->linear-real dn)))
                       #:normalize? normalize?))

;; simple-enumerate : (-> (EnumTree A)) Dnum -> (Hash A Dnum)
;; Handle all paths w/ weight > quit-dnum first, using DFS.
;; Then use heap for paths w/ weight <= quit-dnum, if necessary.
(define (simple-enumerate init-thunk quit-dn)
  (define-values (h wl)
    (let loop ([h (hash)] [wl null] [dn (linear-dnum 1)] [thunk init-thunk])
      (match (thunk)
        [(done v)
         (values (hash-set h v (dnum+ dn (hash-ref h v #f))) wl)]
        [(list (list* _ wdn continue)) ;; score, continue immediately
         (loop h wl (dnum* wdn dn) continue)]
        [(? list? dn+continue-list)
         (for/fold ([h h] [wl wl]) ([dn+continue (in-list dn+continue-list)])
           (match-define (list* tdn wdn continue) dn+continue)
           (define tdn* (dnum* tdn dn))
           (define wdn* (dnum* wdn dn))
           (if (dnum<=? tdn* quit-dn)
               (values h (cons (list* tdn* wdn* continue) wl))
               (loop h wl wdn* continue)))])))
  (if (null? wl) h (heap-enumerate quit-dn (list->heap wl) (dnum-sum (map car wl)) h)))

;; heap-enumerate : Dnum (Heap Entry) Dnum (Hash X Dnum) -> (Hash X Dnum)
;; where Entry = (list* Dnum Dnum (-> (EnumTree A)))
(define (heap-enumerate quit-dnum heap heapdn h)
  (define (heaploop heap heapdn h)
    (cond [(dnum<=? heapdn quit-dnum)
           h]
          [else
           (match (heap-case heap)
             [#f h]
             [(cons (list* tdn wdn continue) heap)
              (continueloop heap (dnum- heapdn tdn) h wdn continue)])]))
  (define (continueloop heap heapdn h dn continue)
    (match (continue)
      [(done v)
       (define h* (hash-set h v (dnum+ dn (hash-ref h v #f))))
       (heaploop heap heapdn h*)]
      [(list (list* _ wdn continue)) ;; score, continue immediately
       (continueloop heap heapdn h (dnum* wdn dn) continue)]
      [(? list? wdn+continue-list)
       (define-values (heap* heapdn*)
         (for/fold ([heap heap] [heapdn heapdn])
                   ([wdn+continue (in-list wdn+continue-list)])
           (match-define (list* tdn wdn continue) wdn+continue)
           (define tdn* (dnum* dn tdn))
           (define wdn* (dnum* dn wdn))
           (values (heap-insert heap (list* tdn* wdn* continue))
                   (dnum+ heapdn tdn*))))
       (heaploop heap* heapdn* h)]))
  (heaploop heap heapdn h))

;; A (EnumTree A) is one of
;; - (done A)
;; - (listof ContinueEntry)
;; where ContinueEntry = (list* Dnum Dnum/#f (-> (EnumTree A)))

;; An entry of (list* tdn wdn continue) means the prior weight of this
;; subtree is tdn, and its computed weights should be multiplied by wdn.
;; Usually tdn = wdn, but for example if enumerating (geometric-dist 1/2):
;;   (list (list* 1/2 1/2 continue1)
;;         (list* 1/4 1/4 continue2)
;;         (list* 1/4 1 (lambda () (list (list* 1/8 1/8 continue3)
;;                                       (list* 1/16 1/16 continue4)
;;                                       (list* 1/16 1 ....)))))
;; the third entry has wdn=1 so the inner entries don't have to be rescaled.

(struct done (answer))

(define SAMPLE-ELEMS 20)

;; ============================================================

(define enumerate-stochastic-ctx%
  (class base-stochastic-ctx%
    (init-field discretize) ;; #f or (Tag RealDist -> (U #f EnumerableDist))
    (inherit run-model)
    (super-new)

    (define memo-key (gensym))
    (define ctag (make-continuation-prompt-tag))

    (define/override (-sample dist tag addr)
      (cond [(finite-dist? dist)
             (call/restore 'sample
               (lambda (restore)
                 (for/list ([(v w) (in-dist dist)])
                   (define wdn (linear-dnum w))
                   (list* wdn wdn (lambda () (restore v))))))]
            [(enumerable-dist? dist)
             (call/restore 'sample
               (lambda (restore)
                 (define str (sequence->stream (in-dist dist)))
                 ;; FIXME: maybe need to start tdn from dist-total-measure?
                 (let loop ([str str] [n SAMPLE-ELEMS] [tdn (linear-dnum 1)])
                   (cond [(stream-empty? str) null]
                         [(zero? n)
                          (list (list* tdn (linear-dnum 1) (lambda () (loop str SAMPLE-ELEMS tdn))))]
                         [else
                          (define-values (v w) (stream-first str))
                          (define wdn (linear-dnum w))
                          (cons (list* wdn wdn (lambda () (restore v)))
                                (loop (stream-rest str) (sub1 n) (dnum- tdn wdn)))]))))]
            [(and discretize (real-dist? dist) (discretize tag dist))
             => (lambda (ddist)
                  (-sample ddist tag addr))]
            [else
             (call/restore 'sample
               (lambda (restore)
                 (error 'enumerate "cannot sample from non-enumerable dist\n  dist: ~e" dist)))]))

    (define/override (-dscore who dn)
      (if (dnum-zero? dn)
          (fail)
          (call/restore who
           (lambda (restore)
             (list (list* dn dn (lambda () (restore (void)))))))))

    (define/override (fail)
      (call/restore 'fail
       (lambda (restore)
         null)))

    (define/override (run-top mdl)
      (call (hash) (lambda () (done (run-model mdl #f)))))

    (define/private (call memo-table proc)
      (with-continuation-mark memo-key (box memo-table)
        (call-with-continuation-prompt proc ctag)))

    (define/private (call/restore who proc)
      (unless (continuation-prompt-available? ctag)
        (error who "used out of enumerate context"))
      (define memo-table (unbox (continuation-mark-set-first #f memo-key)))
      (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation ctag
          (lambda () (proc (lambda (v) (call memo-table (lambda () (k v))))))))
       ctag))

    (define/override (mem f addr)
      (define f-key (gensym))
      (define (memoized-function . args)
        (unless (continuation-prompt-available? ctag)
          (error 'memoized-function
                 (string-append "used out of enumerate context"
                                "\n  function: ~e\n  arguments: ~e\n")
                 f args))
        (define b (continuation-mark-set-first #f memo-key))
        (define key (cons f-key args))
        (cond [(hash-has-key? (unbox b) key)
               (hash-ref (unbox b) key)]
              [else
               ;; Call with creating context; may be outer enumeration!
               (define v (apply f args))
               ;; NOTE: outer b might be stale, if f called ERP!
               (define b (continuation-mark-set-first #f memo-key))
               (set-box! b (hash-set (unbox b) key v))
               v]))
      (procedure-reduce-arity memoized-function (procedure-arity f)))
    ))

;; ============================================================
;; Pairing heap (max-heap wrt dnum)

;; (Heap A) = null | (heaptree A (listof (Heap A)))
;; where A = (cons Dnum X)
(struct heaptree (elem heaps))

;; singleton-heap : A -> (Heap A)
(define (singleton-heap elem) (heaptree elem null))

;; list->heap : (Listof A) -> (Heap A)
(define (list->heap elems)
  (merge-pairs (map singleton-heap elems)))

;; heap-case : (Heap A) -> (U #f (cons A (Heap A)))
(define (heap-case heap)
  (match heap
    ['() #f]
    [(heaptree elem heaps)
     (cons elem (merge-pairs heaps))]))

;; heap-insert : (Heap A) A -> (Heap A)
(define (heap-insert heap elem)
  (heap-merge (heaptree elem null) heap))

;; heap-merge : (Heap A) (Heap A) -> (Heap A)
(define (heap-merge heap1 heap2)
  (match* [heap1 heap2]
    [[heap1 '()] heap1]
    [['() heap2] heap2]
    [[(heaptree elem1 heaps1) (heaptree elem2 heaps2)]
     (if (dnum<=? (car elem1) (car elem2))
         (heaptree elem2 (cons heap1 heaps2))
         (heaptree elem1 (cons heap2 heaps1)))]))

;; merge-pairs : (listof (Heap A)) -> (Heap A)
(define (merge-pairs heaps)
  (match heaps
    [(list) null]
    [(list heap) heap]
    [(list* heap1 heap2 heaps)
     (heap-merge (heap-merge heap1 heap2)
                 (merge-pairs heaps))]))
