;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         racket/stream
         "dist/base.rkt"
         "dist/discrete.rkt"
         "base.rkt"
         "util/density.rkt"
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
  (define dh
    (cond [(> quit-weight 0)
           (heap-enumerate init-thunk quit-weight)]
          [else (simple-enumerate init-thunk)]))
  (hash->discrete-dist (for/fold ([h (hash)]) ([(v dn) (in-hash dh)])
                         (hash-set h v (density->real dn)))
                       #:normalize? normalize?))

;; simple-enumerate : (-> (EnumTree A)) -> (Hash A Density)
(define (simple-enumerate init-thunk)
  (let loop ([h (hash)] [dn one-density] [thunk init-thunk])
    (match (thunk)
      [(done v)
       (hash-set h v (density+ dn (hash-ref h v #f)))]
      [(? list? dn+continue-list)
       (for/fold ([h h]) ([dn+continue (in-list dn+continue-list)])
         (match-define (list* _ wdn continue) dn+continue)
         (loop h (density* wdn dn) continue))])))

;; heap-enumerate : (-> (EnumTree A)) Real -> (Hash A Density)
(define (heap-enumerate init-thunk quit-weight)
  (define init-heap (singleton-heap (list* one-density one-density init-thunk)))
  (define quit-density (density #f quit-weight))
  (define (heaploop heap heapdn h)
    (cond [(density<=? heapdn quit-density)
           h]
          [else
           (match (heap-case heap)
             [#f h]
             [(cons (list* tdn wdn continue) heap)
              (continueloop heap (density- heapdn tdn) h wdn continue)])]))
  (define (continueloop heap heapdn h dn continue)
    (match (continue)
      [(done v)
       (define h* (hash-set h v (density+ dn (hash-ref h v #f))))
       (heaploop heap heapdn h*)]
      [(? list? wdn+continue-list)
       (define-values (heap* heapdn*)
         (for/fold ([heap heap] [heapdn heapdn])
                   ([wdn+continue (in-list wdn+continue-list)])
           (match-define (list* tdn wdn continue) wdn+continue)
           (define tdn* (density* dn tdn))
           (define wdn* (density* dn wdn))
           (values (heap-insert heap (list* tdn* wdn* continue))
                   (density+ heapdn tdn*))))
       (heaploop heap* heapdn* h)]))
  (heaploop init-heap one-density (hash)))

;; A (EnumTree A) is one of
;; - (done A)
;; - (listof ContinueEntry)
;; where ContinueEntry = (list* Density Density/#f (-> (EnumTree A)))

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
                   (define wdn (density #f w))
                   (list* wdn wdn (lambda () (restore v))))))]
            [(enumerable-dist? dist)
             (call/restore 'sample
               (lambda (restore)
                 (define str (sequence->stream (in-dist dist)))
                 ;; FIXME: maybe need to start tdn from dist-total-measure?
                 (let loop ([str str] [n SAMPLE-ELEMS] [tdn one-density])
                   (cond [(stream-empty? str) null]
                         [(zero? n)
                          (list (list* tdn one-density (lambda () (loop str SAMPLE-ELEMS tdn))))]
                         [else
                          (define-values (v w) (stream-first str))
                          (define wdn (density #f w))
                          (cons (list* wdn wdn (lambda () (restore v)))
                                (loop (stream-rest str) (sub1 n) (density- tdn wdn)))]))))]
            [(and discretize (real-dist? dist) (discretize tag dist))
             => (lambda (ddist)
                  (-sample ddist tag addr))]
            [else
             (call/restore 'sample
               (lambda (restore)
                 (error 'enumerate "cannot sample from non-enumerable dist\n  dist: ~e" dist)))]))

    (define/override (-dscore who dn)
      (if (density-zero? dn)
          (fail who)
          (call/restore who
           (lambda (restore)
             (list (list* dn dn (lambda () (restore (void)))))))))

    (define/override (fail reason)
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
;; Pairing heap (max-heap wrt density)

;; (Heap A) = null | (heaptree A (listof (Heap A)))
;; where A = (cons Density X)
(struct heaptree (elem heaps))

;; singleton-heap : A -> (Heap A)
(define (singleton-heap elem) (heaptree elem null))

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
     (if (density<=? (car elem1) (car elem2))
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
