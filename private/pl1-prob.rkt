#lang racket/base
(require racket/list
         data/order
         "pl1-context.rkt")
(provide current-log
         last-log
         reset-log
         apply/reset
         print-log

         flip
         randn
         mem)

;; Unlike bher, use two logs (databases)---better for detecting collisions.

;; ERPTag = 'flip | ...

;; An Entry is (entry ERPTag Any)
;; where the value is appropriate for the ERP denoted by the tag.
(struct entry (tag value) #:prefab)

;; current-log, last-log : hash[Address => Entry]
(define current-log (make-hash))
(define last-log (make-hash))

;; "reset" (maybe wrong word) means end one logged run and start another;
;; the current log becomes available as history for the next run.
;; That is, bher would do { run, reset, perturb } repeatedly.
(define (reset-log)
  (eprintf "Resetting log\n")
  (set! last-log current-log)
  (set! current-log (make-hash)))

(define (apply/reset f . args)
  (reset-log)
  (apply apply/delimit f args))

(define (print-log log)
  (define entries (hash-map log list))
  (define sorted-entries
    (sort entries (order-<? datum-order) #:key car))
  (for ([entry (in-list sorted-entries)])
    (printf "~s => ~s\n" (car entry) (cadr entry))))

;; ----

;; ERP : Tag (-> a) -> a
(define (ERP tag thunk)
  (define context (get-context))
  (define (mem-context?)
    (and (pair? context)
         (let ([frame (last context)])
           (and (list? frame) (memq 'mem frame)))))
  (define (new!)
    (define result (thunk))
    (hash-set! current-log context (entry tag result))
    result)
  (cond [(hash-ref current-log context #f)
         => (lambda (e)
              (cond [(not (equal? (entry-tag e) tag))
                     (eprintf "- MISMATCH ~a ~s / ~s: ~s\n"
                              (if (mem-context?) "MEMOIZED" "COLLISION")
                              (entry-tag e) tag context)
                     (new!)]
                    [(mem-context?)
                     (eprintf "- MEMOIZED ~s: ~s\n" tag context)
                     (entry-value e)]
                    [else
                     (eprintf "- COLLISION ~s: ~s\n" tag context)
                     (entry-value e)]))]
        [(hash-ref last-log context #f)
         => (lambda (e)
              (cond [(not (equal? (entry-tag e) tag))
                     (eprintf "- MISMATCH ~s / ~s: ~s\n" (entry-tag e) tag context)
                     (new!)]
                    [else
                     (eprintf "- REUSED ~s: ~s\n" tag context)
                     (define result (entry-value e))
                     (hash-set! current-log context (entry tag result))
                     result]))]
        [else
         (eprintf "- NEW ~s: ~s\n" tag context)
         (new!)]))

;; flip : -> (U 0 1)
(define (flip)
  (ERP 'flip (lambda () (random 2))))

;; randn : Nat -> Nat
(define (randn n)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'randn "exact-positive-integer?" n))
  (ERP `(randn ,n) (lambda () (random n))))

;; mem : procedure -> procedure
(define (mem f)
  (unless (procedure? f)
    (raise-argument-error 'mem "procedure?" f))
  (let ([context (get-context)])
    (lambda args
      (apply/delimit
       (lambda ()
         (with-continuation-mark CM-KEY (list 'mem args)
           (apply f args)))))))
