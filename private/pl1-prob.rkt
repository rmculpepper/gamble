#lang racket/base
(require racket/list
         data/order
         "pl1-context.rkt")
(provide current-db
         last-db
         reset-db
         apply/reset
         print-db

         pl1-ERP
         pl1-mem)

;; Unlike bher, use two databases---better for detecting collisions.

;; ERPTag = 'flip | ...

;; An Entry is (entry ERPTag Any)
;; where the value is appropriate for the ERP denoted by the tag.
(struct entry (tag value) #:prefab)

;; current-db, last-db : hash[Address => Entry]
(define current-db (make-hash))
(define last-db (make-hash))

;; "reset" (maybe wrong word) means end one logged run and start another;
;; the current db becomes available as history for the next run.
;; That is, bher would do { run, reset, perturb } repeatedly.
(define (reset-db)
  (eprintf "Resetting db\n")
  (set! last-db current-db)
  (set! current-db (make-hash)))

(define (apply/reset f . args)
  (reset-db)
  (apply apply/delimit f args))

(define (print-db db)
  (define entries (hash-map db list))
  (define sorted-entries
    (sort entries (order-<? datum-order) #:key car))
  (for ([entry (in-list sorted-entries)])
    (printf "~s => ~s\n" (car entry) (cadr entry))))

;; ----

(define (pl1-ERP tag sample _get-dist)
  (define context (get-context))
  (define (mem-context?)
    (and (pair? context)
         (let ([frame (last context)])
           (and (list? frame) (memq 'mem frame)))))
  (define (new!)
    (define result (sample))
    (hash-set! current-db context (entry tag result))
    result)
  (cond [(hash-ref current-db context #f)
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
        [(hash-ref last-db context #f)
         => (lambda (e)
              (cond [(not (equal? (entry-tag e) tag))
                     (eprintf "- MISMATCH ~s / ~s: ~s\n" (entry-tag e) tag context)
                     (new!)]
                    [else
                     (eprintf "- REUSED ~s: ~s\n" tag context)
                     (define result (entry-value e))
                     (hash-set! current-db context (entry tag result))
                     result]))]
        [else
         (eprintf "- NEW ~s: ~s\n" tag context)
         (new!)]))

(define (pl1-mem f)
  (let ([context (get-context)])
    (lambda args
      (apply/delimit
       (lambda ()
         (with-continuation-mark CM-KEY (list 'mem args context)
           (apply f args)))))))
