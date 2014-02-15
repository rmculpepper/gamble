#lang racket/base
(require racket/list
         data/order
         "context.rkt"
         "prob.rkt")
(provide mh-sampler*
         print-db
         make-db-ERP
         db-mem
         verbose?)

(define verbose? (make-parameter #f))

;; Unlike bher, use two databases---better for detecting collisions (debugging).

;; An Entry is (entry ERPTag Any)
;; where the value is appropriate for the ERP denoted by the tag.
(struct entry (tag value) #:prefab)

(define (print-db db)
  (define entries (hash-map db list))
  (define sorted-entries
    (sort entries (order-<? datum-order) #:key car))
  (for ([entry (in-list sorted-entries)])
    (printf "~s => ~s\n" (car entry) (cadr entry))))

;; ----

(define (mh-sampler* thunk pred project)
  (let ([last-db (make-hash)]
        [current-db (make-hash)])
    (define (run)
      ;; Rotate & perturb
      (define saved-last-db last-db)
      (define saved-current-db (hash-copy current-db))
      (set! last-db current-db)
      (set! current-db (make-hash))
      (perturb! last-db)
      ;; Run
      (define result
        (parameterize ((current-ERP (make-db-ERP last-db current-db))
                       (current-mem db-mem))
          (apply/delimit thunk)))
      ;; Post???
      (cond [(pred result)
             ;; Post???
             (project result)]
            [else
             (set! last-db saved-last-db)
             (set! current-db saved-current-db)
             (run)]))
    run))

;; perturb! : DB -> void
(define (perturb! db)
  ;; Most naive possible perturbation: just delete an entry
  (let ([n (hash-count db)])
    (unless (zero? n)
      (let* ([index (random n)] ;; more intelligent dist ???
             [iter (for/fold ([iter (hash-iterate-first db)]) ([i index])
                     (hash-iterate-next db iter))])
        (when (verbose?)
          (eprintf "removing entry for key: ~s\n" (hash-iterate-key db iter)))
        (hash-remove! db (hash-iterate-key db iter))))))

;; ----

(define ((make-db-ERP last-db current-db) tag sample _get-dist)
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
                     (when (or (verbose?) #t)
                       (eprintf "- MISMATCH ~a ~s / ~s: ~s\n"
                                (if (mem-context?) "MEMOIZED" "COLLISION")
                                (entry-tag e) tag context))
                     (new!)]
                    [(mem-context?)
                     (when (verbose?)
                       (eprintf "- MEMOIZED ~s: ~s\n" tag context))
                     (entry-value e)]
                    [else
                     (when (verbose?)
                       (eprintf "- COLLISION ~s: ~s\n" tag context))
                     (entry-value e)]))]
        [(hash-ref last-db context #f)
         => (lambda (e)
              (cond [(not (equal? (entry-tag e) tag))
                     (when (verbose?)
                       (eprintf "- MISMATCH ~s / ~s: ~s\n" (entry-tag e) tag context))
                     (new!)]
                    [else
                     (when (verbose?)
                       (eprintf "- REUSED ~s: ~s\n" tag context))
                     (define result (entry-value e))
                     (hash-set! current-db context (entry tag result))
                     result]))]
        [else
         (when (verbose?)
           (eprintf "- NEW ~s: ~s\n" tag context))
         (new!)]))

(define (db-mem f)
  (let ([context (get-context)])
    (lambda args
      (apply/delimit
       (lambda ()
         (with-continuation-mark CM-KEY (list 'mem args context)
           (apply f args)))))))
