#lang racket/base
(require (for-syntax racket/base))
(provide (rename-out [app #%app])
         flip)

(begin-for-syntax
  (define counter 0)
  (define (next-counter)
    (begin0 counter
      (set! counter (add1 counter)))))

(define-syntax (app stx)
  (syntax-case stx ()
    [(app f arg ...)
     (with-syntax ([c (next-counter)])
       #'(call-with-immediate-continuation-mark
          'call-stack
          (lambda (v)
            ;; FIXME: eval f, args outside of wcm region!
            (with-continuation-mark 'call-stack (cons 'c v)
              (#%app f arg ...)))
          null))]))

;; ----

(define current-log (make-hash))

(define (flip)
  (define context
    (continuation-mark-set->list (current-continuation-marks) 'call-stack))
  (define result (random 2))
  
  (begin
    (when (hash-has-key? current-log context)
      (eprintf "context collision: ~s\n" context))
    (hash-set! current-log context result))
  
  result)

(define (print-log)
  (for ([(k v) (in-hash current-log)])
    (printf "~s => ~s\n" k v)))

(provide current-log
         print-log)

