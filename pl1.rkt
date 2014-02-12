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
     (let ([c (next-counter)])
       (printf "app ~s = ~.s\n" c (syntax->datum stx))
       (with-syntax ([(tmp-arg ...)
                      (generate-temporaries #'(arg ...))])
         #`(let ([tmp-f f] [tmp-arg arg] ...)
             (call-with-immediate-continuation-mark
              'call-stack
              (lambda (v)
                (with-continuation-mark 'call-stack (if v (cons '#,c v) '#,c)
                  (#%app tmp-f tmp-arg ...)))))))]))

;; ----

(define current-log (make-hash))
(define last-log (make-hash))

(define (flip)
  (define context
    (continuation-mark-set->list (current-continuation-marks) 'call-stack))
  (when (hash-has-key? current-log context)
    (eprintf "context collision: ~s\n" context))
  (define result 
    (cond [(hash-ref last-log context #f)
           => (lambda (v)
                (eprintf " - reusing flip\n")
                v)]
          [else 
           (eprintf " - flipping\n")
           (random 2)]))
  (hash-set! current-log context result)
  result)

(define (print-log)
  (for ([(k v) (in-hash current-log)])
    (printf "~s => ~s\n" k v)))

(define (apply/log f . args)
  (set! last-log current-log)
  (set! current-log (make-hash))
  (call-with-continuation-prompt (lambda () (apply f args))))

(provide current-log
         last-log
         print-log
         apply/log)


#|

Issue: tail recursion
- Need to distinguish different calls in tail-call sequence.
- What exactly are we using as a proxy for "eval node", anyway?
  - Don't want too much info (eg args, env contents), because it changes
    from run to run.
  - Need some context info to distinguish calls -- eg, (begin (flip) (flip))

Issue: useless WCMs
- Is this even a problem? Probably WCM is cheap, CCM is expensive.
- Maybe avoid via static analysis.

|#
