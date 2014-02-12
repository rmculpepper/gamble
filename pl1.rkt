#lang racket/base
(require (for-syntax racket/base)
         racket/list)
(provide (except-out (all-from-out racket/base) #%app)
         (rename-out [app #%app]))

(define CM-KEY 'call-stack)

(begin-for-syntax
  ;; FIXME: counter resets for each new module, repl
  (define counter 0)
  (define (next-counter)
    (begin0 counter
      (set! counter (add1 counter)))))

(define-syntax (app stx)
  (syntax-case stx ()
    [(app f arg ...)
     (let ([c (next-counter)])
       (printf "app ~s = ~.s\n" c (cdr (syntax->datum stx)))
       (with-syntax ([(tmp-arg ...)
                      (generate-temporaries #'(arg ...))])
         #`(let ([tmp-f f] [tmp-arg arg] ...)
             (call-with-immediate-continuation-mark
              CM-KEY
              (lambda (v)
                (with-continuation-mark CM-KEY (if v (cons '#,c v) '#,c)
                  (#%app tmp-f tmp-arg ...)))))))]))

(define (get-context)
  (continuation-mark-set->list (current-continuation-marks) CM-KEY))

;; ----

(define current-log (make-hash))
(define last-log (make-hash))

(define (print-log)
  (for ([(k v) (in-hash current-log)])
    (printf "~s => ~s\n" k v)))

(define (apply/log f . args)
  (set! last-log current-log)
  (set! current-log (make-hash))
  (eprintf "Starting a log\n")
  ;; prompt to delimit CMs
  (call-with-continuation-prompt (lambda () (apply f args))))

(provide current-log
         last-log
         print-log
         apply/log)

;; ----

(define (flip)
  (define context (get-context))
  (define result
    (cond [(hash-ref current-log context #f)
           => (lambda (v)
                (if (and (pair? context)
                         (let ([frame (last context)])
                           (and (list? frame) (memq 'mem frame))))
                    (eprintf "- reusing flip -- mem: ~s\n" context)
                    (eprintf "- reusing flip -- context collision: ~s\n" context))
                v)]
          [(hash-ref last-log context #f)
           => (lambda (v)
                (eprintf "- reusing flip\n")
                v)]
          [else 
           (eprintf "- flipping -- context = ~s\n" context)
           (random 2)]))
  (hash-set! current-log context result)
  result)

(define (mem f)
  (define context (get-context))
  (lambda args
    (call-with-continuation-prompt
     (lambda ()
       (with-continuation-mark CM-KEY (list 'mem args)
         (apply f args))))))

(provide flip
         mem)

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

Issue: map, for/*, etc not annotated
- annotation instead of replace #%app would solve for for/*
  - naive annotator might introduce *many* more WCMs, though
- replacements for common HO functions, like map
- what about interop story?

|#
