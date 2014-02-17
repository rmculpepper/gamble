#lang racket/base
(require (for-syntax racket/base))
(provide CM-KEY
         app/call-site
         app/call-site*
         get-context
         apply/delimit
         (for-syntax classify-function))

#|
How to represent an Address (ie, a point in evaluation, reasonably stable 
across runs with different random choices)?

Version 0:
An Address0 is a (listof CallSite).
-- the list of call sites in the context (ie, continuation), most recent first

Note: call sites, not functions. Consider (define (f) (g (h) (h)))---need to
distinguish separate calls to h. But what if function changes? Well, what if
argument changes? We're ignoring the latter, why not ignore the former too?

Store address in context using with-continuation-mark (WCM) and retrieve using
current-continuation-marks (CCM).

Problem: tail calls. If f tail-calls itself, then second WCM overwrites first;
calls within the two activations of f will have colliding addresses. Also if 
f tail-calls g then g tail-calls f, collisions in two f activations. And so on. 
(See sum-n-flips* in test-pl1.rkt.)

Version 1 (current):

An Address is a (listof CallSequence)
--- a list of tail-call sequences, most recent first
A CallSequence is an improper list of CallSite.
--- the list of tail calls (most recent first) together with 
    the non-tail call they start from at the end

Issue: May change space complexity of program---but it needs to make finer
distinctions than original program, so somewhat justified. Maybe devise ad-hoc
representation optimizations: eg, RLE for self-tail-calling functions.
|#

(define CM-KEY 'call-stack)

(define-syntax (app/call-site stx)
  (syntax-case stx ()
    [(app/call-site call-site f arg ...)
     #'(app/call-site* #:un call-site f arg ...)]))

(define-syntax (app/call-site* stx)
  (syntax-case stx ()
    [(app/call-site* mode call-site f arg ...)
     (with-syntax ([(tmp-f)
                    (generate-temporaries #'(f))]
                   [(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #`(let ([c call-site] [tmp-f f] [tmp-arg arg] ...)
           (app/call-site** mode c tmp-f tmp-arg ...)))]))

(define-syntax (app/call-site** stx)
  (syntax-case stx ()
    [(app/call-site** #:un c f arg ...)
     #'(call-with-immediate-continuation-mark CM-KEY
         (lambda (v)
           (with-continuation-mark CM-KEY (if v (cons c v) c)
             (#%app f arg ...))))]
    [(app/call-site** #:tail c f arg ...)
     ;; No simpler way when known to be tail call
     #'(app/call-site** #:un c f arg ...)]
    [(app/call-site** #:nt c f arg ...)
     ;; Non-tail; WCM won't overwrite anything
     #'(with-continuation-mark CM-KEY c (#%app f arg ...))]))

(define (get-context)
  (continuation-mark-set->list (current-continuation-marks) CM-KEY))

;; Delimit call-site tracking.
;; Can't test using normal (f arg ...) syntax, because testing call-sites 
;; would be part of context! Use (apply/delimit f arg ...) instead.
(define (apply/delimit f . args)
  (call-with-continuation-prompt (lambda () (apply f args))))

;; ----

;; Many functions are "safe": okay to omit WCM around, since no ERP is
;; executed in the context of a call to them.
;; TODO: add common Racket functions
;; TODO: static analysis for locally-defined functions
(begin-for-syntax
 ;; classify-function : id -> (U 'safe 'unsafe 'unknown)
 (define (classify-function f-id)
   (let ([b (identifier-binding f-id)])
     (if (list? b)
         (let ([def-mpi (car b)]
               [def-name (cadr b)])
           (let-values ([(def-mod def-relto) (module-path-index-split def-mpi)])
             (if (equal? def-mod ''#%kernel)
                 (if (memq def-name HO-kernel-procedures)
                     'unsafe
                     'safe)
                 'unknown)))
         'unknown)))
 ;; functions defined in kernel, known to be unsafe
 (define HO-kernel-procedures
   '(;; omit indirect HO functions, like make-struct-type, chaperone-*, impersonate-*
     apply
     map
     for-each
     andmap
     ormap
     call-with-values
     call-with-escape-continuation
     call/ec
     call-with-current-continuation
     call/cc
     call-with-continuation-barrier
     call-with-continuation-prompt
     call-with-composable-continuation
     abort-current-continuation
     call-with-semaphore
     call-with-semaphore/enable-break
     call-with-immediate-continuation-mark
     time-apply
     dynamic-wind
     hash-map
     hash-for-each
     call-with-input-file
     call-with-output-file
     with-input-from-file
     with-output-to-file
     eval
     eval-syntax
     call-in-nested-thread
     ))
 )

#|
To get list of '#%kernel exports:
(define (simplify e) (match e [`(just-meta ,n (rename '#%kernel ,x ,_)) x] [_ #f]))
(define knames
  (filter symbol?
          (map simplify
               (cdr (syntax->datum (expand '(require (rename-in '#%kernel))))))))
|#
