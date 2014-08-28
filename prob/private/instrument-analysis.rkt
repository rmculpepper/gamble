;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-template racket/base)
         racket/syntax
         syntax/id-table
         syntax/stx
         syntax/parse
         syntax/parse/experimental/template)
(provide analyze
         app-calls-erp?
         classify-function)

;; Need privileged inspector to rewrite expanded code.
(define stx-insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

;; ----------------------------------------

;; analyze : Syntax -> Syntax
(define (analyze stx)
  (define tagged-stx (add-tags stx))
  (analyze-CALLS-ERP tagged-stx)
  tagged-stx)

;; ----------------------------------------

(define counter 0)
(define tags (make-hash))
(define (new-tag [stx #f])
  (set! counter (add1 counter))
  (hash-set! tags counter stx)
  counter)
(define (get-tag stx) 
  (or (syntax-property stx 'tag)
      (error 'get-tag "no tag for: ~a\n" (syntax-summary stx))))

;; process : Syntax -> Syntax
;; Add unique tags to all forms under 'tag syntax-property.
(define (add-tags stx0)
  (define-template-metafunction recur
    (syntax-parser [(recur e) (add-tags #'e)]))
  (define-syntax-rule (T tmpl)
    (relocate (template tmpl) stx0))
  (define stx (syntax-disarm stx0 stx-insp))
  (define processed-stx
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      ;; Fully-Expanded Programs
      ;; -- module body
      [(#%plain-module-begin form ...)
       (T (#%plain-module-begin (recur form) ...))]
      ;; -- module-level form
      [(#%provide . _) stx]
      [(begin-for-syntax . _) stx]
      [(module . _) stx]
      ;; [(module* . _) stx]
      ;; [(#%declare . _) stx]
      ;; -- general top-level form
      [(define-values ids e)
       (T (define-values ids (recur e)))]
      [(define-syntaxes . _) stx]
      [(#%require . _) stx]
      ;; -- expr
      [var:id #'var]
      [(#%plain-lambda formals e ...)
       (T (#%plain-lambda formals (recur e) ...))]
      [(case-lambda [formals e ...] ...)
       (T (case-lambda [formals (recur e) ...] ...))]
      [(if e1 e2 e3)
       (T (if (recur e1) (recur e2) (recur e3)))]
      [(begin e ...)
       (T (begin (recur e) ...))]
      [(begin0 e ...)
       (T (begin0 (recur e) ...))]
      [(let-values ([vars rhs] ...) body ...)
       (T (let-values ([vars (recur rhs)] ...)
            (recur body) ...))]
      [(letrec-values ([vars rhs] ...) body ...)
       (T (letrec-values ([vars (recur rhs)] ...)
            (recur body) ...))]
      [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ...)
       (T (letrec-syntaxes+values ([svars srhs] ...) ([vvars (recur vrhs)] ...)
            (recur body) ...))]
      [(set! var e)
       ;; (eprintf "** set! in expanded code: ~e" (syntax->datum stx))
       (T (set! var (recur e)))]
      [(quote d) stx]
      [(quote-syntax s) stx]
      [(with-continuation-mark e1 e2 e3)
       (T (with-continuation-mark (recur e1) (recur e2) (recur e3)))]
      ;; #%plain-app -- see above
      [(#%plain-app e ...)
       (T (#%plain-app (recur e) ...))]
      [(#%top . _) stx]
      [(#%variable-reference . _) stx]
      [(#%expression e)
       (T (#%expression (recur e)))]
      [_
       (raise-syntax-error #f "unhandled syntax in process" stx)]
      ))
  ;; Rearm and track result
  (syntax-rearm
   (syntax-property processed-stx 'tag (new-tag processed-stx))
   stx0))

(define (relocate stx loc-stx)
  (datum->syntax stx (syntax-e stx) loc-stx stx))

;; ----------------------------------------

;; app-calls-erp? : Syntax -> Boolean
(define (app-calls-erp? stx)
  (hash-ref APP-CALLS-ERP (get-tag stx)))

;; APP-CALLS-ERP : hash[Nat -> Boolean]
;; Indicates whether a function application (but not the evaluation of
;; its arguments) might call an ERP.
(define APP-CALLS-ERP (make-hash))

;; LAM-CALLS-ERP : hash[Nat -> Boolean]
;; Indicates if tagged lambda expr might call an ERP when applied.
(define LAM-CALLS-ERP (make-hash))

;; FUN-EXP : id-table[Nat/#f]
;; Indicates lambda expr bound to id.
(define FUN-EXP (make-free-id-table))

;; number of modifications to *-CALLS-ERP
(define CALLS-ERP-mod 0)
(define (inc-mod!) (set! CALLS-ERP-mod (add1 CALLS-ERP-mod)))
(define-syntax-rule (modfix e)
  ;; repeat until CALLS-ERP-mod stops changing
  (let loop ([i 0])
    ;; (eprintf "modfix loop ~s\n" i)
    (define old-mod CALLS-ERP-mod)
    (define result e)
    (if (= CALLS-ERP-mod old-mod)
        result
        (loop (add1 i)))))

(define (set-APP-CALLS-ERP! stx val)
  (define tag (get-tag stx))
  (define old-val (hash-ref APP-CALLS-ERP tag 'none))
  (unless (equal? val old-val)
    (inc-mod!)
    (hash-set! APP-CALLS-ERP tag val)))

(define (set-LAM-CALLS-ERP! stx val)
  (define tag (get-tag stx))
  (define old-val (hash-ref LAM-CALLS-ERP tag 'none))
  (unless (equal? val old-val)
    (inc-mod!)
    (hash-set! LAM-CALLS-ERP tag val)))

;; analyze-CALLS-ERP : Syntax -> Boolean
(define (analyze-CALLS-ERP stx0)
  (define (recur e) (analyze-CALLS-ERP e))
  (define (recur* es) (strict-ormap recur (stx->list es)))
  (define (bind ids rhs)
    (syntax-parse ids
      [(x:id)
       (when (lambda-form? rhs)
         (free-id-table-set! FUN-EXP #'x (get-tag rhs)))]
      [_ (void)]))
  (define (bind* bindpairs)
    (for ([bindpair (in-list (stx->list bindpairs))])
      (syntax-parse bindpair
        [(ids rhs) (bind #'ids #'rhs)])))
  (define stx (syntax-disarm stx0 stx-insp))
  (define result
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      ;; Fully-Expanded Programs
      ;; -- module body
      [(#%plain-module-begin form ...)
       ;; FIXME: could do bind pass then modfix
       (recur* #'(form ...))]
      ;; -- module-level form
      [(#%provide . _) #f]
      [(begin-for-syntax . _) #f]
      [(module . _) #f]
      ;; [(module* . _) #f]
      ;; [(#%declare . _) #f]
      ;; -- general top-level form
      [(define-values ids e)
       (bind #'ids #'e)
       (modfix (recur #'e))]
      [(define-syntaxes . _) #f]
      [(#%require . _) #f]
      ;; -- expr
      [var:id
       #f]
      [(#%plain-lambda formals e ...)
       (let ([body-calls? (recur* #'(e ...))])
         (set-LAM-CALLS-ERP! stx body-calls?))
       #f]
      [(case-lambda [formals e ...] ...)
       (let ([body-calls? (recur* #'(e ... ...))])
         (set-LAM-CALLS-ERP! stx body-calls?))
       #f]
      [(if e1 e2 e3)
       (recur* #'(e1 e2 e3))]
      [(begin e ...)
       (recur* #'(e ...))]
      [(begin0 e ...)
       (recur* #'(e ...))]
      [(let-values ([vars rhs] ...) body ...)
       (bind* #'([vars rhs] ...))
       (strict-or (recur* #'(rhs ...))
                  (recur* #'(body ...)))]
      [(letrec-values ([vars rhs] ...) body ...)
       (bind* #'([vars rhs] ...))
       (strict-or (modfix (recur* #'(rhs ...)))
                  (recur* #'(body ...)))]
      [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ...)
       (bind* #'([vvars vrhs] ...))
       (strict-or (modfix (recur* #'(vrhs ...)))
                  (recur* #'(body ...)))]
      [(set! var e)
       (recur #'e)]
      [(quote d) #f]
      [(quote-syntax s) #f]
      [(with-continuation-mark e1 e2 e3)
       (recur* #'(e1 e2 e3))]
      ;; #%plain-app -- see above
      [(#%plain-app f:id e ...)
       (define calls-erp? (fun-calls-erp? #'f))
       (set-APP-CALLS-ERP! stx calls-erp?)
       (or (recur* #'(e ...)) calls-erp?)]
      [(#%plain-app e ...)
       (set-APP-CALLS-ERP! stx #t)
       (or (recur* #'(e ...)) #t)]
      [(#%top . _) #f]
      [(#%variable-reference . _) #f]
      [(#%expression e)
       (recur #'e)]
      [_
       (raise-syntax-error #f "unhandled syntax in analyze-CALLS-ERP" stx)]
      ))
  result)

(define (strict-or x y) (or x y))
(define (strict-ormap f xs) (ormap values (map f xs)))

(define (syntax-summary stx)
  (format "~s:~s ~.s" (syntax-line stx) (syntax-column stx) (syntax->datum stx)))

(define (lambda-form? rhs)
  (syntax-parse rhs
    #:literal-sets (kernel-literals)
    [(#%plain-lambda formals e body ...) #t]
    [(case-lambda [formals body ...] ...) #t]
    [_ #f]))

(define (fun-calls-erp? id)
  ;; conservative: #t if unknown function
  (cond [(free-id-table-ref FUN-EXP id #f)
         => (lambda (lam-tag)
              (hash-ref LAM-CALLS-ERP lam-tag #f))]
        [else (imported-fun-calls-erp? id)]))

(define (imported-fun-calls-erp? f-id)
  (case (classify-function f-id)
    [(safe) #f]
    [else #t]))

;; ----------------------------------------

;; ========================================

;; Function classification wrt Address

;; A function is "safe" if no ERP is ever executed in the context of a call
;; to that function. A "safe" function does not need an WCM around it for
;; address tracking.

;; TODO: add common non-kernel Racket functions
;; TODO: static analysis for locally-defined functions

;; classify-function : id -> (U 'safe 'unsafe 'unknown)
(define (classify-function f-id)
  (let ([b (identifier-binding f-id)])
    (if (list? b)
        (let ([def-mpi (car b)]
              [def-name (cadr b)])
          (let-values ([(def-mod def-relto) (module-path-index-split def-mpi)])
            (if (member def-mod '('#%kernel '#%paramz))
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

#|
To get list of '#%kernel exports:
(define (simplify e) (match e [`(just-meta ,n (rename '#%kernel ,x ,_)) x] [_ #f]))
(define knames
  (filter symbol?
          (map simplify
               (cdr (syntax->datum (expand '(require (rename-in '#%kernel))))))))
|#
