;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require (for-template racket/base "../base.rkt")
         (for-syntax racket/base
                     racket/syntax)
         racket/runtime-path
         syntax/id-table
         syntax/stx
         syntax/parse
         syntax/parse/experimental/template
         "known-functions.rkt")
(provide (except-out (all-defined-out)
                     mod-counter
                     new-tag
                     tag-counter
                     next-call-site
                     call-site-counter
                     relocate))

;; ============================================================
;; Modification counter (used to find fixed points)

(define mod-counter 0)

(define (inc-mod-counter!)
  (set! mod-counter (add1 mod-counter)))

(define NONE (gensym 'none))

(define (hash-set/mod! h k v)
  (define old-val (hash-ref h k NONE))
  (unless (equal? v old-val)
    (inc-mod-counter!)
    (hash-set! h k v)))

(define-syntax-rule (modfix e)
  ;; repeat until mod-counter stops changing
  (let loop ([i 0])
    ;; (eprintf "modfix loop ~s\n" i)
    (define old-mod-counter mod-counter)
    (define result e)
    (if (= mod-counter old-mod-counter)
        result
        (loop (add1 i)))))

;; ============================================================
;; Defining hash-backed get and set functions with tag keys.

(define-syntax (define-ref/set stx)
  (syntax-case stx ()
    [(_ X)
     (with-syntax ([X-table (format-id #'X "~a-table" #'X)]
                   [X-set! (format-id #'X "~a-set!" #'X)])
       #'(begin
           (define X-table (make-hash))
           (define (X k [default #f])
             (hash-ref X-table (if (syntax? k) (TAG k) k) default))
           (define (X-set! k0 v)
             (define k (if (syntax? k0) (TAG k0) k0))
             (hash-set/mod! X-table k v))))]))

;; ============================================================
;; Tagging: add integer label for each form

(define tag-counter 0)

(define (new-tag [stx #f])
  (set! tag-counter (add1 tag-counter))
  tag-counter)

(define TAG
  (case-lambda
    [(stx)
     (or (syntax-property stx 'tag)
         (error 'TAG "no tag for: ~a\n" (syntax-summary stx)))]
    [(stx default)
     (or (syntax-property stx 'tag)
         (if (procedure? default) (default) default))]))

(define call-site-counter (make-parameter #f))

;; next-call-site : -> Nat
(define (next-call-site)
  (let ([cs (call-site-counter)])
    (begin (call-site-counter (add1 cs)) cs)))

(define (CALL-SITE stx)
  (or (syntax-property stx 'call-site)
      (raise-syntax-error #f "internal error: missing call-site index" stx)))

;; transform-TAG+CS : Syntax -> (U Syntax Nat)
;; Add unique tags to all forms under 'tag syntax-property.
;; Add index to every call site under 'call-site syntax-property.
;; Also introduce names for all expressions in operator position.
(define (transform-TAG+CS stx)
  (parameterize ((call-site-counter 0))
    (values (transform-TAG+CS* stx)
            (call-site-counter))))

(define (transform-TAG+CS* stx)
  (define-template-metafunction recur
    (syntax-parser [(recur e) (transform-TAG+CS* #'e)]))
  (define-syntax-rule (T tmpl)
    (relocate (template tmpl) stx))
  (define the-tag (new-tag stx))
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
      [(module* . _) stx]
      [(#%declare . _) stx]
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
      [(set! var e)
       (raise-syntax-error #f "disallowed within model" stx)]
      [(quote d) stx]
      [(quote-syntax . _) stx]
      [(with-continuation-mark e1 e2 e3)
       (T (with-continuation-mark (recur e1) (recur e2) (recur e3)))]
      [(#%plain-app f:id e ...)
       (define cs (next-call-site))
       (syntax-property (T (#%plain-app (recur f) (recur e) ...))
                        'call-site cs)]
      [(#%plain-app f e ...)
       (define cs (next-call-site))
       (with-syntax ([(ftmp) (generate-temporaries #'(ftmp))])
         (syntax-property (T (recur (let-values ([(ftmp) f]) (#%plain-app ftmp e ...))))
                          'call-site cs))]
      [(#%top . _) stx]
      [(#%variable-reference . _) stx]
      [(#%expression e)
       (T (#%expression (recur e)))]
      [_ (raise-syntax-error #f "unhandled syntax in transform-TAG" stx)]
      ))
  (syntax-property (syntax-property processed-stx 'tag the-tag)
                   'original-for-check-syntax #t))

(define (relocate stx loc-stx)
  (datum->syntax stx (syntax-e stx) loc-stx loc-stx))

(define (syntax-summary stx)
  (format "~s:~s ~.s" (syntax-line stx) (syntax-column stx) (syntax->datum stx)))

;; ============================================================
;; FUN-EXP - Track lambda expressions

;; FUN-EXP : Identifier -> Nat/#f
;; Indicates (tag of) lambda expr bound to id, #f for not lambda.

;; FUN-EXP-table : (id-table Id => Nat/#f)
(define FUN-EXP-table (make-free-id-table))

(define (FUN-EXP id [default #f])
  (free-id-table-ref FUN-EXP-table id default))

;; analyze-FUN-EXP : Syntax -> Void
(define (analyze-FUN-EXP stx)
  (define (recur e) (analyze-FUN-EXP e))
  (define (recur* es) (for-each recur (stx->list es)))
  (define (bind ids rhs)
    (syntax-parse ids
      [(x:id)
       (when (lambda-form? rhs)
         (free-id-table-set! FUN-EXP-table #'x (TAG rhs)))]
      [_ (void)]))
  (define (bind* bindpairs)
    (for ([bindpair (in-list (stx->list bindpairs))])
      (syntax-parse bindpair
        [(ids rhs) (bind #'ids #'rhs)])))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    ;; Fully-Expanded Programs
    ;; -- module body
    [(#%plain-module-begin form ...)
     (recur* #'(form ...))]
    ;; -- module-level form
    [(#%provide . _) (void)]
    [(begin-for-syntax . _) (void)]
    [(module . _) (void)]
    [(module* . _) (void)]
    [(#%declare . _) (void)]
    ;; -- general top-level form
    [(define-values ids e)
     (bind #'ids #'e)
     (recur #'e)]
    [(define-syntaxes . _) (void)]
    [(#%require . _) (void)]
    ;; -- expr
    [var:id (void)]
    [(#%plain-lambda formals e ...)
     (recur* #'(e ...))]
    [(case-lambda [formals e ...] ...)
     (recur* #'(e ... ...))]
    [(if e1 e2 e3)
     (recur* #'(e1 e2 e3))]
    [(begin e ...)
     (recur* #'(e ...))]
    [(begin0 e ...)
     (recur* #'(e ...))]
    [(let-values ([vars rhs] ...) body ...)
     (bind* #'([vars rhs] ...))
     (recur* #'(rhs ...))
     (recur* #'(body ...))]
    [(letrec-values ([vars rhs] ...) body ...)
     (bind* #'([vars rhs] ...))
     (recur* #'(rhs ...))
     (recur* #'(body ...))]
    [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ...)
     (bind* #'([vvars vrhs] ...))
     (recur* #'(vrhs ...))
     (recur* #'(body ...))]
    [(set! var e)
     (recur #'e)]
    [(quote d) #f]
    [(quote-syntax . _) #f]
    [(with-continuation-mark e1 e2 e3)
     (recur* #'(e1 e2 e3))]
    [(#%plain-app e ...)
     (recur* #'(e ...))]
    [(#%top . _) (void)]
    [(#%variable-reference . _) (void)]
    [(#%expression e)
     (recur #'e)]
    [_ (raise-syntax-error #f "unhandled syntax in analyze-FUN-EXP" stx)]
    ))

(define (lambda-form? rhs)
  (syntax-parse rhs
    #:literal-sets (kernel-literals)
    [(#%plain-lambda formals e body ...) #t]
    [(case-lambda [formals body ...] ...) #t]
    [_ #f]))


;; ============================================================
;; CALLS-ERP: stochastic effects

;; CALLS-ERP(app) is true if a call to {sample, observe-sample, mem,
;; fail} may occur in the dynamic extent of the function call.

;; app-calls-erp? : Syntax -> Boolean
(define (app-calls-erp? stx)
  (hash-ref APP-CALLS-ERP (TAG stx)))

;; APP-CALLS-ERP : hash[Nat -> Boolean]
;; Indicates whether a function application (but not the evaluation of
;; its arguments) might call an ERP.
(define APP-CALLS-ERP (make-hash))

;; LAM-CALLS-ERP : hash[Nat -> Boolean]
;; Indicates if tagged lambda expr might call an ERP when applied.
(define LAM-CALLS-ERP (make-hash))

(define (set-APP-CALLS-ERP! stx val)
  (hash-set/mod! APP-CALLS-ERP (TAG stx) val))
(define (set-LAM-CALLS-ERP! stx val)
  (hash-set/mod! LAM-CALLS-ERP (TAG stx) val))

;; analyze-CALLS-ERP : Syntax -> Boolean
(define (analyze-CALLS-ERP stx)
  (define (recur e) (analyze-CALLS-ERP e))
  (define (recur* es) (strict-ormap recur (stx->list es)))
  (define result
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      ;; Fully-Expanded Programs
      ;; -- module body
      [(#%plain-module-begin form ...)
       (modfix (recur* #'(form ...)))]
      ;; -- module-level form
      [(#%provide . _) #f]
      [(begin-for-syntax . _) #f]
      [(module . _) #f]
      [(module* . _) #f]
      [(#%declare . _) #f]
      ;; -- general top-level form
      [(define-values ids e)
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
       (strict-or (recur* #'(rhs ...))
                  (recur* #'(body ...)))]
      [(letrec-values ([vars rhs] ...) body ...)
       (strict-or (modfix (recur* #'(rhs ...)))
                  (recur* #'(body ...)))]
      [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ...)
       (strict-or (modfix (recur* #'(vrhs ...)))
                  (recur* #'(body ...)))]
      [(set! var e)
       (recur #'e)]
      [(quote d) #f]
      [(quote-syntax . _) #f]
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
      [_ (raise-syntax-error #f "unhandled syntax in analyze-CALLS-ERP" stx)]
      ))
  result)

(define (strict-or x y)
  (or x y))
(define (strict-ormap f xs)
  (for/fold ([r #f]) ([x (in-list xs)]) (or (f x) r)))

(define (fun-calls-erp? id)
  ;; conservative: #t if unknown function
  (cond [(FUN-EXP id #f)
         => (lambda (lam-tag)
              (hash-ref LAM-CALLS-ERP lam-tag #f))]
        [else (function-may-call-erp? id)]))

;; ============================================================

(define model-function-table (make-free-id-table))

(define (register-model-definition! f* fi fg)
  (free-id-table-set! model-function-table f* (list fi fg)))

(define (model-definition-id? id)
  (and (free-id-table-ref model-function-table id #f) #t))

(define (vars->replacements xs)
  (define-values (replacements vars)
    (for/fold ([replacements null] [ys null]) ([x (in-list xs)])
      (cond [(free-id-table-ref model-function-table x #f)
             => (lambda (refs) (values (cons (cons x refs) replacements) ys))]
            [else (values replacements (cons x ys))])))
  (values (map car replacements) (map cadr replacements) (map caddr replacements) vars))

;; FIXME: need to fix free-vars to include registered top-level and module-level vars

;; ============================================================

(define (free-variables expr [add? (lambda (id) #f)])
  (define free (make-free-id-table))
  (define free-ids null)
  (define (free! id)
    (unless (free-id-table-ref free id #f)
      (set! free-ids (cons id free-ids))
      (free-id-table-set! free id #t)))
  (define bound (make-free-id-table))
  (define (bound? id) (free-id-table-ref bound id #f))
  (define (bound! x)
    (cond [(identifier? x) (free-id-table-set! bound x #t)]
          [(pair? x) (bound! (car x)) (bound! (cdr x))]
          [(syntax? x) (bound! (syntax-e x))]))
  (define (loop* es) (for-each loop (or (stx->list es) null)))
  (define (loop e)
    (syntax-parse e
      #:literal-sets (kernel-literals)
      [var:id
       (unless (bound? #'var)
         (cond [(eq? (identifier-binding #'var) 'lexical)
                (free! #'var)]
               [(add? #'var)
                (free! #'var)]
               [else (void)]))]
      [(#%plain-lambda formals e ...)
       (bound! #'formals)
       (loop* #'(e ...))]
      [(case-lambda [formals e ...] ...)
       (bound! #'(formals ...))
       (loop* #'(e ... ...))]
      [(if e1 e2 e3) (loop* #'(e1 e2 e3))]
      [(begin e ...) (loop* #'(e ...))]
      [(begin0 e ...) (loop* #'(e ...))]
      [(let-values ([vars rhs] ...) body ...)
       (bound! #'(vars ...))
       (loop* #'(rhs ... body ...))]
      [(letrec-values ([vars rhs] ...) body ...)
       (bound! #'(vars ...))
       (loop* #'(rhs ... body ...))]
      [(set! var e) (loop* #'(var e))]
      [(quote d) (void)]
      [(quote-syntax . _) (void)]
      [(with-continuation-mark e1 e2 e3)
       (loop* #'(e1 e2 e3))]
      [(#%plain-app e ...) (loop* #'(e ...))]
      [(#%top . var) (loop #'var)]
      [(#%variable-reference . _) (void)]
      [(#%expression e) (loop #'e)]))
  (loop expr)
  (reverse free-ids))
