;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-template racket/base)
         (for-syntax racket/base
                     racket/syntax)
         racket/runtime-path
         syntax/id-table
         syntax/stx
         syntax/parse
         syntax/parse/experimental/template
         "ast.rkt"
         "known-functions.rkt")
(provide define-ref/set

         modfix
         inc-mod-counter!
         hash-set/mod!

         transform-TAG+CS
         TAG
         CALL-SITE
         syntax-summary

         analyze-FUN-EXP
         FUN-EXP
         lambda-form?

         analyze-CALLS-ERP
         app-calls-erp?

         parse-ast)

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
       (T (set! var (recur e)))]
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

(define lenv (make-free-id-table))
(define lvar-counter 1)
(define (next-lvar)
  (begin0 lvar-counter (set! lvar-counter (add1 lvar-counter))))
(define (lenv-add1! var)
  (let ([n (next-lvar)]) (begin0 n (free-id-table-set! lenv var n))))
(define (lenv-add! vars)
  (for/list ([var (in-list (stx->list vars))])
    (lenv-add1! var)))

(define (parse-ast stx)
  (define special-env (make-free-id-table))
  (define ctxenv (make-free-id-table))
  (define ctxvar-counter 0)
  (define (next-ctxvar)
    (begin0 ctxvar-counter (set! ctxvar-counter (add1 ctxvar-counter))))
  (define (loop stx)
    (define (loop* stxs) (map loop (stx->list stxs)))
    (syntax-parse stx
      #:literal-sets (kernel-literals) #:literals (variable-reference-from-unsafe?)
      [(if (#%plain-app variable-reference-from-unsafe? (#%variable-reference)) e2 e3)
       (loop #'e3)]
      ;; --------------------
      [var:id
       (cond [(free-id-table-ref special-env #'var #f)
              (raise-syntax-error #f "special operation used as variable" #'var)]
             [(free-id-table-ref lenv #'var #f)
              => (lambda (index) (ast:lvar index))]
             [else
              (define index (free-id-table-ref! ctxenv #'var (lambda () (next-ctxvar))))
              (ast:ctxvar index)])]
      [(#%plain-lambda (var:id ...) e ...)
       (ast:lambda (lenv-add! #'(var ...)) #f
                   (wrap-begin (loop* #'(e ...))))]
      [(#%plain-lambda (var:id ... . rest-var:id) e ...)
       (ast:lambda (lenv-add! #'(var ...)) (lenv-add1! #'rest-var)
                   (wrap-begin (loop* #'(e ...))))]
      [(case-lambda clause ...)
       (ast:case-lambda (for/list ([c (in-list (syntax->list #'(clause ...)))])
                          (syntax-parse c
                            [[(var:id ...) e ...]
                             (ast:lambda (lenv-add! #'(var ...)) #f
                                         (wrap-begin (loop* #'(e ...))))]
                            [[(var:id ... . rest-var:id) e ...]
                             (ast:lambda (lenv-add! #'(var ...)) (lenv-add! #'rest-var)
                                         (wrap-begin (loop* #'(e ...))))])))]
      [(if e1 e2 e3)
       (ast:if (loop #'e1) (loop #'e2) (loop #'e3))]
      [(begin e ...)
       (ast:begin (loop* #'(e ...)))]
      [(begin0 e0 e ...)
       (ast:begin0 (loop #'e0) (loop* #'(e ...)))]
      [(let-values ([vars rhs] ...) body ...)
       (wrap-let-values
        (for/list ([vars (in-list (stx->list #'(vars ...)))]
                   [rhs (in-list (stx->list #'(rhs ...)))])
          (define lvars (lenv-add! vars))
          (ast:lv-clause lvars (loop rhs)))
        (wrap-begin (loop* #'(body ...))))]
      [(letrec-values ([vars rhs] ...) body ...)
       (define lvarss (for/list ([vars (in-list (stx->list #'(vars ...)))])
                        (lenv-add! vars)))
       (ast:letrec-values
        (for/list ([lvars (in-list lvarss)]
                   [rhs (in-list (stx->list #'(rhs ...)))])
          (ast:lv-clause lvars (loop rhs)))
        (wrap-begin (loop* #'(body ...))))]
      #;[(set! var e) _]
      [(quote d)
       (ast:quote (syntax->datum #'d))]
      #;[(quote-syntax . _) _]
      [(with-continuation-mark e1 e2 e3)
       (ast:wcm (loop #'e1) (loop #'e2) (loop #'e3))]
      [(#%plain-app f e ...)
       (define cs (and (function-may-call-erp? #'f) (CALL-SITE stx)))
       (define args (loop* #'(e ...)))
       (define argc (length args))
       (or (case (and (identifier? #'f) (free-id-table-ref special-env #'f #f))
             [(sample) (cond [(= argc 1) (ast:sample cs (car args) (ast:quote #f))]
                             [(= argc 2) (ast:sample cs (car args) (cadr args))]
                             [else #f])]
             [(dscore) (and (= argc 1) (ast:dscore (car args)))]
             [(lscore) (and (= argc 1) (ast:lscore (car args)))]
             [(observe) (and (= argc 2) (ast:observe (car args) (cadr args)))]
             [(fail)   (cond [(= argc 0) (ast:fail (ast:quote #f))]
                             [(= argc 1) (ast:fail (car args))]
                             [else #f])]
             [(mem)    (and (= argc 1) (ast:mem (car args)))]
             [(run-model) (and (= argc 1) (ast:run-model (car args)))]
             [else #f])
           (ast:app cs (loop #'f) args))]
      [(#%top . var:id)
       (loop #'var)]
      #;[(#%variable-reference . _) _]
      [(#%expression e)
       (loop #'e)]
      [_ (raise-syntax-error #f "unhandled syntax" stx)]
      ))
  (define (top stx)
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      #:literals (#;ctx-get-functions)
      ;; Note: must be kept in sync with `with-ctx` and `model` expressions.
      [(#%plain-lambda (ctx)
         (let-values ([(ctx-sample
                        ctx-dscore
                        ctx-lscore
                        ctx-observe
                        ctx-fail
                        ctx-mem
                        ctx-run-model)
                       (#%plain-app (~datum ctx-get-functions) ctx2:id)])
           body:expr))
       #:when (free-identifier=? #'ctx #'ctx2)
       (free-id-table-set! special-env #'ctx-sample 'sample)
       (free-id-table-set! special-env #'ctx-dscore 'dscore)
       (free-id-table-set! special-env #'ctx-lscore 'lscore)
       (free-id-table-set! special-env #'ctx-observe 'observe)
       (free-id-table-set! special-env #'ctx-fail 'fail)
       (free-id-table-set! special-env #'ctx-mem 'mem)
       (free-id-table-set! special-env #'ctx-run-model 'run-model)
       (loop #'body)]))
  (values (top stx)
          (let ([v (make-vector ctxvar-counter)])
            (for ([(var index) (in-free-id-table ctxenv)])
              (vector-set! v index var))
            (vector->list v))))
