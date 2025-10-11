;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require (for-template racket/base "../base.rkt")
         (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/runtime-path
         syntax/id-table
         syntax/stx
         syntax/parse
         syntax/parse/experimental/template
         "traverse.rkt"
         "known-functions.rkt")
(provide (except-out (all-defined-out)
                     mod-counter
                     new-tag
                     tag-counter
                     next-call-site
                     call-site-counter))

(define-logger analyze)

;; transform+analyze : Syntax[EE] -> (values Syntax[EE*] Nat)
(define (transform+analyze ee)
  (define-values (tagged-ee call-site-count) (transform-TAG+CS ee))
  (analyze-FUN-EXP tagged-ee)
  (analyze-CALLS-ERP tagged-ee)
  (values tagged-ee call-site-count))

;; no-instrument-property : syntax property, indicates that model should not
;; instrument, should lift to avoid duplicating (used to prevent exponential
;; expansion of nested models)
(define no-instrument-property (string->uninterned-symbol "no-instrument"))

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
  (expression-traverse
   stx
   #:replace
   (lambda (stx recur0)
     (define-template-metafunction recur
       (syntax-parser [(recur e) (recur0 #'e)]))
     (define-syntax-rule (T tmpl)
       (relocate (syntax tmpl) stx))
     (syntax-parse stx
       #:literal-sets (kernel-literals)
       [(set! ~! . _)
        (raise-syntax-error #f "disallowed within model" stx)]
       [(#%plain-app f:id e ...)
        (let ([result-stx (T (#%plain-app (recur f) (recur e) ...))])
          (syntax-property result-stx 'call-site (next-call-site)))]
       [(#%plain-app f e ...)
        (with-syntax ([(ftmp) (generate-temporaries #'(ftmp))])
          (let ([result-stx (T (recur (let-values ([(ftmp) f])
                                        (#%plain-app ftmp e ...))))])
            (syntax-property result-stx 'call-site (next-call-site))))]
       [_ #f]))
   #:post
   (lambda (result-stx orig-stx)
     (define the-tag (new-tag orig-stx))
     (log-analyze-info "TAG ~s ~e\n" the-tag result-stx)
     (let* ([result-stx (syntax-property result-stx 'tag the-tag)]
            [result-stx (syntax-property result-stx 'original-for-check-syntax #t)])
       result-stx))))

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
  (expression-fold
   stx
   #:pre (lambda (stx)
           (define (lambda-form? rhs)
             (syntax-parse rhs
               #:literal-sets (kernel-literals)
               [(#%plain-lambda formals e body ...) #t]
               [(case-lambda [formals body ...] ...) #t]
               [_ #f]))
           (define (bind* bindpairs)
             (for ([bindpair (in-list (stx->list bindpairs))])
               (syntax-parse bindpair
                 [((x:id) rhs)
                  (when (lambda-form? #'rhs)
                    (free-id-table-set! FUN-EXP-table #'x (TAG #'rhs)))]
                 [_ (void)])))
           (syntax-parse stx
             #:literal-sets (kernel-literals)
             [(let-values ([vars rhs] ...) body ...)
              (bind* #'([vars rhs] ...))]
             [(letrec-values ([vars rhs] ...) body ...)
              (bind* #'([vars rhs] ...))]
             [_ (void)])))
  (void))


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
  (expression-fold
   stx
   #:replace
   (lambda (stx recur)
     (syntax-parse stx
       #:literal-sets (kernel-literals)
       [(letrec-values ([vars rhs] ...) body ...)
        (list* 'letrec-values
               (modfix (map recur (syntax->list #'(rhs ...))))
               (map recur (syntax->list #'(body ...))))]
       [_ #f]))
   #:reduce
   (lambda (rs) (ormap values rs))
   #:post
   (lambda (stx r)
     (syntax-parse stx
       #:literal-sets (kernel-literals)
       [(#%plain-lambda ~! . _)
        (begin (set-LAM-CALLS-ERP! stx r) #f)]
       [(case-lambda ~! . _)
        (begin (set-LAM-CALLS-ERP! stx r) #f)]
       ;; letrec-values -- FIXME, need fixed point
       [(#%plain-app f:id ~! . _)
        (define calls-erp? (fun-calls-erp? #'f))
        (begin (set-APP-CALLS-ERP! stx calls-erp?) (or r calls-erp?))]
       [(#%plain-app ~! . _)
        (begin (set-APP-CALLS-ERP! stx #t) #t)]
       [_ r]))))

(define (fun-calls-erp? id)
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
  (define replacements
    (for/fold ([replacements null]) ([x (in-list xs)])
      (cond [(free-id-table-ref model-function-table x #f)
             => (lambda (refs) (cons (cons x refs) replacements))]
            [else replacements])))
  (values (map car replacements) (map cadr replacements) (map caddr replacements)))

;; ============================================================

(define (free-variables expr
                        #:add [add? (lambda (id) #f)])
  (define free (make-free-id-table))
  (define free-ids null)
  (define (free! id)
    (unless (free-id-table-ref free id #f)
      (set! free-ids (cons id free-ids))
      (free-id-table-set! free id #t)))
  (define bound (make-free-id-table))
  (define (bound? id) (free-id-table-ref bound id #f))
  (expression-fold expr
                   #:pre
                   (lambda (stx)
                     (syntax-parse stx
                       #:literal-sets (kernel-literals)
                       [var:id
                        (when (and (not (bound? #'var)) (add? #'var))
                          (free! #'var))]
                       [(set! var e)
                        (when (and (not (bound? #'var)) (add? #'var))
                          (free! #'var))]
                       [_ (void)]))
                   #:bind
                   (lambda (xs)
                     (for ([x (in-list xs)])
                       (free-id-table-set! bound x #t))))
  (reverse free-ids))
