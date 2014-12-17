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
         syntax/parse/experimental/template)
(provide (protect-out stx-insp)

         define-ref/set

         modfix
         inc-mod-counter!
         hash-set/mod!

         analyze-TAG
         TAG
         syntax-summary

         analyze-FUN-EXP
         FUN-EXP
         lambda-form?)

;; Base utilities for other analysis passes.

;; ============================================================

;; Need privileged inspector to rewrite expanded code.
(define stx-insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

;; ============================================================

;; Modification counter (used to find fixed points)

(define mod-counter 0)

(define (inc-mod-counter!)
  (set! mod-counter (add1 mod-counter)))

(define (hash-set/mod! h k v)
  (define old-val (hash-ref h k NONE)) ;; FIXME: use gensym?
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

(define NONE (gensym 'none))

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

(define TAG-table (make-hash))

(define (new-tag [stx #f])
  (set! tag-counter (add1 tag-counter))
  (hash-set! TAG-table tag-counter stx)
  tag-counter)

(define TAG
  (case-lambda
    [(stx)
     (or (syntax-property stx 'tag)
         (error 'TAG "no tag for: ~a\n" (syntax-summary stx)))]
    [(stx default)
     (or (syntax-property stx 'tag)
         (if (procedure? default) (default) default))]))

;; analyze-TAG : Syntax -> Syntax
;; Add unique tags to all forms under 'tag syntax-property.
(define (analyze-TAG stx0)
  (define-template-metafunction recur
    (syntax-parser [(recur e) (analyze-TAG #'e)]))
  (define-syntax-rule (T tmpl)
    (relocate (template tmpl) stx0))
  (define stx (syntax-disarm stx0 stx-insp))
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
       (raise-syntax-error #f "unhandled syntax in analyze-TAG" stx)]
      ))
  ;; Rearm and track result
  (syntax-rearm
   (syntax-property processed-stx 'tag the-tag)
   stx0))

(define (relocate stx loc-stx)
  (datum->syntax stx (syntax-e stx) loc-stx loc-stx))

(define (syntax-summary stx)
  (format "~s:~s ~.s" (syntax-line stx) (syntax-column stx) (syntax->datum stx)))

;; ============================================================

;; FUN-EXP : Identifier -> Nat/#f
;; Indicates (tag of) lambda expr bound to id, #f for not lambda.

;; FUN-EXP-table : (id-table Id => Nat/#f)
(define FUN-EXP-table (make-free-id-table))

(define (FUN-EXP id [default #f])
  (free-id-table-ref FUN-EXP-table id default))

;; analyze-FUN-EXP : Syntax -> Void
(define (analyze-FUN-EXP stx0)
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
  (define stx (syntax-disarm stx0 stx-insp))
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
    [(quote-syntax s) #f]
    [(with-continuation-mark e1 e2 e3)
     (recur* #'(e1 e2 e3))]
    [(#%plain-app e ...)
     (recur* #'(e ...))]
    [(#%top . _) (void)]
    [(#%variable-reference . _) (void)]
    [(#%expression e)
     (recur #'e)]
    [_
     (raise-syntax-error #f "unhandled syntax in analyze-FUN-EXP" stx)]
    ))

(define (lambda-form? rhs)
  (syntax-parse rhs
    #:literal-sets (kernel-literals)
    [(#%plain-lambda formals e body ...) #t]
    [(case-lambda [formals body ...] ...) #t]
    [_ #f]))
