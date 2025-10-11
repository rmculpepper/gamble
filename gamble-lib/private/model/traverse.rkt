;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base
(require (for-template racket/base)
         racket/match
         syntax/id-table
         syntax/stx
         syntax/parse
         syntax/parse/experimental/template)
(provide (all-defined-out))

;; ----------------------------------------

(define (make-expression-traverser
         #:bind [bind #f]           ;; (Listof Identifier) -> Void
         #:pre [pre #f]             ;; Syntax -> Void
         #:replace [replace #f]     ;; Syntax -> (U Syntax #f) -- replace recur
         #:post [post #f])          ;; Syntax Syntax -> Syntax
  (lambda (stx)
    (expression-traverse stx #:bind bind #:pre pre #:replace replace #:post post)))

(define (expression-traverse stx
                             #:bind [bind #f]          ;; (Listof Identifier) -> Void
                             #:pre [pre #f]            ;; Syntax -> Void
                             #:replace [replace #f]    ;; Syntax -> (U Syntax #f)
                             #:post [post #f])         ;; Syntax Syntax -> Syntax
  (define (bind-flatten stx)
    (when bind (bind (flatten-identifiers stx))))
  (define (loop stx)
    (when pre (pre stx))
    (define processed-stx
      (cond [(and replace (replace stx loop))
             => values]
            [else
             (define-template-metafunction recur
               (syntax-parser [(recur e) (loop #'e)]))
             (define-syntax-rule (T tmpl)
               (relocate (syntax tmpl) stx))
             (syntax-parse stx
               #:literal-sets (kernel-literals)
               [(kw:#%plain-lambda ~! formals e ...)
                (bind-flatten #'formals)
                (T (kw formals (recur e) ...))]
               [(kw:case-lambda ~! [formals e ...] ...)
                (bind-flatten #'(formals ...))
                (T (kw [formals (recur e) ...] ...))]
               [(kw:if ~! e1 e2 e3)
                (T (kw (recur e1) (recur e2) (recur e3)))]
               [(kw:begin ~! e ...)
                (T (kw (recur e) ...))]
               [(kw:begin0 ~! e ...)
                (T (kw (recur e) ...))]
               [(kw:let-values ~! ([vars rhs] ...) body ...)
                (bind-flatten #'(vars ...))
                (T (kw ([vars (recur rhs)] ...) (recur body) ...))]
               [(kw:letrec-values ~! ([vars rhs] ...) body ...)
                (bind-flatten #'(vars ...))
                (T (kw ([vars (recur rhs)] ...) (recur body) ...))]
               [(kw:set! ~! var e)
                (T (kw var (recur e)))]
               [(kw:with-continuation-mark ~! e1 e2 e3)
                (T (kw (recur e1) (recur e2) (recur e3)))]
               [(kw:#%plain-app ~! f e ...)
                (T (kw (recur f) (recur e) ...))]
               [(kw:#%expression ~! e)
                (T (kw (recur e)))]
               [_ stx])]))
    (if post (post processed-stx stx) processed-stx))
  (loop stx))

;; ----------------------------------------

(define (expression-fold stx
                         #:bind [bind #f]        ;; (Listof Identifier) -> Void
                         #:pre [pre #f]          ;; Syntax -> Void
                         #:replace [replace #f]  ;; Syntax (Syntax -> (Tree X)) -> (Tree X)
                         #:reduce [reduce0 #f]   ;; (Listof X) -> X
                         #:fold [fold0 #f]       ;; (Tree X) -> X
                         #:post [post #f])       ;; Syntax X -> X
  (define (bind-flatten stx)
    (when bind (bind (flatten-identifiers stx))))
  (define (reduce rs) (if reduce0 (reduce0 rs) (void)))
  (define (fold v)
    (if fold0
        (fold0 v)
        (match v
          [(list* '#%plain-lambda rs) (reduce rs)]
          [(list* 'case-lambda rss) (reduce (map reduce rss))]
          [(list* 'if rs) (reduce rs)]
          [(list* 'begin rs) (reduce rs)]
          [(list* 'begin0 rs) (reduce rs)]
          [(list* 'let-values rhs-rs body-rs)
           (reduce (list (reduce rhs-rs) (reduce body-rs)))]
          [(list* 'letrec-values rhs-rs body-rs)
           (reduce (list (reduce rhs-rs) (reduce body-rs)))]
          [(list* 'with-continuation-mark rs) (reduce rs)]
          [(list* '#%plain-app rs) (reduce rs)]
          [(list 'set! r) r]
          [(list '#%expression r) r]
          ['(variable) (reduce null)]
          ['(quote) (reduce null)]
          ['(quote-syntax) (reduce null)]
          ['(#%top) (reduce null)]
          ['(#%variable-reference) (reduce null)]
          [(list 'just r) r])))
  (define (loop* es)
    (map loop (syntax->list es)))
  (define (loop stx)
    (when pre (pre stx))
    (define result
      (cond [(and replace (replace stx loop))
             => values]
            [else
             (syntax-parse stx
               #:literal-sets (kernel-literals)
               [var:id (fold '(variable))]
               [(#%plain-lambda ~! formals e ...)
                (bind-flatten #'formals)
                (fold (list* '#%plain-lambda (loop* #'(e ...))))]
               [(case-lambda ~! [formals e ...] ...)
                (bind-flatten #'(formals ...))
                (fold (list* 'case-lambda (map loop* (syntax->list #'((e ...) ...)))))]
               [(if ~! e1 e2 e3)
                (fold (list* 'if (loop* #'(e1 e2 e3))))]
               [(begin ~! e ...)
                (fold (list* 'begin (loop* #'(e ...))))]
               [(begin0 ~! e ...)
                (fold (list* 'begin0 (loop* #'(e ...))))]
               [(let-values ~! ([vars rhs] ...) body ...)
                (bind-flatten #'(vars ...))
                (fold (list* 'let-values (loop* #'(rhs ...)) (loop* #'(body ...))))]
               [(letrec-values ~! ([vars rhs] ...) body ...)
                (bind-flatten #'(vars ...))
                (fold (list* 'letrec-values (loop* #'(rhs ...)) (loop* #'(body ...))))]
               [(set! ~! var e)
                (fold (list 'set! (loop #'e)))]
               [(with-continuation-mark ~! e1 e2 e3)
                (fold (list* 'with-continuation-mark (loop* #'(e1 e2 e3))))]
               [(#%plain-app ~! f e ...)
                (fold (list* '#%plain-app (loop* #'(f e ...))))]
               [(#%expression ~! e)
                (fold (list '#%expression (loop #'e)))]
               [(quote ~! . _)
                (fold '(quote))]
               [(quote-syntax ~! . _)
                (fold '(quote-syntax))]
               [(#%top ~! . _)
                (fold '(#%top))]
               [(#%variable-reference ~! . _)
                (fold '(#%variable-reference))])]))
    (if post (post stx result) result))
  (loop stx))

(define (make-expression-folder
         #:bind [bind #f]           ;; (Listof Identifier) -> Void
         #:pre [pre #f]             ;; Syntax -> Void
         #:replace [replace #f]     ;; Syntax (Syntax -> (Tree X)) -> (Tree X)
         #:reduce [reduce #f]       ;; (Listof X) -> X
         #:fold [fold0 #f]          ;; (Tree X) -> X
         #:post [post #f])          ;; Syntax X -> X
  (lambda (stx)
    (expression-fold stx
                     #:bind bind #:pre pre #:replace replace #:reduce reduce
                     #:fold fold0 #:post post)))

;; ----------------------------------------

(define (flatten-identifiers stx)
  (let loop ([stx stx] [onto null])
    (cond [(identifier? stx) (cons stx onto)]
          [(syntax? stx) (loop (syntax-e stx) onto)]
          [(pair? stx) (loop (car stx) (loop (cdr stx) onto))]
          [else onto])))

(define (relocate stx loc-stx)
  (datum->syntax stx (syntax-e stx) loc-stx loc-stx))
