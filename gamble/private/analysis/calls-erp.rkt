;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-template racket/base)
         syntax/stx
         syntax/parse
         "base.rkt"
         "known-functions.rkt")
(provide app-calls-erp?
         analyze-CALLS-ERP)

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
(define (analyze-CALLS-ERP stx0)
  (define (recur e) (analyze-CALLS-ERP e))
  (define (recur* es) (strict-ormap recur (stx->list es)))
  (define stx (syntax-disarm stx0 stx-insp))
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

(define (fun-calls-erp? id)
  ;; conservative: #t if unknown function
  (cond [(FUN-EXP id #f)
         => (lambda (lam-tag)
              (hash-ref LAM-CALLS-ERP lam-tag #f))]
        [else (imported-fun-calls-erp? id)]))

(define (imported-fun-calls-erp? f-id)
  (case (classify-function f-id)
    [(non-random-first-order) #f]
    [else #t]))
