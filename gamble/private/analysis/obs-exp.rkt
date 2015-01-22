;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-template racket/base)
         syntax/stx
         syntax/parse
         "base.rkt"
         "known-functions.rkt")
(provide OBS-EXP
         OBS-LAM
         analyze-OBS-EXP)

;; OBS-EXP(e) is true if a call to sample MAY occur in an observable
;; context wrt the evaluation of e.

;; OBS-LAM((lambda args e)) is true iff OBS-EXP(e) is true.

;; Note: Actually, only record value for app and lambda.

;; Uses:
;; - if OBS-EXP(e) = false, then (observe e v) should raise an error

;; TODO:
;; - It would be nice to be able to analyze through mem: eg,
;;   (mem (lambda () (normal))) is OBS-LAM, but (mem (lambda () 3)) is not.
;; - Export analysis summary so importing modules can use.

(define-ref/set OBS-EXP)
(define-ref/set OBS-LAM)

;; OBS-FUN : Syntax -> Boolean
(define (OBS-FUN f)
  (cond [(lambda-form? f)
         (OBS-LAM f)]
        [(syntax-case f (#%top)
           [(#%top . var) #'var]
           [_ #f])
         => OBS-FUN]
        [(and (identifier? f) (FUN-EXP f))
         => OBS-LAM]
        [(and (identifier? f)
              (eq? (classify-function f) 'non-random-first-order))
         #f]
        ;; Note: imprecision introduced here: default is #t for MAY analysis.
        [else #t]))

;; analyze-OBS-EXP : Syntax -> Boolean
;; Updates OBS-EXP-table with analysis of given term.
;; Returns whether sample MAY be called in an observable context wrt e.
(define (analyze-OBS-EXP stx0)
  (define (recur e) (analyze-OBS-EXP e))
  (define (recur* es) (strict-ormap recur (stx->list es)))
  (define stx (syntax-disarm stx0 stx-insp))
  (define result
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      ;; Fully-Expanded Programs
      ;; -- module body
      [(#%plain-module-begin form ...)
       (modfix (recur* #'(form ...)))
       #f]
      ;; -- module-level form
      [(#%provide . _) #f]
      [(begin-for-syntax . _) #f]
      [(module . _) #f]
      [(module* . _) #f]
      [(#%declare . _) #f]
      ;; -- general top-level form
      [(define-values ids e)
       (modfix (recur #'e))
       #f]
      [(define-syntaxes . _) #f]
      [(#%require . _) #f]
      ;; -- expr
      [var:id
       #f]
      [(#%plain-lambda formals e ...)
       (let ([body-calls? (recur* #'(e ...))])
         (OBS-LAM-set! stx body-calls?))
       #f]
      [(case-lambda [formals e ...] ...)
       (let ([body-calls? (recur* #'(e ... ...))])
         (OBS-LAM-set! stx body-calls?))
       #f]
      [(if e1 e2 e3)
       (recur #'e1)
       ;; Note: MAY, not MUST
       (strict-or (recur #'e2) (recur #'e3))]
      [(begin e ... e*)
       (recur* #'(e ...))
       (recur #'e*)]
      [(begin0 e0 e ...)
       (begin0 (recur #'e0)
         (recur* #'(e ...)))]
      [(let-values ([vars rhs] ...) body ... body*)
       (recur* #'(rhs ... body ...))
       (recur #'body*)]
      [(letrec-values ([vars rhs] ...) body ... body*)
       (modfix (recur* #'(rhs ...)))
       (recur* #'(body ...))
       (recur #'body*)]
      [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ... body*)
       (modfix (recur* #'(vrhs ...)))
       (recur* #'(body ...))
       (recur #'body*)]
      [(set! var e)
       (recur #'e)
       #f]
      [(quote d) #f]
      [(quote-syntax s) #f]
      [(with-continuation-mark e1 e2 e3)
       (recur* #'(e1 e2))
       (recur #'e3)]
      ;; ----------------------------------------
      ;; FIXME: use common declaration for analysis and instrumentation
      ;; rather than duplicating knowledge and code
      [(#%plain-app (~literal +) e ... e*)
       (recur* #'(e ...))
       (define obs? (recur #'e*))
       (OBS-EXP-set! stx obs?)
       obs?]
      [(#%plain-app (~literal cons) e1 e2)
       (define obs? (strict-or (recur #'e1) (recur #'e2)))
       (OBS-EXP-set! stx obs?)
       obs?]
      [(#%plain-app (~literal reverse) e)
       (define obs? (recur #'e))
       (OBS-EXP-set! stx obs?)
       obs?]
      ;; ----------------------------------------
      [(#%plain-app f e ...)
       (recur* #'(f e ...))
       (define obs? (OBS-FUN #'f))
       (OBS-EXP-set! stx obs?)
       obs?]
      ;; ----------------------------------------
      [(#%top . _) #f]
      [(#%variable-reference . _) #f]
      [(#%expression e)
       (recur #'e)]
      [_
       (raise-syntax-error #f "unhandled syntax in analyze-OBS-EXP" stx)]
      ))
  result)

(define (strict-or x y) (or x y))
(define (strict-ormap f xs) (ormap values (map f xs)))
