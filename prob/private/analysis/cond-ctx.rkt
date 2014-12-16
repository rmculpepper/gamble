;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-template racket/base)
         (for-syntax racket/base)
         syntax/stx
         syntax/parse
         "base.rkt")
(provide analyze-COND-CTX
         lam-cond-ctx?)

;; COND-CTX(e) is true if e might be evaluated in (tail position wrt)
;; a conditioning context.

;; Assume every lambda escapes and may be called in a CC *except*
;; let-bound lambdas---if all references are calls and all calls are
;; in non-CCs, then lambda body is non-CC.

(define (lam-cond-ctx? e)
  (hash-ref LAM-COND-CTX (TAG e)))

(define EXP-COND-CTX (make-hash))
(define LAM-COND-CTX (make-hash))

(define (set-EXP-COND-CTX! stx val)
  (hash-set/mod! EXP-COND-CTX (TAG stx) val))
(define (set-LAM-COND-CTX! stx val)
  ;; (eprintf "CC set to ~s for ~s: ~.s\n" val (TAG stx) (syntax->datum stx))
  (hash-set/mod! LAM-COND-CTX (TAG stx) val))
(define (set-fun-COND-CTX! id val)
  (cond [(FUN-EXP id #f)
         => (lambda (lam) 
              ;; (eprintf "CC set to ~s for ~s: ~.s\n"
              ;;          val lam (syntax->datum id))
              (hash-set/mod! LAM-COND-CTX lam val))]))

;; analyze-COND-CTX : Syntax -> Void
(define (analyze-COND-CTX stx0 cc?)
  ;; (eprintf "-- analyzing ~s\n" (TAG stx0))
  (define (recur e) (analyze-COND-CTX e cc?))
  (define (recur* es) (for-each recur (stx->list es)))
  (define (recur-cc e) (analyze-COND-CTX e #t))
  (define (recur-cc* es) (for-each recur-cc (stx->list es)))
  (define (recur-nt e) (analyze-COND-CTX e #f))
  (define (recur-nt* es) (for-each recur-nt (stx->list es)))
  (define (recur/bind rhs)
    (define lam-cc? (hash-ref LAM-COND-CTX (TAG rhs) #f))
    ;; (eprintf "-- recur/bind ~s with cc = ~s\n" (TAG rhs) lam-cc?)
    (set-LAM-COND-CTX! rhs lam-cc?)
    (syntax-parse rhs
      #:literal-sets (kernel-literals)
      [(#%plain-lambda formals e ... e*)
       (recur-nt* #'(e ...))
       (analyze-COND-CTX #'e* lam-cc?)]
      [(case-lambda [formals e ... e*] ...)
       (recur-nt* #'(e ... ...))
       (if lam-cc?
           (recur-cc* #'(e* ...))
           (recur-nt* #'(e* ...)))]
      [_ (recur-nt rhs)]))
  (define (recur/bind* bindpairs)
    (for ([bindpair (in-list (stx->list bindpairs))])
      (syntax-parse bindpair
        [((x:id) rhs) (recur/bind #'rhs)]
        [(_ rhs) (recur-nt #'rhs)])))
  (define stx (syntax-disarm stx0 stx-insp))
  (define (set-cond-ctx!)
    (set-EXP-COND-CTX! stx cc?))
  (define result
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      #:literals (+ cons reverse)
      ;; Fully-Expanded Programs
      ;; -- module body
      [(#%plain-module-begin form ...)
       (modfix (recur-nt* #'(form ...)))]
      ;; -- module-level form
      [(#%provide . _) (void)]
      [(begin-for-syntax . _) (void)]
      [(module . _) (void)]
      [(module* . _) (void)]
      [(#%declare . _) (void)]
      ;; -- general top-level form
      [(define-values ids e)
       (modfix (recur-nt #'e))]
      [(define-syntaxes . _) (void)]
      [(#%require . _) (void)]
      ;; -- expr
      [var:id
       ;; If ref to lam, lam escapes!
       (set-fun-COND-CTX! #'var #t)]
      [(#%plain-lambda formals e ... e*)
       ;; lambda expr not in let rhs assumed to escape
       (set-LAM-COND-CTX! stx #t)
       (recur-nt* #'(e ...))
       (recur-cc #'e*)]
      [(case-lambda [formals e ... e*] ...)
       ;; lambda expr not in let rhs assumed to escape
       (set-LAM-COND-CTX! stx #t)
       (recur-nt* #'(e ... ...))
       (recur-cc* #'(e* ...))]
      [(if e1 e2 e3)
       (recur* #'(e1 e2 e3))]
      [(begin e ... e*)
       (recur-nt* #'(e ...))
       (recur #'e*)]
      [(begin0 e ...) ;; FIXME: could CC e0
       (recur-nt* #'(e ...))]
      [(let-values ([vars rhs] ...) b ... b*)
       (recur/bind* #'([vars rhs] ...))
       (recur-nt* #'(b ...))
       (recur #'b*)]
      [(letrec-values ([vars rhs] ...) b ... b*)
       (modfix (recur/bind* #'([vars rhs] ...)))
       (recur-nt* #'(b ...))
       (recur #'b*)]
      [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) b ... b*)
       (modfix (recur/bind* #'([vvars vrhs] ...)))
       (recur-nt* #'(b ...))
       (recur #'b*)]
      [(set! var e)
       (recur-nt #'e)]
      [(quote d) (void)]
      [(quote-syntax s) (void)]
      [(with-continuation-mark e1 e2 e3)
       (recur-nt* #'(e1 e2))
       (recur #'e3)]
      ;; #%plain-app
      ;; Conditionable functions --- keep synced with instrument.rkt
      [(#%plain-app + e ... e*)
       (set-cond-ctx!)
       (recur-nt* #'(e ...))
       (recur #'e*)]
      [(#%plain-app reverse e*)
       (set-cond-ctx!)
       (recur #'e*)]
      [(#%plain-app cons e1 e2)
       (set-cond-ctx!)
       (recur* #'(e1 e2))]
      ;; Direct calls to local functions
      [(#%plain-app f:id e ...)
       (set-cond-ctx!)
       (recur-nt* #'(e ...))
       (when cc? (set-fun-COND-CTX! #'f #t))]
      ;; Other applications
      [(#%plain-app e ...)
       (set-cond-ctx!)
       (recur-nt* #'(e ...))]
      ;; ----
      [(#%top . _) (void)]
      [(#%variable-reference . _) (void)]
      [(#%expression e)
       (recur #'e)]
      [_
       (raise-syntax-error #f "unhandled syntax in analyze-COND-CTX" stx)]
      ))
  result)
