;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "instrument-analysis.rkt")
         racket/match
         "context.rkt")
(provide describe-all-call-sites
         describe-call-site
         instrumenting-module-begin
         instrumenting-top-interaction
         instrument
         next-counter)

(begin-for-syntax
  ;; display with: PLTSTDERR="info@instr" racket ....
  (define-logger instr))

#|
A CallSite is a Nat.

Call-site index determined dynamically, so distinct modules get
distinct call-site indexes.
|#

;; call-site-counter : Nat
(define call-site-counter 0)

;; call-site-table : hash[nat => sexpr describing source]
(define call-site-table (make-hash))

;; next-counter : any (List any any any) : -> Nat
(define (next-counter mod src-info)
  (set! call-site-counter (add1 call-site-counter))
  (hash-set! call-site-table call-site-counter (cons mod src-info))
  call-site-counter)

(define-syntax (fresh-call-site stx)
  (syntax-case stx ()
    [(fresh-call-site info)
     #'(#%plain-app next-counter
         (#%plain-app variable-reference->module-source (#%variable-reference))
         info)]))

(begin-for-syntax
  (define (lift-call-site stx)
    (with-syntax ([stx-file (syntax-source stx)]
                  [line (syntax-line stx)]
                  [col (syntax-column stx)])
      (syntax-local-lift-expression
       #`(fresh-call-site '(stx-file line col #,stx))))))

(define (describe-all-call-sites)
  (for ([i (in-range 1 (add1 call-site-counter))])
    (describe-call-site i)))

(define (describe-call-site n)
  (cond [(hash-ref call-site-table n #f)
         => (lambda (info)
              (match info
                [(list mod src line col stx)
                 (let ([mod (if (path? mod) (path->string mod) mod)]
                       [src (if (path? src) (path->string src) src)])
                   (printf "call site ~s: ~a:~a:~a\n"
                           n
                           (or src (and (not line) (not col) mod) "?")
                           (or line "?")
                           (or col "?")))]))]
        [else (printf "call site ~s: no info available" n)]))

;; ----

(define-syntax (instrumenting-module-begin stx)
  (syntax-case stx ()
    [(instrumenting-module-begin form ...)
     (with-syntax ([(_pmb e-form ...)
                    (analyze
                     (local-expand #'(#%plain-module-begin form ...)
                                   'module-begin
                                   null))])
       #'(#%module-begin (instrument e-form #:nt) ...))]))

(define-syntax (instrumenting-top-interaction stx)
  (syntax-case stx ()
    [(instrumenting-top-interaction . e)
     (let ([estx (local-expand #'(#%top-interaction . e) 'top-level #f)])
       (syntax-case estx (begin)
         [(begin form ...)
          #'(begin (instrumenting-top-interaction . form) ...)]
         [form
          (with-syntax ([e-form
                         (analyze
                          (local-expand #'form 'top-level null))])
            #'(instrument e-form #:nt))]))]))

(begin-for-syntax
 ;; Need privileged inspector to rewrite expanded code.
 (define stx-insp
   (variable-reference->module-declaration-inspector
    (#%variable-reference))))

;; (instrument expanded-form Mode)
;; where Mode is one of:
;;    #:cc - maybe tail wrt conditioning context in enclosing lambda
;;    #:nt - non-tail wrt conditioning context
(define-syntax (instrument stx0)
  (syntax-parse stx0
    [(instrument form-to-instrument m)
     (define stx (syntax-disarm #'form-to-instrument stx-insp))
     ;; Use recur-nt on non-tail sub-expressions of possible cc expression
     (define/with-syntax recur-nt
       (case (syntax->datum #'m)
         [(#:cc) #'recur-cc-to-nt]
         [(#:nt) #'recur-nt-to-nt]))
     (define instrumented
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         ;; Fully-Expanded Programs
         ;; Rewrite applications
         [(#%plain-app) stx]
         [(#%plain-app f e ...)
          #`(instrumenting-app m #,stx)]
         ;; Just recur through all other forms
         ;; -- module body
         [(#%plain-module-begin form ...)
          #'(#%plain-module-begin (instrument form #:nt) ...)]
         ;; -- module-level form
         [(#%provide . _) stx]
         [(begin-for-syntax . _) stx]
         [(module . _) stx]
         ;; [(module* . _) stx]
         ;; [(#%declare . _) stx]
         ;; -- general top-level form
         [(define-values ids e)
          #'(define-values ids (instrument e #:nt))]
         [(define-syntaxes . _) stx]
         [(#%require . _) stx]
         ;; -- expr
         [var:id #'var]
         [(#%plain-lambda formals e ... e*)
          (unless (lam-cond-ctx? stx)
            (log-instr-info "*** not CC: ~s: ~a"
                            (get-tag stx) (syntax-summary stx)))
          (with-syntax ([mode (if (lam-cond-ctx? stx) '#:cc '#:nt)])
            #'(#%plain-lambda formals (instrument e #:nt) ... (instrument e* mode)))]
         [(case-lambda [formals e ... e*] ...)
          (unless (lam-cond-ctx? stx)
            (log-instr-info "*** not CC: ~s: ~a"
                            (get-tag stx) (syntax-summary stx)))
          (with-syntax ([mode (if (lam-cond-ctx? stx) '#:cc '#:nt)])
            #'(case-lambda [formals (instrument e #:nt) ... (instrument e* mode)] ...))]
         [(if e1 e2 e3)
          #'(if (recur-nt e1) (instrument e2 m) (instrument e3 m))]
         [(begin e ... e*)
          #'(begin (recur-nt e) ... (instrument e* m))]
         [(begin0 e ...)
          #'(begin0 (recur-nt e) ...)]
         [(let-values ([vars rhs] ...) body ... body*)
          #'(let-values ([vars (recur-nt rhs)] ...)
              (recur-nt body) ... (instrument body* m))]
         [(letrec-values ([vars rhs] ...) body ... body*)
          #'(letrec-values ([vars (recur-nt rhs)] ...)
              (recur-nt body) ... (instrument body* m))]
         [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ... body*)
          #'(letrec-syntaxes+values ([svars srhs] ...) ([vvars (recur-nt vrhs)] ...)
              (recur-nt body) ... (instrument body* m))]
         [(set! var e)
          ;; (eprintf "** set! in expanded code: ~e" (syntax->datum stx))
          #'(set! var (recur-nt e))]
         [(quote d) stx]
         [(quote-syntax s) stx]
         [(with-continuation-mark e1 e2 e3)
          #'(with-continuation-mark (recur-nt e1) (recur-nt e2)
              (instrument e3 m))]
         ;; #%plain-app -- see above
         [(#%top . _) stx]
         [(#%variable-reference . _) stx]
         [(#%expression e)
          #'(#%expression (instrument e m))]
         [_ (raise-syntax-error #f "unhandled syntax in instrument" stx)]
         ))
     ;; Rearm and track result
     (let ([instrumented (relocate instrumented #'form-to-instrument)])
       (syntax-rearm (if (eq? stx instrumented)
                         stx
                         (syntax-track-origin instrumented stx #'instrument))
                     #'form-to-instrument))]))

(begin-for-syntax
  (define (relocate stx loc-stx)
    (if (identifier? stx)
        stx
        (datum->syntax stx (syntax-e stx) loc-stx stx))))

(define-syntax (recur-cc-to-nt stx)
  (syntax-parse stx
    [(_ e0)
     (let loop ([expr #'e0])
       (with-syntax ([e expr])
         (syntax-parse #'e
           #:literal-sets (kernel-literals)
           ;; FIXME: move this to analysis-time?
           ;; Simple cases; avoid inserting wrap-nt
           [(quote _) #'(instrument e #:nt)]
           [(quote-syntax _) #'(instrument e #:nt)]
           [var:id #'(instrument e #:nt)]
           [(#%plain-lambda . _) #'(instrument e #:nt)]
           [(begin inner) (loop #'inner)] ;; FIXME: stx-track?
           [(#%expression inner) (loop #'inner)] ;; FIXME: stx-track?
           [(begin (quote-syntax _) (#%plain-app values))
            ;; HACK: a TR `:' annotation expands into this pattern; no need to annotate,
            ;; and annotation just confuses TR, so avoid (FIXME: better solution?)
            #'e]
           ;; Normal case: need to turn off observing
           [e #'(wrap-nt (instrument e #:nt))])))]))

(define-syntax (recur-nt-to-nt stx)
  (syntax-case stx ()
    [(_ e) #'(instrument e #:nt)]))

;; ----

;; App needs to do 2 transformations
;;  - address-tracking (unless "safe" function)
;;  - conditioning-tracking (if conditionable AND tail wrt conditioning context)
;; Conditionable functions are a subset of safe functions,
;; so split that way.

(define-syntax (instrumenting-app iastx)
  (define stx (syntax-case iastx () [(_ m stx) #'stx]))
  (define calls-erp? (app-calls-erp? stx))
  (when #f
    (eprintf "~a ~s:~s ~.s\n"
             (if calls-erp? "yes" "no ")
             (syntax-line stx)
             (syntax-column stx)
             (syntax->datum stx)))
  (syntax-parse iastx
    #:literals (#%plain-app + cons reverse)
    ;; Conditionable functions
    [(_ #:cc (#%plain-app + e ... eFinal))
     (with-syntax ([(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (with-continuation-mark obs-mark (+ tmp ...)
             (#%plain-app + tmp ... (instrument eFinal #:cc)))))]
    [(_ #:cc (#%plain-app cons e1 e2))
     #'(with-continuation-mark obs-mark 'car
         (let-values ([(tmp1) (wrap-cc (instrument e1 #:cc))])
           (with-continuation-mark obs-mark 'cdr
             (let ([tmp2 (wrap-cc (instrument e2 #:cc))])
               (#%plain-app cons tmp1 tmp2)))))]
    [(_ #:cc (#%plain-app reverse e))
     #'(with-continuation-mark obs-mark 'reverse
         (#%plain-app reverse (wrap-cc (instrument e #:cc))))]

    ;; Non-conditionable functions in conditionable contexts
    ;; Put the function into the CC marks (for debugging).
    [(_ #:cc (#%plain-app f:id e ...))
     #:when (not calls-erp?)
     ;; "safe" function: doesn't need address tracking
     (when (not (eq? (classify-function #'f) 'safe))
       (log-instr-info "*** no calls-erp: ~a" (syntax-summary #'f)))
     #'(#%plain-app f (wrap-nt (instrument e #:nt)) ...)]
    [(_ #:cc (#%plain-app f:id e ...))
     (with-syntax ([c (lift-call-site stx)])
       #'(app/call-site c f (wrap-nt (instrument e #:nt)) ...))]
    [(_ #:cc (#%plain-app e ...))
     (with-syntax ([c (lift-call-site stx)])
       #'(app/call-site c (wrap-nt (instrument e #:nt)) ...))]

    ;; Non-conditionable contexts
    [(_ #:nt (#%plain-app f:id e ...))
     #:when (not calls-erp?)
     ;; "safe" function: doesn't need address tracking
     (when (not (eq? (classify-function #'f) 'safe))
       (log-instr-info "*** no calls-erp: ~a" (syntax-summary #'f)))
     #'(#%plain-app f (instrument e #:nt) ...)]
    [(_ #:nt (#%plain-app f:id e ...))
     (with-syntax ([c (lift-call-site stx)])
       #'(app/call-site c f (instrument e #:nt) ...))]
    [(_ #:nt (#%plain-app f e ...))
     (with-syntax ([c (lift-call-site stx)])
       #'(app/call-site c (instrument f #:nt) (instrument e #:nt) ...))]))

;; wrap-cc : wrapped around CC args to CC function call
(define-syntax (wrap-cc stx)
  (syntax-case stx ()
    [(wrap-cc e)
     #'(with-continuation-mark obs-mark 'unknown e)]))

;; wrap-nt : wrapped around NT args to CC function call
(define-syntax (wrap-nt stx)
  (syntax-case stx ()
    [(wrap-nt e)
     #'(parameterize ((observing? '#f)) e)]))

;; Application w/ call-site added to context parameter.
(define-syntax (app/call-site stx)
  (syntax-parse stx
    [(app/call-site call-site f:id arg ...)
     ;; Special case where f is varref: keep f in operator position
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #`(let-values ([(tmp-arg) arg] ...)
           (parameterize ((the-context (#%plain-app cons call-site (#%plain-app the-context))))
             (#%plain-app f tmp-arg ...))))]
    [(app/call-site call-site f arg ...)
     (with-syntax ([(tmp-f)
                    (generate-temporaries #'(f))]
                   [(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #`(let-values ([(tmp-f) f] [(tmp-arg) arg] ...)
           (parameterize ((the-context (#%plain-app cons call-site (#%plain-app the-context))))
             (#%plain-app tmp-f tmp-arg ...))))]))
