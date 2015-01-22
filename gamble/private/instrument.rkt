;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/id-table
                     "analysis/base.rkt"
                     "analysis/known-functions.rkt"
                     "analysis/calls-erp.rkt"
                     "analysis/obs-exp.rkt"
                     "analysis/cond-ctx.rkt")
         racket/match
         (only-in racket/contract/private/provide
                  contract-rename-id-property
                  provide/contract-info?
                  provide/contract-info-original-id)
         "instrument-data.rkt"
         "interfaces.rkt"
         "context.rkt")
(provide describe-all-call-sites
         describe-call-site
         instrumenting-module-begin
         instrumenting-top-interaction
         begin-instrumented
         instrument/local-expand
         (for-syntax analyze)
         instrument
         next-counter
         declare-observation-propagator)

(begin-for-syntax
  ;; analyze : Syntax -> Syntax
  (define (analyze stx)
    (define tagged-stx (analyze-TAG stx))
    (analyze-FUN-EXP tagged-stx)
    (analyze-CALLS-ERP tagged-stx)
    (analyze-COND-CTX tagged-stx #f)
    (analyze-OBS-EXP tagged-stx)
    tagged-stx))

(begin-for-syntax
  ;; display with: PLTSTDERR="info@instr" racket ....
  (define-logger instr))

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
                  [col (syntax-column stx)]
                  [fun (syntax-case stx (#%plain-app)
                         [(#%plain-app f arg ...) (identifier? #'f) #'f]
                         [_ #f])])
      (syntax-local-lift-expression
       #`(fresh-call-site '(stx-file line col #,stx f))))))

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

(define-syntax (begin-instrumented stx)
  (syntax-case stx ()
    [(begin-instrumented form ...)
     #'(instrument/local-expand (begin form ...))]))

(define-syntax (instrument/local-expand stx)
  (syntax-case stx ()
    [(instrument/local-expand form)
     (case (syntax-local-context)
       [(expression)
        (with-syntax ([e-form (analyze (local-expand #'form 'expression null))])
          #'(instrument e-form #:nt))]
       [else ;; module, top-level
        (let ([e-form (local-expand #'form 'module #f)])
          (syntax-parse e-form
            #:literal-sets (kernel-literals)
            [(define-values ids rhs)
             #'(define-values ids (instrument/local-expand rhs))]
            [(define-syntaxes . _) e-form]
            [(#%require . _) e-form]
            [(#%provide . _) e-form]
            [(#%declare . _) e-form]
            [(module . _) e-form]
            [(module* . _) e-form]
            [(begin form ...)
             #'(begin (instrument/local-expand form) ...)]
            [expr
             #'(#%expression (instrument/local-expand expr))]))])]))

(begin-for-syntax

 ;; instr-fun-table : (free-id-table Id => Id)
 (define instr-fun-table
   (make-free-id-table))

 (define (register-instrumented-fun! id id* arity)
   (free-id-table-set! instr-fun-table id (cons id* arity)))

 (define-syntax-class instr-fun
   #:attributes (instr arity)
   (pattern f:id
            #:do [(define p (free-id-table-ref instr-fun-table #'f #f))]
            #:when p
            #:with instr (car p)
            #:attr arity (cdr p)))
 )

;; (instrument expanded-form Mode)
;; where Mode is one of:
;;    #:cc - in observing context wrt enclosing lambda
;;    #:nt - not in observing context wrt enclosing lambda
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
          #`(instrument-app m #,stx)]
         ;; -- module body
         [(#%plain-module-begin form ...)
          #'(#%plain-module-begin (instrument form #:nt) ...)]
         ;; -- module-level form
         [(#%provide . _) stx]
         [(begin-for-syntax . _) stx]
         [(module . _) stx]
         [(module* . _)
          (raise-syntax-error #f "submodule not allowed within `#lang gamble' module" stx)]
         [(#%declare . _) stx]
         ;; -- general top-level form
         [(define-values ids e)
          #`(instrument-definition #,stx)]
         [(define-syntaxes . _) stx]
         [(#%require . _) stx]
         ;; -- expr
         [var:id #'var]
         [(#%plain-lambda formals e ... e*)
          (cond [(lam-cond-ctx? stx)
                 #'(#%plain-lambda formals
                     (call-with-immediate-continuation-mark OBS-mark
                       (lambda (obs)
                         (with ([OBS obs] [ADDR (ADDR-mark)])
                           (instrument e #:nt) ... (instrument e* #:cc)))))]
                [else
                 (log-instr-info "NON-CC lambda: ~s: ~a"
                                 (TAG stx) (syntax-summary stx))
                 #'(#%plain-lambda formals
                     (with ([OBS #f] [ADDR (ADDR-mark)])
                       (instrument e #:nt) ... (instrument e* #:nt)))])]
         [(case-lambda [formals e ... e*] ...)
          (cond [(lam-cond-ctx? stx)
                 #'(case-lambda
                     [formals
                      (call-with-immediate-continuation-mark OBS-mark
                        (lambda (obs)
                          (with ([OBS obs] [ADDR (ADDR-mark)])
                            (instrument e #:nt) ... (instrument e* #:cc))))]
                     ...)]
                [else
                 (log-instr-info "NON-CC case-lambda: ~s: ~a"
                                 (TAG stx) (syntax-summary stx))
                 #'(case-lambda
                     [formals
                      (with ([OBS #f] [ADDR (ADDR-mark)])
                        (instrument e #:nt) ... (instrument e* #:nt))]
                     ...)])]
         [(if e1 e2 e3)
          #'(if (recur-nt e1)
                (instrument e2 m)
                (instrument e3 m))]
         [(begin e*)
          #'(begin (instrument e* m))]
         [(begin e ... e*)
          #'(begin (recur-nt (begin e ...))
                   (instrument e* m))]
         [(begin0 e0)
          #'(begin0 (instrument e0 m))]
         [(begin0 e0 e ...)
          #'(begin0 (instrument e0 m)
              (recur-nt (begin e ...)))]
         [(let-values ([vars rhs] ...) body ... body*)
          ;; HACK: okay to turn let-values into intdef (letrec) because
          ;; already expanded, thus "alpha-renamed", so no risk of capture
          #'(let ()
              (instrument (define-values vars rhs) #:nt) ...
              (#%expression (recur-nt body)) ...
              (#%expression (instrument body* m)))]
         [(letrec-values ([vars rhs] ...) body ... body*)
          #'(let ()
              (instrument (define-values vars rhs) #:nt) ...
              (#%expression (recur-nt body)) ...
              (#%expression (instrument body* m)))]
         [(letrec-syntaxes+values ([svars srhs] ...) ([vvars vrhs] ...) body ... body*)
          #'(let ()
              (define-syntaxes svars srhs) ...
              (instrument (define-values vvars vrhs) #:nt) ...
              (#%expression (recur-nt body)) ...
              (#%expression (instrument body* m)))]
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

(define-syntax (instrument-definition idstx)
  (syntax-parse idstx
    #:literals (define-values)
    [(_ (define-values (var:id) e))
     (syntax-parse (syntax-disarm #'e stx-insp)
       #:literals (#%plain-lambda case-lambda)
       ;; FIXME: handle rest args
       [(#%plain-lambda (arg ...) body ... body*)
        (define/with-syntax (var*) (generate-temporaries #'(var)))
        (define/with-syntax arity (length (syntax->list #'(arg ...))))
        #'(begin (define-values (var*)
                   (#%plain-lambda (addr obs arg ...)
                     (with ([ADDR addr] [OBS obs])
                       (instrument body #:nt) ...
                       (instrument body* #:cc))))
                 (define-values (var)
                   (#%plain-lambda (arg ...)
                     (call-with-immediate-continuation-mark OBS-mark
                       (lambda (obs)
                         (var* (ADDR-mark) obs arg ...)))))
                 (begin-for-syntax*
                  (register-instrumented-fun!
                   (quote-syntax var)
                   (quote-syntax var*)
                   'arity)))]
       ;; FIXME: handle case-lambda
       [_
        #'(define-values (var) (instrument e #:nt))])]
    [(_ (define-values vars e))
     #'(define-values vars (instrument e #:nt))]))

(define-syntax (begin-for-syntax* stx)
  (syntax-case stx ()
    [(_ expr ...)
     (case (syntax-local-context)
       [(module top-level)
        #'(begin-for-syntax expr ...)]
       [else
        #'(define-syntaxes () (begin expr ... (values)))])]))

;; ------------------------------------------------------------

(begin-for-syntax

  (define-syntax-class final-arg-prop-fun
    #:attributes (pred inverter scaler)
    (pattern f:id
             #:with (#:final-arg pred inverter scaler)
                    (free-id-table-ref observation-propagators #'f #f)))
  (define-syntax-class all-args-prop-fun
    #:attributes (pred [inverter 1])
    (pattern f:id
             #:with (#:all-args pred (inverter ...))
                    (free-id-table-ref observation-propagators #'f #f)))

  (define-syntax-class nrfo-fun
    (pattern f:id
             #:when (or (eq? (classify-function #'f) 'non-random-first-order)
                        (free-id-table-ref non-random-first-order-funs #'f #f))))

  ;; Suppose contracted function f st (f e ...) => (f* blah e ...)
  ;; then the f* identifier has a property contract-rename-id-property
  ;; containing f-ren, where (syntax-local-value f-ren) is a
  ;; provide/contract-info structure.

  (define-syntax-class contracted-export-id
    #:attributes (original-id)
    (pattern (~var x (static provide/contract-info? 'contracted-function))
             #:with original-id (provide/contract-info-original-id (attribute x.value))))

  (define-syntax-class contract-indirection-id
    (pattern x:id
             #:with c:contracted-export-id (contract-rename-id-property #'x)
             #:with original-id #'c.original-id))

  (define-syntax-class contracted-final-arg-prop-fun
    #:attributes (pred inverter scaler)
    (pattern c:contract-indirection-id
             #:when (not (regexp-match? #rx"^lifted\\." (symbol->string (syntax-e #'c))))
             #:with :final-arg-prop-fun #'c.original-id))

  ;; The other contract expansion form:
  ;; Suppose contracted function f st (f e ...) => (lifted.f e ...)
  ;; then the lifted.f identifier has a property contract-rename-id-property
  ;; containing f-ren, ....

  (define-syntax-class lifted-contracted-final-arg-prop-fun
    #:attributes (pred inverter scaler)
    (pattern c:contract-indirection-id
             #:when (regexp-match? #rx"^lifted\\." (symbol->string (syntax-e #'c)))
             #:with :final-arg-prop-fun #'c.original-id))
  )

;; App needs to do 2 transformations
;;  - address-tracking (unless "safe" function)
;;  - conditioning-tracking (if conditionable AND tail wrt conditioning context)
;; Conditionable functions are a subset of safe functions,
;; so split that way.

(define-syntax (instrument-app iastx)
  (define stx (syntax-case iastx () [(_ m stx) #'stx]))
  (define (log-app-type msg f)
    (log-instr-info (format "~a for ~s" msg f)))
  (check-app stx)
  (syntax-parse iastx
    #:literals (#%plain-app)

    ;; Conditionable primitives in conditionable context
    ;; All non-random first-order, so no need for address tracking.

    [(_ #:cc (#%plain-app op:final-arg-prop-fun e ... eFinal))
     (log-app-type "OBS PROP app (final arg)" #'op)
     (with-syntax ([(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (#%plain-app op tmp ...
             (with ([OBS
                     (if OBS
                         (let ([obs-v (observation-value OBS)])
                           (if (op.pred obs-v tmp ...)
                               (let* ([x (op.inverter obs-v tmp ...)]
                                      [scale (op.scaler x tmp ...)])
                                 (observation x (* (observation-scale OBS) scale)))
                               (fail 'observe-failed-invert)))
                         #f)])
                   (instrument eFinal #:cc)))))]

    ;; contracted
    [(_ #:cc (#%plain-app op:contracted-final-arg-prop-fun ctc-info e ... eFinal))
     (log-app-type "OBS PROP app (contracted, final arg)" #'op)
     (with-syntax ([(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (#%plain-app op ctc-info tmp ...
             (with ([OBS
                     (if OBS
                         (let ([obs-v (observation-value OBS)])
                           (if (op.pred obs-v tmp ...)
                               (let* ([x (op.inverter obs-v tmp ...)]
                                      [scale (op.scaler x tmp ...)])
                                 (observation x (* (observation-scale OBS) scale)))
                               (fail 'observe-failed-invert)))
                         #f)])
                   (instrument eFinal #:cc)))))]

    ;; lifted contracted (w/o ctc-info arg)
    [(_ #:cc (#%plain-app op:lifted-contracted-final-arg-prop-fun e ... eFinal))
     (log-app-type "OBS PROP app (lifted contracted, final arg)" #'op)
     (with-syntax ([(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (#%plain-app op tmp ...
             (with ([OBS
                     (if OBS
                         (let ([obs-v (observation-value OBS)])
                           (if (op.pred obs-v tmp ...)
                               (let* ([x (op.inverter obs-v tmp ...)]
                                      [scale (op.scaler x tmp ...)])
                                 (observation x (* (observation-scale OBS) scale)))
                               (fail 'observe-failed-invert)))
                         #f)])
                   (instrument eFinal #:cc)))))]

    [(_ #:cc (#%plain-app op:all-args-prop-fun e ...))
     (log-app-type "OBS PROP app (all args)" #'op)
     #'(#%plain-app op
         (with ([OBS
                 (if OBS
                     (let ([obs-v (observation-value OBS)])
                       (if (op.pred obs-v) ;; FIXME: redundant for 2nd arg on
                           (let ([x (op.inverter (observation-value OBS))])
                             (observation x (observation-scale OBS)))
                           (fail 'observe-failed-invert)))
                     #f)])
               (instrument e #:cc))
         ...)]
    [(_ #:cc (#%plain-app (~literal list) e ...))
     (log-app-type "OBS PROP app (desugar list)" #'list)
     (with-syntax ([unfolded-expr
                    (let loop ([es (syntax->list #'(e ...))])
                      (cond [(pair? es)
                             #`(#%plain-app cons #,(car es) #,(loop (cdr es)))]
                            [else
                             #'(quote ())]))])
       #'(instrument unfolded-expr #:cc))]

    ;; Non-conditionable-primitives in conditionable context

    ;; * non-random first-order non-instrumented
    ;;   Doesn't need address tracking, doesn't need observation
    [(_ #:cc (#%plain-app f:nrfo-fun e ...))
     (log-app-type "STATIC app (NRFO)" #'f)
     #'(#%plain-app f (wrap-nt (instrument e #:nt)) ...)]
    ;; * analysis says doesn't call ERP (superset of prev case)
    ;;   Doesn't need address tracking, doesn't need observation
    [(_ #:cc (#%plain-app f:id e ...))
     #:when (not (app-calls-erp? stx))
     (log-app-type "STATIC app (!APP-CALLS-ERP)" #'f)
     #'(#%plain-app f (wrap-nt (instrument e #:nt)) ...)]
    ;; * instrumented function with right arity
    ;;   Use static protocol
    [(_ #:cc (#%plain-app f:instr-fun e ...))
     #:when (= (length (syntax->list #'(e ...))) (attribute f.arity))
     (with-syntax ([c (lift-call-site stx)])
       #'(#%plain-app f.instr (cons c ADDR) OBS
                      (wrap-nt (instrument e #:nt)) ...))]
    ;; * unknown, function is varref
    ;;   Use dynamic protocol
    [(_ #:cc (#%plain-app f:id e ...))
     (log-app-type "DYNAMIC app" #'f)
     (with-syntax ([c (lift-call-site stx)]
                   [(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (with-continuation-mark ADDR-mark (cons c ADDR)
             (with-continuation-mark OBS-mark OBS
               (#%plain-app f tmp ...)))))]
    ;; * unknown, function is expr
    ;;   Use dynamic protocol
    [(_ #:cc (#%plain-app e ...))
     (with-syntax ([c (lift-call-site stx)]
                   [(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (with-continuation-mark ADDR-mark (cons c ADDR)
             (with-continuation-mark OBS-mark OBS
               (#%plain-app tmp ...)))))]

    ;; Non-conditionable context

    ;; * non-random first-order non-instrumented
    ;;   Doesn't need address, doesn't need observation
    [(_ #:nt (#%plain-app f:nrfo-fun e ...))
     (log-app-type "STATIC app (NRFO)" #'f)
     #'(#%plain-app f (instrument e #:nt) ...)]
    ;; * analysis says doesn't call ERP (superset of prev case)
    ;;   Doesn't need address tracking, doesn't need observation
    [(_ #:nt (#%plain-app f:id e ...))
     #:when (not (app-calls-erp? stx))
     (log-app-type "STATIC app (!APP-CALLS-ERP)" #'f)
     #'(#%plain-app f (instrument e #:nt) ...)]
    ;; * instrumented function with right arity
    ;;   Use static protocol
    [(_ #:nt (#%plain-app f:instr-fun e ...))
     #:when (= (length (syntax->list #'(e ...))) (attribute f.arity))
     (with-syntax ([c (lift-call-site stx)])
       #'(#%plain-app f.instr (cons c ADDR) OBS (instrument e #:nt) ...))]
    ;; * unknown, function is varref
    ;;   Use dynamic protocol
    [(_ #:nt (#%plain-app f:id e ...))
     (log-app-type "DYNAMIC app" #'f)
     (with-syntax ([c (lift-call-site stx)]
                   [(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (with-continuation-mark ADDR-mark (cons c ADDR)
             (with-continuation-mark OBS-mark OBS
               (#%plain-app f tmp ...)))))]
    ;; * unknown, function is expr
    ;;   Use dynamic protocol
    [(_ #:nt (#%plain-app e ...))
     (with-syntax ([c (lift-call-site stx)]
                   [(tmp ...) (generate-temporaries #'(e ...))])
       #'(let-values ([(tmp) (wrap-nt (instrument e #:nt))] ...)
           (with-continuation-mark ADDR-mark (cons c ADDR)
             (with-continuation-mark OBS-mark OBS
               (#%plain-app tmp ...)))))]))

(begin-for-syntax
  ;; check-app : Syntax -> Void
  ;; Check constraints on applications; currently, just that observe* is
  ;; called with observable function (OBS-LAM).
  (define (check-app stx)
    (syntax-parse stx
      #:literals (#%plain-app observe*)
      [(#%plain-app observe* thunk expected)
       (unless (OBS-LAM #'thunk)
         (define observe-form (syntax-property #'thunk 'observe-form))
         (syntax-case observe-form ()
           [(_ e v)
            (raise-syntax-error #f NOT-OBSERVABLE-MESSAGE observe-form #'e)]
           [_
            (raise-syntax-error 'observe NOT-OBSERVABLE-MESSAGE stx)]))]
      [_ (void)]))

  (define NOT-OBSERVABLE-MESSAGE
    (string-append "expression is not observable"
                   ";\n it does not sample in an observable context")))

;; wrap-nt : wrapped around NT args to CC function call
(define-syntax (wrap-nt stx)
  (syntax-case stx ()
    [(wrap-nt e)
     #'(with ([OBS #f]) e)]))
