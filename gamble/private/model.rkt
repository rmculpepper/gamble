;; Copyright (c) 2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     (rename-in racket/match [match-define defmatch])))
(provide (all-defined-out))

;; Defn += (defmodel M body ...)
;;       | (open-model M)

;; FIXME: re deflazy, avoid repeated wrapping!
;; FIXME: add rename/prefix options for open-model

(begin-for-syntax
  ;; A ModelInfo is (modelinfo (Listof Export) Identifier)
  ;; where the identifier is a reference to a procedure (-> Vector)
  ;; producing a vector of (possibly wrapped) export values.
  (struct modelinfo (exports fun-var)
          #:property prop:procedure
          (lambda (self stx)
            (raise-syntax-error #f "illegal use of model name" stx)))

  ;; An Export is (export Symbol ExportKind)
  ;; where ExportKind = 'value | 'lazy | 'illegal
  (struct export (symbol kind)))

(define-syntax (defmodel stx)
  (syntax-parse stx
    [(_ model-name:id body:expr ...)
     (define defctx (syntax-local-make-definition-context))
     (define (export-id? id)
       (define id-from-ctx
         (internal-definition-context-apply defctx (datum->syntax stx (syntax->datum id))))
       (when #f
         (eprintf "export? : ~s, bound=? ~a, free=? ~a\n"
                  id (bound-identifier=? id id-from-ctx) (free-identifier=? id id-from-ctx)))
       (bound-identifier=? id id-from-ctx))
     (define e-body-stxs (expand-body (syntax->list #'(body ...)) defctx (list (gensym))))
     (define exports (get-exports e-body-stxs export-id?))
     (with-syntax ([(modeltmp) (generate-temporaries #'(model-name))]
                   [(e-body ...) e-body-stxs]
                   [((exp-id exp-kind) ...) exports])
       #'(begin
           (define (modeltmp)
             e-body ...
             (vector (wrap-export exp-id exp-kind) ...))
           (define-syntax model-name
             (modelinfo (list (export 'exp-id 'exp-kind) ...) (quote-syntax modeltmp)))))]))

(define-syntax (wrap-export stx)
  (syntax-parse stx
    [(_ expr (~datum value))
     #'expr]
    [(_ expr (~datum lazy))
     #'(lambda () expr)]
    [(_ expr (~datum illegal))
     #'#f]))

(begin-for-syntax

  (define (get-exports e-stxs export-id?)
    (apply append
           (for/list ([e-stx (in-list e-stxs)])
             (syntax-parse e-stx
               #:literal-sets (kernel-literals)
               [(define-values (var ...) rhs)
                (for/list ([var (in-list (syntax->list #'(var ...)))]
                           #:when (export-id? var))
                  (list var (id-export-mode var 'value)))]
               [(define-syntaxes (svar ...) rhs)
                ;; FIXME: deflazy, struct constructors, etc???
                (for/list ([svar (in-list (syntax->list #'(svar ...)))]
                           #:when (export-id? svar))
                  (list svar (id-export-mode svar 'illegal)))]
               [_ null]))))

  (define (id-export-mode id default-mode)
    (cond [(syntax-property id 'gamble:model:export-mode)
           => (lambda (mode)
                (unless (memq mode '(value lazy illegal))
                  (raise-syntax-error #f (format "bad export mode: ~s" mode) id))
                mode)]
          [else default-mode]))

  (define (flatten-ids v onto)
    (cond [(pair? v) (flatten-ids (car v) (flatten-ids (cdr v) onto))]
          [(identifier? v) (cons v onto)]
          [else onto]))

  (define (expand-body stxs defctx ctx)
    (let loop ([stxs stxs] [acc null])
      (cond [(pair? stxs)
             (define e-stx (local-expand (car stxs) ctx #f defctx))
             (syntax-parse e-stx
               #:literal-sets (kernel-literals)
               [(begin part ...)
                (loop (append (syntax->list #'(part ...)) (cdr stxs)) acc)]
               [(define-values (var ...) rhs)
                (syntax-local-bind-syntaxes (syntax->list #'(var ...)) #f defctx)
                (loop (cdr stxs) (cons e-stx acc))]
               [(define-syntaxes (svar ...) rhs)
                (syntax-local-bind-syntaxes (syntax->list #'(svar ...)) #'rhs defctx)
                (loop (cdr stxs) (cons e-stx acc))]
               [_ (loop (cdr stxs) (cons e-stx acc))])]
            [else (reverse acc)])))

  )

;; ============================================================

(define-syntax (open-model stx)
  (syntax-parse stx
    [(_ m)
     #:declare m (static modelinfo? "identifier defined as a model")
     ;; FIXME: separate lctx arg
     (define (make-id exp)
       (define id (datum->syntax stx (export-symbol exp) #'m))
       (syntax-property id 'gamble:model:export-mode (export-kind exp)))
     (defmatch (modelinfo exports fun-var) (attribute m.value))
     (with-syntax ([(exp ...) (map make-id exports)]
                   [(exp-kind ...) (map export-kind exports)]
                   [(tmp ...) (generate-temporaries (map export-symbol exports))]
                   [run-model fun-var])
       (syntax-property
        #'(begin
            (define-values (tmp ...) (vector->values (run-model)))
            (define-syntaxes (exp)
              (make-export-transformer 'exp 'exp-kind (quote-syntax tmp)))
            ...)
        'disappeared-use #'m))]))

(begin-for-syntax
  (define (make-export-transformer exp-sym exp-kind var-id)
    (make-set!-transformer
     (lambda (stx)
       (syntax-parse stx
         #:literals (set!)
         [(set! me:id rhs:expr)
          (raise-syntax-error #f "cannot mutate model-imported name" stx #'me)]
         [me:id
          (case exp-kind
            [(value) var-id]
            [(lazy) #`(#%plain-app #,var-id)]
            [(illegal) (illegal-use-error stx)])]
         [(me:id arg:expr ...)
          (define app-stx (cons #'(#%expression me) (syntax->list #'(arg ...))))
          (datum->syntax stx app-stx stx)]))))

  (define (illegal-use-error id)
    (raise-syntax-error #f
      (string-append
       "use of this model export name is not allowed"
       ";\n it is bound as syntax in original model")
      id)))
