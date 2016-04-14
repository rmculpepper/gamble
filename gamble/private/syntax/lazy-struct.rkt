;; Copyright (c) 2015 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/match
         racket/struct
         "../interfaces.rkt")
(provide (all-defined-out))

(define-syntax (lazy-struct stx)
  (syntax-parse stx
    [(_ name:id (field:id ...))
     (with-syntax ([(name-field ...)
                    (for/list ([field (in-list (syntax->list #'(field ...)))])
                      (format-id #'name "~a-~a" #'name field))]
                   [lazy-make-name (format-id #'HIDDEN "lazy-make-~a" #'name)]
                   [strict-make-name (format-id #'name "strict-make-~a" #'name)]
                   [name? (format-id #'name "~a?" #'name)]
                   [struct:name (format-id #'name "struct:~a" #'name)])
       (syntax/loc stx
         (begin
           (define-values (struct:name name? lazy-make-name strict-make-name name-field ...)
             (let ()
               ;; Each field in underlying struct contains (-> A) memoized fun.
               (struct name (field ...)
                       ;; Equality, hashing, and printing force all fields.
                       #:property prop:equal+hash
                       (list (lambda (v1 v2 recur)
                               (and (recur ((name-field v1)) ((name-field v2))) ...))
                             (lambda (v recur)
                               (recur (vector 'name ((name-field v)) ...)))
                             (lambda (v recur)
                               (recur (vector 'name ((name-field v)) ...))))
                       #:property prop:custom-write
                       (make-constructor-style-printer
                        (lambda (v) 'name)
                        (lambda (v) (list (pretty-get-field name-field v) ...))))
               (values struct:name name?
                       name
                       (let ([strict-make-name (lambda (field ...) (name (lambda () field) ...))])
                         strict-make-name)
                       (let ([name-field (lambda (v) ((name-field v)))])
                         name-field)
                       ...)))
           (define-match-expander name
             (make-lazy-match-transformer
              #'name? #'(name-field ...))
             (make-lazy-constructor-transformer
              #'lazy-make-name #'strict-make-name (length '(field ...)))))))]))

(define (pretty-get-field accessor v)
  (with-handlers ([exn:fail?
                   (lambda (e) (pretty-print-as "(error)"))])
    ;; Use values to force single value.
    (values ((accessor v)))))

(struct pretty-print-as (show)
        #:property prop:custom-write
        (lambda (self out mode) (write-string (pretty-print-as-show self) out)))

;; It would be nice if we could define a match expander s.t.
;;   (lazy-struct S (x))
;;   (match v [(S x) ....])
;; were equivalent to
;;   (cond [(S? v) (deflazy x (S-x v)) ....] ....)
;; but match-expanders don't have any way of binding syntax.

(begin-for-syntax

  (define (make-lazy-match-transformer name? name-accessors)
    (lambda (stx)
      (syntax-parse stx
        [(_ #:strict pat:expr ...)
         (with-syntax ([name? name?]
                       [(name-accessor ...) name-accessors])
           #'(? name? (app name-accessor pat) ...))]
        [(_ #:thunk var:id ...)
         (with-syntax ([name? name?]
                       [(name-accessor ...) name-accessors])
           #'(? name? (app (lambda (v) (lambda () (name-accessor v))) var) ...))])))

  (define (make-lazy-constructor-transformer lazy-maker strict-maker field-count)
    (lambda (stx)
      (syntax-parse stx
        [(_ arg:expr ...)
         (unless (equal? (length (syntax->list #'(arg ...))) field-count)
           (raise-syntax-error #f
             (format "wrong number of fields given (expected ~s)" field-count) stx))
         (with-syntax ([lazy-maker lazy-maker])
           (syntax/loc stx
             (lazy-maker (mem (lambda () arg)) ...)))]
        [_:id
         (raise-syntax-error #f
           (string-append
            "illegal use of lazy-struct constructor"
            (format "\n maybe use `~a' instead" (syntax->datum strict-maker)))
           stx)]))))
