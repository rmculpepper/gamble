;; Copyright (c) 2016 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (rename-in racket/match [match-define defmatch])
         (for-syntax racket/base syntax/parse))
(provide verbose?
         with-verbose>
         vprintf
         Info
         print-accinfo)

;; ============================================================
;; Debugging

;; FIXME: add logger?

(define verbose? (make-parameter #f))
(define verbose-indent (make-parameter 0))

(define-syntax-rule (with-verbose> body ...)
  (parameterize ((verbose-indent (+ (verbose-indent) 2))) body ...))

(define-syntax-rule (vprintf fmt arg ...)
  (when #t
    (when (verbose?)
      (eprintf "# ~a" (make-string (verbose-indent) #\space))
      (eprintf fmt arg ...))))

;; ============================================================
;; Info

;; Designed to minimize allocation (hopefully)

;; An AccInfo is (vector AccInfoHeader Value ...)
;; An AccInfoHeader is (Listof (List String AccInfoKeyType)
;; An AccInfoKeyType is one of #:label, #:value, #:percent, #:nested

(define-syntax (Info stx)
  (define-syntax-class clause
    #:attributes (key value)
    #:datum-literals (% nested include)
    (pattern s:str
             #:with key #'(#f #:label)
             #:with value #''s)
    (pattern [s:str value:expr]
             #:with key #'(s #:value))
    (pattern [% s:str v:expr tot:expr]
             #:with key #'(s #:percent)
             #:with value #'(cons v tot))
    (pattern [include value:expr]
             #:with key #'(#f #:include))
    (pattern [nested s:str value:expr]
             #:with key #'(s #:nested)))
  (syntax-parse stx
    [(_ c:clause ...)
     #'(vector '(c.key ...) c.value ...)]))

(define (print-accinfo ai i)
  (for ([key (in-list (vector-ref ai 0))]
        [value (in-vector ai 1)])
    (match key
      [(list #f '#:include)
       (print-accinfo value i)]
      [(list #f '#:label)
       (iprintf i "~a\n" value)]
      [(list label '#:value)
       (iprintf i "~a: ~v\n" label value)]
      [(list label '#:percent)
       (match value
         [(cons x total)
          (iprintf i "~a: ~v, ~v%\n" label x (%age x total))])]
      [(list label '#:nested)
       (unless (null? value)
         (iprintf i "~a:\n" label)
         (let ([value (if (list? value) value (list value))])
           (if (null? value)
               (iprintf (+ i 2) "(empty)")
               (for ([sub value]) (print-accinfo sub (+ i 2))))))])))

(define (iprintf i fmt . args)
  (write-string (make-string i #\space))
  (apply printf fmt args))

(define (%age nom denom)
  (/ (* 100.0 nom) (exact->inexact denom)))
