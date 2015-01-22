;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base)
         racket/performance-hint
         racket/match
         racket/stxparam
         unstable/markparam)
(provide (all-defined-out))

;; This module defines parameters and functions used for
;; address-tracking (a la Bher) and conditioning.

;; See instrument.rkt for the implementation (through rewriting of
;; expanded modules) of the approaches described here.

;; ------------------------------------------------------------
;; Static communication

(define-syntax-parameter ADDR
  (make-rename-transformer (quote-syntax null)))
(define-syntax-parameter OBS
  (lambda (stx) #'#f))

(define-syntax (with stx)
  (syntax-case stx ()
    [(with ([x e] ...) body ...)
     (with-syntax ([(v ...) (generate-temporaries #'(x ...))])
       #'(let-values ([(v) e] ...)
           (syntax-parameterize ([x (make-rename-transformer (quote-syntax v))] ...)
             body ...)))]))

;; A CallSite is a Nat.

;; The call-site index is determined dynamically, so distinct modules
;; get distinct call-site indexes.

;; call-site-counter : Nat
(define call-site-counter 0)

;; call-site-table : hash[nat => sexpr describing source]
(define call-site-table (make-hash))

;; next-counter : any (List any any any) : -> Nat
(define (next-counter mod src-info)
  (set! call-site-counter (add1 call-site-counter))
  (hash-set! call-site-table call-site-counter (cons mod src-info))
  call-site-counter)

(define (address->string addr)
  (cond [(and (pair? addr) (exact-nonnegative-integer? (car addr)))
         (call-site->string (car addr))]
        [else "unknown call site"]))

(define (call-site->string n)
  (cond [(hash-ref call-site-table n #f)
         => (lambda (info)
              (match info
                [(list mod src line col stx fun)
                 (define src-string (info->src mod src line col))
                 (format "call at ~a" src-string)]))]
        [else
         "unknown call site"]))

(define (info->src mod src line col)
  (let ([mod (if (path? mod) (path->string mod) mod)]
        [src (if (path? src) (path->string src) src)])
    (format "~a ~a:~a"
            (or src (and (not line) (not col) mod) "?")
            (or line "?")
            (or col "?"))))

(define (describe-all-call-sites)
  (for ([i (in-range 1 (add1 call-site-counter))])
    (describe-call-site i)))

(define (describe-call-site n)
  (cond [(hash-ref call-site-table n #f)
         => (lambda (info)
              (match info
                [(list mod src line col stx fun)
                 (printf "call site ~s: ~a"
                         n (info->src mod src line col))]))]
        [else (printf "call site ~s: no info available" n)]))


;; ------------------------------------------------------------
;; Dynamic communication

(define ADDR-mark (mark-parameter))
(define OBS-mark 'observation)


;; Delimit call-site tracking.
;; Can't test using normal (f arg ...) syntax, because testing call-sites 
;; would be part of context! Use (apply/delimit f arg ...) instead.
(define (apply/delimit f . args)
  (with-continuation-mark ADDR-mark null
    (with-continuation-mark OBS-mark #f
      (apply f args))))


;; ------------------------------------------------------------
;; Observations

#|
Goal: implement (observe CC[(sample d)] v)
             as CC[(observe-sample d CC^-1[v])]

Conditioning contexts (CC) are defined thus:
CC ::=                   -- Observation rep:
       []
    |  (op v ... CC)
       where op is defined as condition-propagating
|#

(struct observation (value scale) #:transparent)

(define (observe* thunk expected)
  (define obs (observation expected 1))
  (define actual
    (with-continuation-mark 'observation obs
      (thunk)))
  ;; Check that result "equals" value.
  ;; FIXME: make equality approximation parameter or optional arg to observe?
  (check-observe-result 'observe expected actual)
  actual)

;; Checks that the result of thunk is consistent with observe context,
;; if observe context exists.
(define (check-observe* thunk)
  (call-with-immediate-continuation-mark OBS-mark
    (lambda (obs)
      (cond [obs
             (define actual (with-continuation-mark OBS-mark obs (thunk)))
             (define expected (observation-value obs))
             (check-observe-result 'check-observe expected actual)
             actual]
            [else
             ;; For consistency: thunk non-tail applied.
             (values (thunk))]))))

(define (check-observe-result who expected0 actual0)
  (define (bad)
    ;; This is an error rather than a (fail) because it indicates that
    ;; a condition is being applied to a non-conditionable expr.
    ;; FIXME: statically detect conditionable exprs
    (error who "observation failed\n  expected: ~e\n  got: ~e"
           expected0 actual0))
  (let loop ([expected expected0] [actual actual0])
    (cond [(pair? expected)
           (unless (pair? actual) (bad))
           (loop (car expected) (car actual))
           (loop (cdr expected) (cdr actual))]
          [(rational? expected)
           ;; FIXME: ??? better general way of checking "close-enough" for reals ???
           (unless (rational? expected) (bad))
           (unless (or (= actual expected)
                       (<= (abs (- expected actual))
                           (* 0.001 (max (abs expected) (abs actual)))))
             (bad))]
          [else (unless (equal? actual expected) (bad))])))

#|
[(eq? frame 'unknown)
 (error 'observe "expression is not conditionable;\n ~a"
        "`sample' context contains unknown unconditionable frame")]
[else
 (error 'observe "expression is not conditionable;\n ~a\n  bad frame: ~e"
        "`sample' context contains unconditionable frame"
        frame)]))
|#
