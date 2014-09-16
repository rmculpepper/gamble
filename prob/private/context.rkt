;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(provide (all-defined-out))

;; This module describes (in comments) the approaches used for
;; address-tracking (a la Bher) and conditioning. It also defines
;; parameters and functions used to contain that information.

;; See instrument.rkt for the implementation (through rewriting of
;; expanded modules) of the approaches described here.

;; ------------------------------------------------------------
;; Address-tracking

;; the-context : (Parameterof (Listof Nat))
(define the-context (make-parameter null))

(define (get-context)
  (the-context))

;; Delimit call-site tracking.
;; Can't test using normal (f arg ...) syntax, because testing call-sites 
;; would be part of context! Use (apply/delimit f arg ...) instead.
(define (apply/delimit f . args)
  (parameterize ((the-context null))
    (apply f args)))


;; ------------------------------------------------------------
;; Observations

#|
Goal: implement (observe CC[(sample d)] v)
             as CC[(observe-at CC^-1[v])]

Conditioning contexts (CC) are defined thus:
CC ::=                   -- CCFrame rep:
       []
    |  (+ v CC)          -- v
    |  (- CC)            -- '-
    |  (cons CC e)       -- 'car
    |  (cons v CC)       -- 'cdr
    |  (reverse CC)      -- 'reverse
    |  ... more invertible functions that don't affect density
           (eg, * is bad, I think)
|#

(struct observation (value))

(define obs-mark 'observe)
(define obs-prompt (make-continuation-prompt-tag))

(define observing? (make-parameter #f))

(define (observe* thunk expected)
  (define obs (observation expected))
  (define actual
    (call-with-continuation-prompt
     (lambda ()
       (parameterize ((observing? obs))
         (with-continuation-mark obs-mark 'unknown (thunk))))
     obs-prompt))
  ;; Check that result "equals" value.
  ;; FIXME: make equality approximation parameter or optional arg to observe?
  (check-observe-result 'observe expected actual)
  actual)

(define (check-observe* thunk)
  (define obs (observing?))
  (cond [obs
         (with-continuation-mark obs-mark 'ok
           (let ()
             (define cc (get-observe-context))
             (define actual (thunk))
             (define expected (observe-context-adjust-value cc obs))
             (check-observe-result 'check-observe expected actual)
             actual))]
        [else
         ;; For consistency: thunk non-tail applied.
         (values (thunk))]))

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

(define (get-observe-context)
  (continuation-mark-set->list
   (current-continuation-marks obs-prompt)
   obs-mark
   obs-prompt))

(define (observe-context-adjust-value ctx obs)
  (interpret-context ctx (observation-value obs)))

(define (interpret-context ctx val)
  (foldr interpret-frame val ctx))

(define (interpret-frame frame val)
  (cond [(real? frame)  ;; (+ frame [])
         (- val frame)]
        [(eq? frame 'car) ;; (cons [] e)
         (car val)]
        [(eq? frame 'cdr) ;; (cons v [])
         (cdr val)]
        [(eq? frame 'reverse) ;; (reverse [])
         (reverse val)]
        [(eq? frame 'ok)
         val]
        [(eq? frame 'unknown)
         (error 'observe "expression is not conditionable;\n ~a"
                "`sample' context contains unknown unconditionable frame")]
        [else
         (error 'observe "expression is not conditionable;\n ~a\n  bad frame: ~e"
                "`sample' context contains unconditionable frame"
                frame)]))
