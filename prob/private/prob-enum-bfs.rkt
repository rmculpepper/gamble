;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         "prob-hooks.rkt"
         "prob-enum-common.rkt"
         "util.rkt")
(provide enumerate-id*)

#|
Enumeration via iterative deepening instead of delimited continuations.
|#

(define current-rtrace (make-parameter null))

;; A (StepResult A) is one of
;; - (only A)
;; - (split (listof (cons Prob RTrace)))

;; A TraceEntry is one of
;; - (special Tag Dist)
;; - any other value
(struct special (tag dist))

;; A Trace is (list Value ... TraceEntry)
;; An RTrace is (list TraceEntry Value ...)
;; -- unlike MH, don't need to keep track of params, dist, etc; just replay

(define (enumerate-id* thunk pred [project values]
                       #:limit [limit 1e-6]
                       #:normalize? [normalize? #t])
  (define tree (call-with-context thunk null))
  (define-values (table prob-unexplored prob-accepted)
    (explore tree pred project limit))
  (when (verbose?)
    (eprintf "enumerate: unexplored rate: ~s\n" prob-unexplored)
    (eprintf "enumerate: accept rate: ~s\n"
             (/ prob-accepted (- 1 prob-unexplored))))
  (when (zero? (hash-count table))
    (error 'enumerate "condition accepted no paths"))
  (when (and normalize? (zero? prob-accepted))
    (error 'enumerate "probability of accepted paths underflowed to 0"))
  (tabulate table prob-accepted #:normalize? normalize?))

(define (call-with-context thunk rtrace)
  (step-result->enum-tree
   thunk
   (let/ec escape
     (parameterize ((current-ERP (make-enum-ERP (reverse rtrace) escape))
                    (current-rtrace rtrace))
       (only (thunk))))))

(define (step-result->enum-tree thunk sr)
  (match sr
    [(only v)
     (only v)]
    [(split subs)
     (split (for/list ([sub (in-list subs)])
              (match sub
                [(cons prob rtrace)
                 (cons prob (lambda () (call-with-context thunk rtrace)))])))]))

(define ((make-enum-ERP replay-trace escape) tag dist)
  (cond [(pair? replay-trace)
         (begin0 (match (car replay-trace)
                   [(special sp-tag sp-dist)
                    ;; Always last in rtrace
                    (current-rtrace (cdr (current-rtrace)))
                    (ERP* escape sp-tag sp-dist)]
                   [v v])
           (set! replay-trace (cdr replay-trace)))]
        [else
         (ERP* escape tag dist)]))

(define (ERP* escape tag dist)
  (unless (memq (car tag)
                '(bernoulli flip discrete binomial geometric poisson continue-special))
    (error 'ERP "cannot enumerate non-discrete distribution: ~s" tag))
  (define-values (vals probs tail-prob+tag)
    (dist->vals+probs tag dist))
  (define rtrace (current-rtrace))
  (escape
   (split
    (cons/f (and tail-prob+tag
                 (let ([tail-prob (car tail-prob+tag)]
                       [tail-tag (cdr tail-prob+tag)])
                   (cons tail-prob (cons (special tail-tag dist) rtrace))))
            (for/list ([val (in-list vals)]
                       [prob (in-list probs)])
              (cons prob (cons val rtrace)))))))

(define (cons/f x xs) (if x (cons x xs) xs))

;; ----

(define (enum-mem f)
  (error 'mem "unimplemented"))
