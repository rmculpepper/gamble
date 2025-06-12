;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
         (rename-in racket/match [match-define defmatch])
         racket/class
         "dist.rkt"
         "context.rkt"
         "prob-util.rkt"
         "prob-mh.rkt"
         "enumerate/enumerate.rkt"
         "interfaces.rkt")
(provide (all-defined-out))

;; ----

(define-syntax (observe stx)
  (syntax-case stx ()
    [(observe e v)
     ;; Note: instrumenter uses 'observed-expr property to report error
     (with-syntax ([thunk (syntax-property #'(lambda () e) 'observe-form stx)])
       #'(observe* thunk v))]))

(define observe/fail
  ;; FIXME: turn 2nd case into observe with fail instead of error?
  ;; FIXME: then convert 1st case w/ =, equal?, etc into 2nd case
  (case-lambda
    [(v) (unless v (fail 'observation))]
    [(v1 v2) (unless (equal? v1 v2) (fail 'observation))]))

;; ----

(define-syntax (rejection-sampler stx)
  (syntax-parse stx
    [(rejection-query def:expr ... result:expr)
     (template
      (rejection-sampler*
       (lambda () def ... result)))]))

(define (rejection-sampler* thunk)
  (new rejection-sampler% (thunk thunk)))

(define rejection-sampler%
  (class sampler-base%
    (init-field thunk)
    (field [successes 0]
           [rejections 0])
    (super-new)

    (define/override (info)
      (printf "== Rejection sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections))

    (define/override (sample)
      (define ctx (new rejection-stochastic-ctx%))
      (define v (send ctx run thunk))
      (case (car v)
        [(okay)
         (set! successes (add1 successes))
         (cdr v)]
        [(fail)
         (set! rejections (add1 rejections))
         (sample)]))
    ))

(define rejection-stochastic-ctx%
  (class plain-stochastic-ctx/run%
    (inherit fail run)
    (super-new)
    (define/override (observe-sample dist val scale)
      (cond [(or (finite-dist? dist) (integer-dist? dist))
             ;; ie, actually have pmf
             (unless (< (random) (dist-pdf dist val))
               (fail 'observation))]
            [else
             (error 'observe-sample
                    (string-append 
                     "observation on distribution not supported by rejection sampler"
                     "\n  distribution: ~e")
                    dist)]))

    (define/public (trycatch p1 p2)
      (match (run p1)
        [(cons 'okay value)
         value]
        [(cons 'fail _)
         (p2)]))
    ))

;; ----

(define-syntax (importance-sampler stx)
  (syntax-parse stx
    [(importance-sampler def:expr ... result:expr)
     (template
      (importance-sampler*
       (lambda () def ... result)))]))

(define (importance-sampler* thunk)
  (new importance-sampler% (thunk thunk)))

(define importance-sampler%
  (class* object% (weighted-sampler<%>)
    (init-field thunk)
    (field [successes 0]
           [rejections 0]
           [dens-dim-rejections 0]
           [min-dens-dim +inf.0]
           [bad-samples 0])
    (super-new)

    (define/public (info)
      (printf "== Importance sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections)
      (printf "Density dimension: ~s\n" min-dens-dim)
      (unless (zero? dens-dim-rejections)
        (printf "Density dimension rejections: ~s\n" dens-dim-rejections))
      (unless (zero? bad-samples)
        (printf "Bad samples emitted (wrong density dimension): ~s" bad-samples)))

    (define/public (sample/weight)
      (define ctx (new importance-stochastic-ctx%))
      (define v (send ctx run thunk))
      (case (car v)
        [(okay)
         (define dens-dim (get-field dens-dim ctx))
         (when (< dens-dim min-dens-dim)
           (unless (zero? successes)
             (eprintf "WARNING: previous ~s samples are meaningless; wrong density dimension\n"
                      successes))
           (vprintf "Lower density dimension seen: ~s\n" dens-dim)
           (set! bad-samples successes)
           (set! min-dens-dim dens-dim))
         (cond [(<= dens-dim min-dens-dim)
                (set! successes (add1 successes))
                (cons (cdr v) (get-field weight ctx))]
               [else
                (set! dens-dim-rejections (add1 dens-dim-rejections))
                (set! rejections (add1 rejections))
                (sample/weight)])]
        [(fail)
         (set! rejections (add1 rejections))
         (sample/weight)]))
    ))

(define importance-stochastic-ctx%
  (class rejection-stochastic-ctx%
    (field [weight 1]
           [dens-dim 0])
    (inherit fail run)
    (super-new)
    (define/override (observe-sample dist val scale)
      (define l (dist-pdf dist val))
      (unless (dist-has-mass? dist) (set! dens-dim (add1 dens-dim)))
      (if (positive? l)
          (set! weight (* weight l scale))
          (fail 'observation)))

    (define/override (trycatch p1 p2)
      (define saved-weight weight)
      (define saved-dens-dim dens-dim)
      (match (run p1)
        [(cons 'okay value)
         value]
        [(cons 'fail _)
         (set! weight saved-weight)
         (set! dens-dim saved-dens-dim)
         (p2)]))
    ))

;; ----

(define-syntax (mh-sampler stx)
  (syntax-parse stx
    [(mh-sampler (~optional (~seq #:transition tx))
                 def:expr ... result:expr)
     #:declare tx (expr/c #'mh-transition?)
     (template
      (mh-sampler*
       (lambda () def ... result)
       (?? tx.c)))]))

;; ----

(define-syntax (enumerate stx)
  (syntax-parse stx
    [(enumerate (~or (~optional (~seq #:limit limit:expr))
                     (~optional (~seq #:normalize? normalize?)))
                ...
                def:expr ... result:expr)
     (template
      (enumerate*
       (lambda () def ... result)
       (?? limit #f)
       (?? normalize? #t)))]))

;; ----

(define-syntax-rule (with-zone z e ...)
  (parameterize ((current-zones (cons z (current-zones)))) e ...))
