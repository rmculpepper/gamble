;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/vector
         (rename-in racket/match [match-define defmatch])
         "interfaces.rkt"
         "context.rkt"
         "../dist.rkt")
(provide (all-defined-out))

;; ========================================

(define (indicator v)
  (if (procedure? v)
      (indicator/predicate v)
      (indicator/value v)))

(define ((indicator/value v) x)
  (if (equal? x v) 1 0))

(define ((indicator/predicate p?) x)
  (if (p? x) 1 0))

(define (sampler->discrete-dist s n [f values]
                                #:burn [burn 0]
                                #:thin [thin 0]
                                #:normalize? [normalize? #t])
  (define h (make-hash))
  (define (s*)
    (for ([_ (in-range thin)]) (send s sample/weight))
    (defmatch (cons v w) (send s sample/weight))
    (cons (f v) w))
  (for ([_ (in-range burn)]) (send s sample/weight))
  (for ([i (in-range n)])
    (let* ([a+p (s*)]
           [a (car a+p)]
           [p (cdr a+p)])
      (hash-set! h a (+ p (hash-ref h a 0)))))
  (table->discrete-dist h normalize?))

(define (table->discrete-dist h normalize?)
  (define total-w
    (for/sum ([(a w) (in-hash h)]) w))
  (define entries
    (for/list ([(a w) (in-hash h)])
      (cons a (exact->inexact (/ w total-w)))))
  (make-discrete-dist entries #:normalize? normalize?))

;; ----------------------------------------

;; discrete-dist-error : (Discrete-Dist A) (Discrete-Dist A) -> Real
(define (discrete-dist-error a b)
  ;; Why 1/2? Because every error is counted twice: 
  ;; once for being present where it shouldn't be, 
  ;; and again for being absent from where it should be.
  (* 1/2
     ;; a U b = a U (b - a)   --- membership means positive pdf
     (+ (for/sum ([aval (in-vector (discrete-dist-values a))])
          (define aweight (dist-pdf a aval))
          (define bweight (dist-pdf b aval))
          (if (positive? aweight)
              (abs (- aweight bweight))
              0))
        (for/sum ([bval (in-vector (discrete-dist-values b))]
                  #:when (zero? (dist-pdf a bval)))
          (define bweight (dist-pdf b bval))
          (abs bweight)))))

;; ----------------------------------------

;; Misc utils

(define (probability? x)
  (and (real? x) (<= 0 x 1)))
