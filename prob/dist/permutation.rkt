;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/pretty
         racket/dict
         racket/vector
         racket/list
         (prefix-in m: math/special-functions)
         "../private/dist.rkt"
         "../private/dist-define.rkt")

(define-dist-type permutation-dist
  ([n exact-nonnegative-integer?])
  #:pdf permutation-pdf
  #:sample permutation-sample
  #:drift (lambda (value scale-factor)
            (cond [(<= n 1)
                   value]
                  [else
                   (define v (vector-copy value))
                   (define idx1 (random n))
                   (define idx2-pre (random (sub1 n)))
                   (define idx2 (+ idx2-pre (if (>= idx2-pre idx1) 1 0)))
                   (define elt1 (vector-ref v idx1))
                   (define elt2 (vector-ref v idx2))
                   (vector-set! v idx1 elt1)
                   (vector-set! v idx2 elt2)
                   v])))

(define (permutation-pdf n perm log?)
  (cond [(permutation? perm n)
         (let ((log-p (- (m:log-gamma (+ n 1)))))
           (if log?
               log-p
               (exp log-p)))]
        [else
         (if log? -inf.0 0)]))

;; FIXME: need to check that it's actually an n-permutation
;; FIXME: maybe want permutation ADT
;; FIXME: alternatively, could have fast-pdf vs slow-pdf functions,
;;   where fast-pdf only called on values returned from dist (but maybe different params)
(define (permutation? perm n)
  (and (vector? perm)
       (= (vector-length perm) n)))

(define (permutation-sample n)
  (define perm (build-vector n values))
  (for ([i (in-range (sub1 n))])
    (define j (+ i (random (- n i)))) ;; random index in [i,n)
    (define vi (vector-ref perm i))
    (define vj (vector-ref perm j))
    (vector-set! perm i vj)
    (vector-set! perm j vi))
  perm)
