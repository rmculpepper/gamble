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
         racket/fixnum
         racket/flonum
         (prefix-in m: math/distributions)
         (prefix-in m: math/special-functions)
         "../private/dist.rkt"
         "../private/dist-impl.rkt"
         "../private/dist-define.rkt")
(provide permutation?)

(define-dist-type permutation-dist
  ([n exact-nonnegative-integer?])
  #:pdf permutation-pdf
  #:sample permutation-sample
  #:has-mass
  #:drift (lambda (value scale-factor) (permutation-drift n value scale-factor)))

(define (permutation-pdf n perm log?)
  (cond [(permutation? perm n)
         (let ((log-p (- (m:log-gamma (+ n 1)))))
           (if log?
               log-p
               (exp log-p)))]
        [else
         (impossible log? 'permutation "not a permutation")]))

(define fxsize (- (system-type 'word) 2))

;; FIXME: maybe want permutation ADT?
;; FIXME: alternatively, could have fast-pdf vs slow-pdf functions,
;;   where fast-pdf only called on values returned from dist (but maybe different params)
(define (permutation? perm n)
  (cond [(and (vector? perm) (= (vector-length perm) n))
         (cond [(<= n fxsize)
                (and (for/fold ([seen 0]) ([e (in-vector perm)])
                       (and seen
                            (exact-nonnegative-integer? e)
                            (<= 0 e (sub1 n))
                            (not (bitwise-bit-set? seen e))
                            (bitwise-ior seen (arithmetic-shift 1 e))))
                     #t)]
               [else
                (define seen (make-fxvector (quotient (+ n -1 fxsize) fxsize) 0))
                (for/and ([e (in-vector perm)])
                  (and (exact-nonnegative-integer? e)
                       (<= 0 e (sub1 n))
                       (let ()
                         (define i (quotient e fxsize))
                         (define j (remainder e fxsize))
                         (and (not (bitwise-bit-set? (fxvector-ref seen i) j))
                              (void (fxvector-set! seen i
                                      (bitwise-ior (fxvector-ref seen i)
                                                   (arithmetic-shift 1 j))))
                              #t))))])]
        [else #f]))

(define (permutation-sample n)
  (define perm (build-vector n values))
  (for ([i (in-range (sub1 n))])
    (define j (+ i (random (- n i)))) ;; random index in [i,n)
    (define vi (vector-ref perm i))
    (define vj (vector-ref perm j))
    (vector-set! perm i vj)
    (vector-set! perm j vi))
  perm)

(define (permutation-drift n value scale-factor)
  (cond [(<= n 1)
         (cons value 0)]
        [else
         (define v (vector-copy value))
         (define nswaps
           (inexact->exact (ceiling (exponential (* 0.25 n scale-factor)))))
         ;; FIXME: if nswaps >> n, just resample; faster ??
         (for ([_swap (in-range nswaps)])
           (define idx1 (random n))
           (define idx2-pre (random (sub1 n)))
           (define idx2 (+ idx2-pre (if (>= idx2-pre idx1) 1 0)))
           (define elt1 (vector-ref v idx1))
           (define elt2 (vector-ref v idx2))
           (vector-set! v idx1 elt2)
           (vector-set! v idx2 elt1))
         (cons v 0)]))

(define (exponential scale)
  (flvector-ref (m:flexponential-sample (exact->inexact scale) 1) 0))
