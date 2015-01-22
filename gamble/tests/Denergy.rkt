;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble
(require (for-syntax racket/base)
         racket/format
         (except-in rackunit fail))

(define epsilon #i1e-6)

(define-check (check-close a b)
  (when #f
    (printf "   -- differ by ~a%    {~s, ~s}\n"
            (~r (* 100 (abs (/ (- a b) (max (abs a) (abs b))))) #:precision 4)
            a b))
  (check-= a b (* 1/100 (max (abs a) (abs b)))))

;; For simplicity, assume all params can be any positive real; generate params in (1,1000).
(define (genparam0) (add1 (* (random) 999)))

(define (test-dist make-dist nparams [genparam genparam0])
  (define params (for/list ([i nparams]) (genparam)))
  (define dist (apply make-dist params))
  (define x (dist-sample dist))
  (define energy0 (dist-energy dist x))

  (when #t
    (printf "** Testing ~a; value (~s)\n" (object-name make-dist) x))
  (test-case (format "~a; value" (object-name make-dist))
    (define delta (* (abs x) 0.000001))
    (check-close (dist-Denergy dist x 1)
                 (/ (- (dist-energy dist (+ x delta))
                       (dist-energy dist x))
                    delta)))

  (for ([dindex nparams])
    (when #t
      (printf "** Testing ~a; parameter #~s\n" (object-name make-dist) dindex))
    (test-case (format "~a; parameter #~s" (object-name make-dist) dindex)
      (define dparams/dt (for/list ([i nparams]) (if (= i dindex) 1 0)))
      (check-close (apply dist-Denergy dist x 0 dparams/dt)
                   (diff-energy make-dist params x dindex)))))

(define (diff-energy make-dist params x dindex)
  (define dist0 (apply make-dist params))
  (define params1
    (for/list ([p params] [i (in-naturals)])
      (if (= i dindex) (+ p epsilon) p)))
  (define dist1 (apply make-dist params1))
  (/ (- (dist-energy dist1 x)
        (dist-energy dist0 x))
     epsilon))

;; ----

(for ([i 10]) (test-dist beta-dist 2))
(for ([i 10]) (test-dist cauchy-dist 2))
(for ([i 10]) (test-dist exponential-dist 1))
(for ([i 10]) (test-dist gamma-dist 2 (lambda () (* (random) 2))))
(for ([i 10]) (test-dist gamma-dist 2))
(for ([i 10]) (test-dist logistic-dist 2))
(for ([i 10]) (test-dist normal-dist 2))
