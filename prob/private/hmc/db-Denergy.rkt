;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.
#lang racket/base

(require (only-in racket/vector
                  vector-member)
         "../db.rkt"
         (only-in "../dist.rkt"
                  dist-Denergy)
         (only-in "../interfaces.rkt"
                  some-zone-matches?)
         (only-in "../util.rkt"
                  verbose?)
         )

(provide db-Denergy)

;; db-Denergy : (Hashof Address (Vectorof PartialDerivatives))
;;              ZonePattern
;;              -> (Address DB[Position] -> PotentialEnergy)
;;
;; Computes the partial derivative of the energy U(x) of total system x with
;; respect to the random variable xk with address k. 
;;
;;   ∂U(x)/∂xk = ∂Ux1/∂xk + ... + ∂UxN/∂xk
;; Let's do a change of variable by considering xk as a function of a convenience variable t, such that
;;     xk(t) = t   (and therefore ∂xk/∂t = 1)
;;   ∂U(x)/∂xk = ∂Ux1/∂t / (∂xk/∂t) + ... + ∂UxN/∂t / (∂xk/∂t)
;;             = ∂Ux1/∂t + ... + ∂UxN/∂t
;; Now consider an individual ∂Uxi/∂t
;;   It is given by (dist-Denergy Di ∂xi/∂t ∂param(i,1)/∂t ... ∂param(i,jᵢ)/∂t)
;;  Where ∂xi/∂t = (indicator i k) = { 1 if i = k, 0 if i ≠ k }.
;;  Consider a change of variable with respect to some xr:
;;    ∂param(i,j)/∂t = ∂param(i,j)/∂xr * (∂xr/∂t)
;;    Here again ∂xr/∂t is (indicator r k)
;;  so ∂param(i,j)/∂t = ∂param(i,j)/∂xk or 0 if param(i,j) does not depend on xk.
;;
;; So to compute the ∂U(x)/∂xk, let  I = { i | i = k ∨ ∃ j . param(i,j) depends on k }
;;  Then ∂U(x)/∂xk = Σ{i∈I} (dist-Denergy Di (indicator i k) ∂param(i,1)/∂xk ... ∂param(i,jᵢ)/∂xk)
(define ((db-Denergy param-partials zone) k x)
  (define (indicator i) (if (equal? k i) 1 0))
  (when #f 
    (when (verbose?)
      (eprintf " computing derivative of energy with respect to ~e\n" k)))

  (define denergy/dk
    (for/sum ([(i xi) (in-hash x)]
              #:when (some-zone-matches? (entry-zones xi) zone)
              )
      (define xi-dist (entry-dist xi))
      (define xi-value (entry-value xi))
      (define i-param-vec (hash-ref param-partials i))
      (define parameter-partials
        (for/list ([param-i-j (in-vector i-param-vec)])
          (if (not param-i-j)
              0 ; param is #f aka [() . (λ () 0)], so ∂param(i,j)/∂xk = 0
              (let ([depends (car param-i-j)]
                    [param-fn (cdr param-i-j)])
                (define (evaluate-depends)
                  (define depends-values 
                    (for/list ([p (in-vector depends)])
                      (entry-value (hash-ref x p))))
                  (apply param-fn depends-values))
                (cond
                 [(vector-member k depends)
                  => (λ (r)
                       (nth-value evaluate-depends r))]
                 [else 0])))))
      (let ([denergy-i/dk (apply dist-Denergy xi-dist xi-value (indicator i) parameter-partials)])
        (when #f
          (when (verbose?)
            (eprintf "  dU(~e)/d~e = ~s\n" i k denergy-i/dk)))
        denergy-i/dk)))
  
  (when #f
    (when (verbose?)
      (eprintf " denergy/d~e = ~s\n"
               k
               denergy/dk)))

  denergy/dk)


;; nth-value : (-> (Values Any ...)) Exact-Nonnegative-Integer -> Any
;;
;; (nth-value producer n) evaluates producer and returns just its nth result.
(define (nth-value producer n)
  (call-with-values producer
    (λ results
      (list-ref results n))))
