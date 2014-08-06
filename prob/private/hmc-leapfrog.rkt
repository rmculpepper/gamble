;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

(require racket/contract
         "db.rkt"
         (only-in "util.rkt"
                  verbose?
                  )
         racket/class
         (only-in "interfaces.rkt"
                  spcond:equal?))

(provide 
 (contract-out
  [hmc-leapfrog-proposal
   (-> (>/c 0)
       exact-nonnegative-integer?
       (-> any/c hash? real?)
       hash?
       hash?
       (-> any/c)
       (listof (cons/c symbol? spcond:equal?))
       (values hash? hash?))]))

;; Compute an update step for Hamiltonian Monte Carlo using the leapfrog method.
;;
;; Assumes that the momentum is drawn from a joint Normal(0,1) distribution such that
;; the mass matrix M in "x = x0 + ε·M⁻¹p(t+ε/2)" is the identity.
;;
;;  PositiveReal
;;  NonNegativeInteger
;;  (Address Position -> Momentum) ; partial derivative with respect to Address at Position
;;  Position
;;  Momentum
;;  (-> Any) ; model thunk
;;  SpConds
;;  -> (Values Position Momentum)
(define (hmc-leapfrog-proposal
         epsilon
         L
         grad-potential-fn
         x0
         p0
         thunk
         spconds)
  (define half-epsilon (/ epsilon 2.0))
  (define P-half-step (momentum-step half-epsilon grad-potential-fn))
  (define P-step (momentum-step epsilon grad-potential-fn))
  (define X-step (position-step epsilon thunk spconds))
  (let loop ([i (- L 1)]
             [x x0]
             ;; half step momentum using old position
             [p (P-half-step x0 p0)])
    (if (zero? i)
        (let* ([last-x
                ;; one last full step for the position. it's now at (* epsilon L)
                (X-step x p)]
               [last-p
                ;; momentum is a half epsilon behind. catch up.
                (P-half-step last-x p)])
          (values last-x last-p))
        (let* ([next-x
                ;; full step position
                (X-step x p)]
               [next-p
                ;; conceptually two half-steps: one for the end of the
                ;; current loop iteration, and one for the beginning
                ;; of the next iteration.
                (P-step next-x p)])
          (when (verbose?)
            (eprintf "    step position ~e\n    step momentum ~e\n" next-x next-p))
          (loop (- i 1) next-x next-p)))))
         
(define ((momentum-step epsilon grad-potential-fn) x p)
  (unless (hash? x)
    (raise-argument-error 'momentum-step "hash" 1 grad-potential-fn x p))
  (unless (hash? p)
    (raise-argument-error 'momentum-step "hash" 2 grad-potential-fn x p))
  (db-map (λ (k e)
            (let ([grad-U-x (grad-potential-fn k x)])
              (entry-value-map (λ (p)
                                 (- p (* epsilon grad-U-x)))
                               e)))
          p
          #:with-address #t))

; There ought to be a (* epsilon inv-M p) term, but
; we assume that M is the identity, so inv-M is 1.
(define ((position-step epsilon thunk spconds) x p)
  (unless (hash? x)
    (raise-argument-error 'position-step "hash" 3 epsilon thunk spconds x p))
  (unless (hash? p)
    (raise-argument-error 'position-step "hash" 4 epsilon thunk spconds x p))
  (define delta-db
    (db-map (λ (k e)
              (if (entry-pinned? e)
                  e
                  (let ([p (entry-value (hash-ref p k))])
                    (entry-value-map (λ (x)
                                       (+ x (* epsilon p)))
                                     e
                                     #f))))
            x
            #:with-address #t))
  
  ; okay, so in principle delta-db is is what we want to return - it's
  ; the updated positions.  but that's not enough: we also need to update the parameters
  ; that are embedded within each distribution object.
  ;
  ; Consider:
  ;    (define A (normal 0 1))
  ;    (define B (normal A 1))
  ;
  ; What we've done so far is kind of equivalent ot
  ;    (define A-delta (+ A-momentum (normal 0 1))
  ;    (define B-delta (+ B-momentum (normal A 1))
  ; where note that B-delta still refers to the old values of A as the mean.
  ;
  ; So now we rerun the thunk again so that we construct a brand new collection of
  ; distribution structs with updated parameters.
  ;
  ; One may wonder why do we need the updated params?  The answer is because the parameters
  ; are going to be used when we calculate the (dist-Denergy) of each distribution
  ; in order to compute gradients.
  
  (define ctx
    (new db-stochastic-ctx%
         (last-db x)
         (delta-db delta-db)
         (spconds spconds)))
  
  (begin
    (send ctx run thunk) ; don't care about the answer
     ; but do care about the recorded choices
    (get-field current-db ctx)))
  
