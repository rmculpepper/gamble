;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

(provide hmc-leapfrog-proposal)

;; Compute an update step for Hamiltonian Monte Carlo using the leapfrog method.
;;
;; Assumes that the momentum is drawn from a joint Normal(0,1) distribution such that
;; the mass matrix M in "x = x0 + ε·M⁻¹p(t+ε/2)" is the identity.
;;
;;  PositiveReal NonNegativeInteger
;;               (Position -> (Listof Position))
;;               Position
;;               Momentum
;;  -> (Values Position Momentum)
(define (hmc-leapfrog-proposal
         epsilon
         L
         grad-potential-fn
         x0
         p0)
  (unless (> L 0)
    (error "wanted non-negative L, got " L))
  (define half-epsilon (/ epsilon 2.0))
  (define P-half-step (momentum-step half-epsilon grad-potential-fn))
  (define P-step (momentum-step epsilon grad-potential-fn))
  (define X-step (position-step epsilon))
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
          (loop (- i 1) next-x next-p)))))
         
(define ((momentum-step epsilon grad-potential-fn) x p)
  (for/list ([p p]
             [grad-U-x (grad-potential-fn x)])
    (- p (* epsilon grad-U-x))))

; There ought to be a (* epsilon inv-M p) term, but
; we assume that M is the identity, so inv-M is 1.
(define ((position-step epsilon) x p)
  (for/list ([x x]
             [p p])
    (+ x (* epsilon p))))
