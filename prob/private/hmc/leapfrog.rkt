;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

(require racket/contract
         racket/match
         racket/class
         "system.rkt"
         "../db.rkt"
         (only-in "../util.rkt"
                  verbose?
                  )
         (only-in "../interfaces.rkt"
                  spcond:equal?
                  some-zone-matches?)
         )

(provide 
 (contract-out
  [hmc-leapfrog-proposal
   (-> (>/c 0)
       exact-nonnegative-integer?
       (-> any/c hash? real?)
       hmc-system?
       (-> any/c)
       (listof (cons/c symbol? spcond:equal?))
       any/c ;; ZonePattern
       (or/c (list/c 'okay any/c hmc-system?)
             (list/c 'fail any/c)))]))

;; Compute an update step for Hamiltonian Monte Carlo using the leapfrog method.
;;
;; Assumes that the momentum is drawn from a joint Normal(0,1) distribution such that
;; the mass matrix M in "x = x0 + ε·M⁻¹p(t+ε/2)" is the identity.
;;
;;  PositiveReal
;;  NonNegativeInteger
;;  (Address Position -> Momentum) ; partial derivative with respect to Address at Position
;;  HMC-System
;;  (-> Any) ; model thunk
;;  SpConds
;;  ZonePattern
;;  -> (List 'okay Any HMC-System)
;;     | (List 'fail Any)
(define (hmc-leapfrog-proposal
         epsilon
         L
         grad-potential-fn
         sys0
         thunk
         spconds
         zone)
  (let* ([half-epsilon  (/ epsilon 2.0)]
         [P-half-step   (momentum-step half-epsilon grad-potential-fn)]
         [P-step        (momentum-step epsilon grad-potential-fn)]
         [escape-prompt (make-continuation-prompt-tag)]
         [bad-step      (λ (reason)
                          (abort-current-continuation
                           escape-prompt
                           (λ () (list 'fail reason))))]
         [propagate-X   (propagate-X-changes-to-model thunk spconds bad-step)]
         [X-step        (position-step epsilon zone propagate-X bad-step)])
    (call-with-continuation-prompt
     (λ ()
       (let loop ([i (- L 1)]
                  ;; To start, step momentum by a half-step.  It's now
                  ;; a half time-step ahead and will be for the next L-1 iterations.
                  [sys (hmc-system-evolve-P sys0 P-half-step)])
         (if (zero? i)
             (let* ([ans-box (box (void))]
                    [final-X-step (position-step epsilon zone propagate-X bad-step
                                                 #:record-result ans-box)]
                    [new-sys
                    ;; One last full step for the position. it's now
                    ;; at time (* epsilon L).  Then since momentum now
                    ;; fell a half step behind, catch up.
                     (hmc-system-evolve-P (hmc-system-evolve-X sys final-X-step)
                                          P-half-step)])
               (list 'okay (unbox ans-box) new-sys))
             (let
                 ;; Full step position, then conceptually two
                 ;; half-steps for momentum: one for the end of the
                 ;; current loop iteration, and one for the beginning
                 ;; of the next iteration.
                 ([next-sys (hmc-system-evolve-P (hmc-system-evolve-X sys X-step)
                                                 P-step)])
               (when (verbose?)
                 (eprintf "    stepped to ~e\n" next-sys))
               (loop (- i 1) next-sys)))))
     escape-prompt)))
         
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
(define ((position-step epsilon zone propagate-X-change-fn escape
                        #:record-result [record-result-box #f]) x p)
  (unless (hash? x)
    (raise-argument-error 'position-step "hash" 3
                          epsilon propagate-X-change-fn escape x p))
  (unless (hash? p)
    (raise-argument-error 'position-step "hash" 4
                          epsilon propagate-X-change-fn escape x p))
  (define ((update-position dx) x) (+ x (* epsilon dx)))
  ; key property: we must leave pinned entries out of delta-x so that
  ; they keep their old values from x (but with updated ll).
  (define delta-X
    (let ([delta-X (make-hash)])
      (for ([(k e) (in-hash x)]
            #:when (not (entry-pinned? e))
            #:when (some-zone-matches? (entry-zones e) zone)
            [p     (in-value (entry-value (hash-ref p k)))])
        (let ([delta-e (entry-value-map (update-position p) e)])
          (if (ll-possible? (entry-ll delta-e))
              (hash-set! delta-X k delta-e)
              (escape (format "~e moved from ~e (~e) to ~e (~e)"
                              k
                              (entry-value e)
                              (entry-ll e)
                              (entry-value delta-e)
                              (entry-ll delta-e))))))
      delta-X))
            
  (propagate-X-change-fn x delta-X #:record-result record-result-box))

(define ((propagate-X-changes-to-model thunk spconds escape) x delta-X
         #:record-result record-result-box)
  ; okay, so in principle delta-x is is what we want to return - it's
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
  (define ctx (new db-stochastic-ctx%
                   (last-db x)
                   (delta-db delta-X)
                   (spconds spconds)))
  (define run-result (send ctx run thunk))
  (match run-result
    [(cons 'okay result) 
     (when record-result-box
       (set-box! record-result-box result))
     (get-field current-db ctx)]
    [(cons 'fail reason) (escape reason)]))
  
