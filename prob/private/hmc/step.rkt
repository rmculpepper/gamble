;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.
#lang racket/base

(require racket/match
         "system.rkt"
         "leapfrog.rkt"
         )

(provide hmc-step)

;; hmc-step : HMC-System Epsilon Positive-Integer (Address Position -> PotentialEnergy)
;;            -> (List 'okay HMC-System HMC-System)
;;               | (List 'fail Any)
;;
;; Construct randomly an initial momentum, then run L epsilon-steps
;; of Hamiltonian dynamics to arrive at a new position and momentum.
;;
;; Returns the initial system (with synthesized momentum) and the
;; final system (evolved after L epsilon-steps).
(define (hmc-step last-sys epsilon L grad-potential-fn thunk spconds)
  (define init-sys (hmc-system-evolve-P last-sys (λ(x p) (synthesize-P-db x))))
  (define step-result
    (hmc-leapfrog-proposal epsilon L
                           grad-potential-fn
                           init-sys
                           thunk
                           spconds))
  (match step-result
    [(list 'okay new-sys) (list 'okay init-sys new-sys)]
    [(list 'fail reason)  step-result]))
  


        
