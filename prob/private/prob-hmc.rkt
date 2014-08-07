;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

(require racket/class
         racket/match
         "context.rkt"
         "db.rkt"
         "hmc/system.rkt"
         "hmc/leapfrog.rkt"
         "hmc/db-Denergy.rkt"
         "interfaces.rkt"
         (only-in "util.rkt"
                  verbose?
                  ))

(provide hmc-sampler*)


#|
HMC - Hamiltonian Monte Carlo

The usual database of random choices is now thought of as
the "position" of a particle and an auxiliary normally
distributed "momentum" random variable is added such that the
quantity:
  H(x,p) = U(x) + K(p)
is conserved.  Where U(x) is the "potential energy" of the position
and K(p) is the "kinetic energy" of the momentum.

Limitations:
1. All distributions in the database should be continous.

2. There should be no "structural" choices.  (ie, the values of random
choices do not affect control flow through the probabilistic program).

3. (Temporary) All parameters of all random choices must be constant
and cannot depend on earlier random choices.  This means we don't have
to use automatic differentiation to account for dependencies in random
choices --- we can just take the sum of the energies of each entry in
the database as the potential energy of the entire system.

|#

;; hmc-sampler* : (-> A) PositiveReal PositiveInteger SPConds -> Sampler
(define (hmc-sampler* thunk epsilon L spconds)
  (new hmc-sampler%
       [thunk   thunk]
       [epsilon epsilon]
       [L       L]
       [spconds spconds]))

(define hmc-sampler%
  (class sampler-base%
    (init-field thunk
                epsilon
                L
                spconds)
    (field [last-db '#hash()]
           [answer #f]
           [gradients '#hash()])
    (super-new)
    
    (define/override (sample)
      (when (or (not last-db) (hash-empty? last-db))
        (collect-initial-sample))
      (when (verbose?)
        (eprintf "# okay, have a first sample ~e\n" last-db))
      (define-values (next-energy next-sys alpha)
        (match 
            (hmc-step last-db epsilon L (db-Denergy gradients)
                      thunk spconds)
          [(list 'okay old-sys next-sys)
           (let-values ([(next-energy alpha)
                         (hmc-acceptance-threshold old-sys
                                                   next-sys)])
             (values next-energy next-sys alpha))]
          [(list 'fail reason)
           (when (verbose?)
             (eprintf "# bad proposal ~e\n" reason))
           (values +nan.0 #f -inf.0)]))
      (define u (log (random)))
      (when (verbose?)
        (eprintf "# accept threshold = ~s\n" (exp alpha)))
      (cond [(< u alpha)
             (when (verbose?)
               (eprintf "# Accepted HMC step with ~s\n" (exp u)))
             (when #f
               (eprintf "(A)Energy: ~s (alpha ~s) \n" next-energy alpha))
             (let ([prev-db last-db])
               (set! last-db (hmc-system-X next-sys))
               (if (not (eval-definition-thunk!))
                   (begin
                     (when (verbose?)
                       (eprintf "# ... but the condition didn't hold, rejecting!\n"))
                     (set! last-db prev-db)
                     (sample))
                   answer))]
            [else
             ;; restart
             (when (verbose?)
               (eprintf "# Rejected HMC step with ~s\n" (exp u)))
             (when #f
               (eprintf "(R)Energy: ~s (alpha ~s)\n" next-energy alpha))
             (sample)]))
    
    (define/private (collect-initial-sample)
      (let loop ()
        (let ([current-db (eval-definition-thunk!)])
          (if (not current-db)
              (loop)
              (set! last-db current-db)))))

    (define/private (eval-definition-thunk!)
      (define delta-db (make-hash))
      (define ctx 
        (new db-stochastic-derivative-ctx%
             (last-db last-db)
             (delta-db delta-db)
             (spconds spconds)))
      (define result (send ctx run thunk))
      (define ans-db (get-field current-db ctx))
      (define grads (get-field derivatives ctx))
      (when (verbose?)
        (eprintf " (recorded derivatives are) ~e\n" grads)
        (eprintf " (relevant labels were) ~e \n" (get-field relevant-labels ctx)))
      (match result
        [(cons 'okay ans)
         (set! answer ans)
         (set! gradients grads)
         ans-db]
        [(cons 'fail fail-reason)
         (when (verbose?)
           (eprintf "# Rejected condition (~s)" fail-reason))
         #f]))
    ))
    
;; hmc-step : DB[X] Epsilon Positive-Integer (Address Position -> PotentialEnergy)
;;            -> (List 'okay HMC-System HMC-System)
;;               | (List 'fail Any)
;;
;; Construct randomly an initial momentum, then run L epsilon-steps
;; of Hamiltonian dynamics to arrive at a new position and momentum.
;;
;; Returns the initial system (with synthesized momentum) and the
;; final system (evolved after L epsilon-steps).
(define (hmc-step curr-x-db epsilon L grad-potential-fn thunk spconds)
  (define curr-p-db (synthesize-P-db curr-x-db))
  (define old-sys (hmc-system curr-x-db curr-p-db))
  (define step-result
    (hmc-leapfrog-proposal epsilon L
                           grad-potential-fn
                           old-sys
                           thunk
                           spconds))
  (match step-result
    [(list 'okay new-sys) (list 'okay old-sys new-sys)]
    [(list 'fail reason)  step-result]))
  

;; hmc-acceptance-threshold : HMC-System HMC-System -> Real
;;
;; Compute the acceptance ratio for a move from the given position and
;; momentum to a proposed position and momentum
(define (hmc-acceptance-threshold curr-sys next-sys)
  (define next-energy (hmc-system-energy next-sys))
  (define curr-energy (hmc-system-energy curr-sys))
  
  ; (x0 + p0) - (x* + p*)
  
  (define threshold
    ; Do I need to worry about cancellation here?
    (- curr-energy next-energy))
  
  (values next-energy threshold))

