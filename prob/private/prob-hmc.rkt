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
         "hmc/acceptance-threshold.rkt"
         "hmc/step.rkt"
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
    (field [last-accepted-sys #f]
           [answer #f]
           [gradients '#hash()])
    (super-new)
    
    (define/override (sample)
      (when (not last-accepted-sys) 
        (collect-initial-sample))
      (when (verbose?)
        (eprintf "# okay, have a first sample ~e\n"
                 (hmc-system-X last-accepted-sys)))
      (define-values (proposal-energy proposal-sys alpha)
        (match 
            (hmc-step last-accepted-sys epsilon L (db-Denergy gradients)
                      thunk spconds)
          [(list 'okay initial-sys proposal-sys)
           ;; N.B. "initial-sys" is the starting state of the hmc step.
           ;; it is not equal to the last-accepted-sys.  In particular, it has
           ;; a synthesized (random) momentum. 
           (let-values ([(proposal-energy alpha)
                         (hmc-acceptance-threshold initial-sys
                                                   proposal-sys)])
             (values proposal-energy proposal-sys alpha))]
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
               (eprintf "(A)Energy: ~s (alpha ~s) \n" proposal-energy alpha))
             (let ([prev-accepted-sys last-accepted-sys])
               (set! last-accepted-sys proposal-sys)
               (if (not (eval-definition-thunk!))
                   (begin
                     (when (verbose?)
                       (eprintf "# ... but the condition didn't hold, rejecting!\n"))
                     (set! last-accepted-sys prev-accepted-sys)
                     (sample))
                   answer))]
            [else
             ;; restart
             (when (verbose?)
               (eprintf "# Rejected HMC step with ~s\n" (exp u)))
             (when #f
               (eprintf "(R)Energy: ~s (alpha ~s)\n" proposal-energy alpha))
             (sample)]))
    
    (define/private (collect-initial-sample)
      (let loop ()
        (let ([current-db (eval-definition-thunk!)])
          (if (not current-db)
              (loop)
              (set! last-accepted-sys (hmc-system current-db (make-hash)))))))

    (define/private (eval-definition-thunk!)
      (define delta-db (make-hash))
      (define last-db (or (and last-accepted-sys 
                               (hmc-system-X last-accepted-sys))
                          (make-hash)))
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
    

