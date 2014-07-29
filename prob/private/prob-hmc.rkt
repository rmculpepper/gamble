;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

(require racket/class
         racket/match
         "context.rkt"
         "db.rkt"
         "hmc-leapfrog.rkt"
         "interfaces.rkt"
         "../dist.rkt"
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

;; hmc-sampler* : (-> A) PositiveReal PositiveInteger -> Sampler
(define (hmc-sampler* thunk epsilon L)
  (new hmc-sampler%
       [thunk   thunk]
       [epsilon epsilon]
       [L       L]))

(define hmc-sampler%
  (class sampler-base%
    (init-field thunk
                epsilon
                L)
    (field [last-db '#hash()]
           [answer #f])
    (super-new)
    
    (define/override (sample)
      (when (hash-empty? last-db)
        (eval-definition-thunk!))
      (define-values
        (last-p-db next-x-db next-p-db)
        (hmc-step last-db epsilon L hmc-naive-potential-fn))
      (define alpha
        (hmc-acceptance-threshold last-db last-p-db next-x-db next-p-db))
      (define u (log (random)))
      (cond [(< u alpha)
             (set! last-db next-x-db)
             (eval-definition-thunk!)
             answer]
            [else
             ;; restart
             (sample)]))
    
    (define/private (eval-definition-thunk!)
      (define delta-db (make-hash))
      (define current-db (make-hash))
      (define result
        (let/ec escape
          (parameterize ((current-stochastic-ctx
                          (new db-stochastic-ctx%
                               (current-db current-db)
                               (last-db last-db)
                               (delta-db delta-db)
                               (spconds null)
                               (escape escape))))
            (cons 'okay (apply/delimit thunk)))))
      (match result
        [(cons 'okay ans)
         (set! answer ans)]
        [(cons 'fail fail-reason)
         (when (verbose?)
           (eprintf "# Rejected condition (~s)" fail-reason))
         #f]))

    ))
    
;; hmc-naive-potential-fn : Address Position -> PotentialEnergy
;;
;; This potential function is WRONG.  It assumes that each
;; DB entry is independent of all the others.  An assumption that
;; only holds for samples drawn independently.
(define (hmc-naive-potential-fn k x)
  (cond [(hash-ref k x #f)
         => (λ (entry)
              (define dist (entry-dist entry))
              (define v (entry-value entry))
              ;; d/dt dist at v
              (dist-Denergy dist v))]
        [else 0]))

;; hmc-step : DB[X] Epsilon Positive-Integer -> (Values DB[P] DB[X*] DB[P*])
;;
;; Construct randomly an initial momentum, then run L epsilon-steps
;; of Hamiltonian dynamics to arrive at a new position and momentum.
(define (hmc-step curr-x-db epsilon L grad-potential-fn)
  (define curr-p-db
    (db-map (λ (e)
              ; pinned entries have zero kinetic energy
              ; and will thus not move in next-x-db.
              (let* ([d      (normal-dist 0 1)]
                     [v      (if (entry-pinned? e) 
                                 0
                                 (dist-sample d))]
                     [pinned (entry-pinned? e)])
                (entry d v pinned)))
            curr-x-db))
  (define-values (next-x-db next-p-db)
    (hmc-leapfrog-proposal epsilon L
                           grad-potential-fn
                           curr-x-db
                           curr-p-db))
  (values curr-p-db next-x-db next-p-db))
  
;; hmc-acceptance-threshold : DB[X] DB[P] DB[X*] DB[P*] -> Real
;;
;; Compute the acceptance ratio for a move from the given position and
;; momentum to a proposed position and momentum
(define (hmc-acceptance-threshold curr-x-db curr-p-db next-x-db next-p-db)
  (define curr-x-ll (db-ll curr-x-db))
  (define curr-p-ll (db-ll curr-p-db))

  (define next-x-ll (db-ll next-x-db))
  (define next-p-ll (db-ll next-p-db))
  
  (+ (- curr-x-ll next-x-ll)
     (- curr-p-ll next-p-ll)))

