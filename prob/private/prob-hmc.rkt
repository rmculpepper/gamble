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

#|
HMC - Harmonic Monte Carlo

The usual database of random choices is now thought of as
the "position" of a particle and an auxiliary normally
distributed "momentum" random variable is added such that the
quantity:
  H(x,p) = U(x) + K(p)
is conserved.  Where U(x) is the "potential energy" of the position
and K(p) is the "kinetic energy" of the momentum.
|#

;; hmc-sampler* : (-> (-> A)) -> Sampler
;; The idea is that when the sampler runs the definition thunk, it will
;; populate the random choice DB and return a (non-stochastic) answer thunk
;; which closed over the random variables (ie the DB) of the definition.
(define (hmc-sampler* definition-thunk)
  (new hmc-sampler% (definition-thunk definition-thunk)))

(define hmc-sampler%
  (class sampler-base%
    (init-field definition-thunk)
    (field [last-db '#hash()]
           [answer-thunk #f])
    (super-new)
    
    (define/override (sample)
      (unless answer-thunk
        (eval-definition-thunk!))
      
      (answer-thunk))
    
    (define (eval-definition-thunk!)
      (define delta-db (make-hash))
      (define current-db (make-hash))
      (define first-run-result
        (let/ec escape
          (parameterize ((current-stochastic-ctx
                          (new db-stochastic-ctx%
                               (current-db current-db)
                               (last-db last-db)
                               (delta-db delta-db)
                               (spconds null)
                               (escape escape))))
            (cons 'okay (apply/delimit definition-thunk)))))
      
      (match first-run-result
        [(cons 'okay answer)
         (set! answer-thunk answer)]
        [(cons 'fail fail-reason)
         (when (verbose?)
           (eprintf "# Rejected condition (~s)" fail-reason))
         #f]))

    ))
    

;; hmc-step : DB[X] Epsilon Positive-Integer -> (Values DB[P] DB[X*] DB[P*])
;;
;; Construct randomly an initial momentum, then run L epsilon-steps
;; of Hamiltonian dynamics to arrive at a new position and momentum.
(define (hmc-step curr-x-db epsilon L grad-potential-fn)
  (define curr-p-db
    (db-map (λ (e)
              ; pinned entries have zero momentum,
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

