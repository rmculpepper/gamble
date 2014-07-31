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
           [answer #f]
           [gradients '#hash()])
    (super-new)
    
    (define/override (sample)
      (when (or (not last-db) (hash-empty? last-db))
        (let ([current-db (eval-definition-thunk!)])
          (set! last-db current-db)))
      (define-values
        (last-p-db next-x-db next-p-db)
        (hmc-step last-db epsilon L (hmc-gradient-potential-fn gradients)))
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
                          (new db-stochastic-derivative-ctx%
                               (current-db current-db)
                               (last-db last-db)
                               (delta-db delta-db)
                               (spconds null)
                               (escape escape))))
            (begin0
                (list 'okay 
                      (apply/delimit thunk)
                      current-db
                      (get-field derivatives (current-stochastic-ctx)))
              (when (verbose?)
                (eprintf " (recorded derivatives are) ~e\n"
                         (get-field derivatives (current-stochastic-ctx)))
                (eprintf " (relevant labels were) ~e \n"
                         (get-field relevant-labels (current-stochastic-ctx))))))))
      (match result
        [(list 'okay ans ans-db grads)
         (set! answer ans)
         (set! gradients grads)
         ans-db]
        [(cons 'fail fail-reason)
         (when (verbose?)
           (eprintf "# Rejected condition (~s)" fail-reason))
         #f]))
    ))
    
;; hmc-naive-potential-fn : Address Position -> PotentialEnergy
;;
;; This potential function is WRONG to use on its own.  It assumes
;; that each DB entry is independent of all the others.  An assumption
;; that only holds for samples drawn independently.
(define (hmc-naive-potential-fn k x)
  (cond [(hash-ref x k #f)
         => (λ (entry)
              (define dist (entry-dist entry))
              (define v (entry-value entry))
              ;; d(-log(distpdf))/dt at v
              (dist-Denergy dist v))]
        [else 0]))

;; hmc-gradient-potential-fn : (Hashof Address (Vectorof PartialDerivatives))
;;                             -> (Address Position -> PotentialEnergy)
(define ((hmc-gradient-potential-fn gradients) k x)
  (cond [(hash-ref gradients k #f)
         => (λ (gradient-vec)
              (define k-entry (hash-ref x k))
              (define k-dist (entry-dist k-entry))
              (define k-dist-num-params (dist-param-count k-dist))
              (define v (entry-value k-entry))
              (unless (and (vector? gradient-vec)
                           (= (vector-length gradient-vec)
                              k-dist-num-params))
                (error 'hmc-gradient-potential-fn
                       " recorded derivative for ~e has ~e gradients, but the distribution ~e has ~e params"
                       k
                       (vector-length gradient-vec)
                       k-dist
                       k-dist-num-params))
              (define param-d/dts
                (for/list ([partial-deriv-entry (in-vector gradient-vec)])
                  (if (not partial-deriv-entry)
                      0
                      (let ([dependent-addrs-vec (car partial-deriv-entry)]
                            [partial-deriv-fn (cdr partial-deriv-entry)])
                        (define dependent-values
                          (for/list ([a (in-vector dependent-addrs-vec)])
                            (entry-value (hash-ref x a))))
                        (call-with-values (λ () (apply partial-deriv-fn dependent-values))
                          (λ param-partial-vs
                            ;; we are working on param(v1(t), ... vN(t)) = <something>   
                            ;; and we just got param-partial-vs which is dparam/dv for each v1... vN
                            ;; we want dparam/dt, which is the total derivative of param
                            ;;   dparamt/dt = dparam/dv1 * dv1/dt + ... + dparam/dvN * dvN/dt
                            ;;
                            ;; Example: suppose param(y1,y2) = y1^2*y2^2
                            ;; so, dparam(y1,y2)/dt = d(y1^2)/dy1 * y2^2 * dy1/dt + d(y2^2)/dy2 * y1^2 * dy2/dt
                            ;;                      = 2*y1*y2^2 * dy1/dt + 2*y2*y1^2 * dy2/dt
                            ;;
                            ;; So we need to sum up while recursively calling ourselves to compute
                            ;; the time derivatives of the variables we depend on.
                            (for/sum ([dparam/dv param-partial-vs]
                                      [a (in-vector dependent-addrs-vec)]
                                      [v dependent-values])
                              ;; one more complication: we don't have dv/dt directly,
                              ;; we have Denergy: d(-log v)/dt = - d(log v)/dt = - 1/v * dv/dt
                              ;; so dv/dt = - v * d(-log v)/dt 
                              ;; so overall we want
                              ;;  dparam/dt = dparam/dv * (- v) * d(-log v)/dt
                              (define v-Denergy ((hmc-gradient-potential-fn gradients) a x))
                              (* dparam/dv (- v) v-Denergy))))))))
              (apply dist-Denergy k-dist v 1 param-d/dts))]
        [else
         (error 'hmc-gradient-potential-fn
                " no derivative available for distribution with address ~e"
                k)]))
                       

;; hmc-step : DB[X] Epsilon Positive-Integer (Address Position -> PotentialEnergy)
;;            -> (Values DB[P] DB[X*] DB[P*])
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

