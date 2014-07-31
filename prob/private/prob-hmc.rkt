;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base

(require racket/class
         racket/match
         (only-in racket/vector
                  vector-member)
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
      (when (verbose?)
        (eprintf "# accept threshold = ~s\n" (exp alpha)))
      (cond [(< u alpha)
             (when (verbose?)
               (eprintf "# Accepted HMC step with ~s\n" (exp u)))
             (set! last-db next-x-db)
             (eval-definition-thunk!)
             answer]
            [else
             ;; restart
             (when (verbose?)
               (eprintf "# Rejected HMC step with ~s\n" (exp u)))
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
;;
;; Compute the partial derivative of the energy U(x) total system x with
;; respect to the random variable xk with address k. 
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
(define ((hmc-gradient-potential-fn gradients) k x)
  (define (indicator i) (if (equal? k i) 1 0))
  (when (verbose?)
    (eprintf " computing derivative of energy with respect to ~e\n" k))

  (define denergy/dk
    (for/sum ([(i xi) (in-hash x)])
      (define xi-dist (entry-dist xi))
      (define xi-value (entry-value xi))
      (define i-param-vec (hash-ref gradients i))
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
        (when (verbose?)
          (eprintf "  dU(~e)/d~e = ~s\n" i k denergy-i/dk))
        denergy-i/dk)))
  
  (when (verbose?)
    (eprintf " denergy/d~e = ~s\n"
             k
             denergy/dk))

  denergy/dk)


;; nth-value : (-> (Values Any ...)) Exact-Nonnegative-Integer -> Any
;;
;; (nth-value producer n) evaluates producer and returns just its nth result.
(define (nth-value producer n)
  (call-with-values producer
    (λ results
      (list-ref results n))))

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
  
  (+ (- curr-x-ll) next-x-ll
     (- curr-p-ll) next-p-ll))

