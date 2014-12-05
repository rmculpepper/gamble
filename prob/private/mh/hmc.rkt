;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.
#lang racket/base
(require racket/contract
         racket/class
         (rename-in racket/match [match-define defmatch])
         racket/vector
         "db.rkt"
         "base.rkt"
         "../interfaces.rkt"
         (only-in "../../dist/univariate.rkt" normal-dist)
         "../dist.rkt")
(provide (all-defined-out))

#|
(provide 
 (contract-out
  [hmc-leapfrog-proposal
   (-> (>/c 0)
       exact-nonnegative-integer?
       (-> any/c hash? real?)
       hmc-system?
       (-> any/c)
       any/c ;; ZonePattern
       (or/c (list/c 'okay any/c hmc-system?)
             (list/c 'fail any/c)))]))
|#

;; Hamiltonian Monte Carlo - System

;; An HMC system is a pair of:
;; - a DB of random choices, now thought of as the position X;
;; - a DB of synthetic random choices that are the momentum P of the system.
;;
;; Coherence:
;;   - Each address k that appears in P also appears in both X.
;;   - if there is some zone z such that X can be partitioned into
;;     disjoint Xz Xnz such that all entries in Xz are in z and none
;;     in Xnz are in z, then each address in P is in Xz and not in Xnz.
;;   - Corresponding entires x, p with the same address
;;       obey: (and (equal? (entry-pinned? x) (entry-pinned? p))
;;                  (equal? (entry-zones x) (entry-zones p)))
;;
;; N.B. while the system is immutable, the hashes may change.
(struct hmc-system (X P) #:prefab)

;; hmc-system-copy :: HMC-System -> HMC-System
;;
;; Make a new copy of the given system by creating new databases
;; and copying over their entries.
(define (hmc-system-copy old-sys)
  (define new-sys (hmc-system (make-hash) (make-hash)))
  (for ([(k x) (in-hash (hmc-system-X old-sys))]
        [p     (in-value (hash-ref (hmc-system-P old-sys)))])
    (hash-set! (hmc-system-X new-sys) k x)
    (hash-set! (hmc-system-P new-sys) k p)))

;; hmc-system-evolve-X : HMC-System (DB[X] DB[P] -> DB[X]) -> HMC-System
;;
;; Construct a new system where the position has evolved using the given function.
;; N.B.: the new system aliases the momentum database of the old system.
(define (hmc-system-evolve-X old-sys evolve-X)
  (defmatch (struct hmc-system (x p)) old-sys)
  (hmc-system (evolve-X x p) p))

;; hmc-system-evolve-P : HMC-System (DB[X] DB[P] -> DB[P]) -> HMC-System
;;
;; Construct a new system where the momentum has evolved using the given function.
;; N.B.: the new system aliases the position database of the old system.
(define (hmc-system-evolve-P old-sys evolve-P)
  (defmatch (struct hmc-system (x p)) old-sys)
  (hmc-system x (evolve-P x p)))
                

;; ------------------------------------------------------------
;; Momentum synthesis

;; synthesize-K-entry : ZonePattern -> (Entry[X] -> Entry[P])
;;
;; Given an entry, return a new entry entry representing its momentum.
;; The momentum value is sampled from (normal-dist 0 1) except in the case
;; of pinned positions which get a value of 0.0.
;;
;; Momentum entries do not have a zone.  (Should they?)
(define ((synthesize-P-entry zone) x)
  ;; pinned entries have zero kinetic energy
  ;; and will thus not move in next-x-db.
  (let* ([d      (normal-dist 0 1)]
         [v      (if (or (entry-pinned? x) 
                         (not (some-zone-matches? (entry-zones x) zone)))
                     0
                     (dist-sample d))]
         [pinned (entry-pinned? x)])
    (entry (entry-zones x) d v (dist-pdf d v #t) pinned)))
  
;; synthesize-K-db : DB[X] ZonePattern -> DB[P]
;;  Given a database of positions, returns a database of momenta
;; where the momentum of each random choice is sampled independently from (normal-dist 0 1).
(define (synthesize-P-db x zone) (db-map (synthesize-P-entry zone) x))


;; ------------------------------------------------------------
;; System Energy

;; The energy in an HMC model is the sum of:
;;  Potential Energy:  U(X) = -log pdf(X)
;;    where X is the DB of random variables, now thought of as the "position"
;;  Kinetic Energy:  K(P) = ((dot (conjP) P) / 2)
;;    where P is a DB of synthetic random variables each drawn from (normal-dist 0 1)
;;    which represents the "momentum" of the system.
;;
;; We will sometimes write DB[X] for the database of positions, and
;; DB[P] for the dababase of momenta.

;; db-kinetic-energy : DB[P] -> Real
;;
;; The kinetic energy of a momentum is (/ (dot (conj p) p) 2).
;;
;; Note in particular that unlike the potential energy, the kinetic energy is a function
;; of the values of the random choices, not of their likelihoods.
(define (db-kinetic-energy ps)
  (/ (for/sum ([e (in-hash-values ps)])
            (let ([p (entry-value e)])
              (* p p)))
     2))

;; db-potential-energy : DB[X] -> Real
;;
;; The potential energy of a database of positions X is -log pdf(X)
(define (db-potential-energy xs)
  (- (db-ll xs)))

;; system-energy : HMC-System -> Real
;;
;; The energy of an HMC system is a sum U(X) + K(P)
(define (hmc-system-energy sys)
  (+ (db-potential-energy (hmc-system-X sys))
     (db-kinetic-energy (hmc-system-P sys))))

;; ============================================================

;; hmc-acceptance-threshold : HMC-System HMC-System -> (Values Real Real)
;;
;; Compute the acceptance ratio for a move from the current system
;; to the proposed system.
;; Returns (values next-energy threshold) where next-energy is energy of the
;; proposed system and threshold is the acceptance threshold.
(define (hmc-acceptance-threshold curr-sys next-sys)
  (define next-energy (hmc-system-energy next-sys))
  (define curr-energy (hmc-system-energy curr-sys))
  
  ; (x0 + p0) - (x* + p*)
  
  (define threshold
    ; Do I need to worry about cancellation here?
    (- curr-energy next-energy))
  
  (values next-energy threshold))

;; ============================================================

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
;;  ZonePattern
;;  -> (List 'okay Any HMC-System)
;;     | (List 'fail Any)
(define (hmc-leapfrog-proposal
         epsilon
         L
         grad-potential-fn
         sys0
         thunk
         zone)
  (let* ([half-epsilon  (/ epsilon 2.0)]
         [P-half-step   (momentum-step half-epsilon grad-potential-fn)]
         [P-step        (momentum-step epsilon grad-potential-fn)]
         [escape-prompt (make-continuation-prompt-tag)]
         [bad-step      (λ (reason)
                          (abort-current-continuation
                           escape-prompt
                           (λ () (list 'fail reason))))]
         [propagate-X   (propagate-X-changes-to-model thunk bad-step)]
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

(define ((propagate-X-changes-to-model thunk escape) x delta-X
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
                   (delta-db delta-X)))
  (define run-result (send ctx run thunk))
  (match run-result
    [(cons 'okay result) 
     (when record-result-box
       (set-box! record-result-box result))
     (get-field current-db ctx)]
    [(cons 'fail reason) (escape reason)]))

;; ============================================================

;; hmc-step : HMC-System
;;            Epsilon Positive-Integer
;;            (Address Position -> PotentialEnergy)
;;            (-> Any)
;;            ZonePattern
;;            -> (List 'okay HMC-System Any HMC-System)
;;               | (List 'fail Any)
;;
;; Construct randomly an initial momentum, then run L epsilon-steps
;; of Hamiltonian dynamics to arrive at a new position and momentum.
;;
;; Returns the initial system (with synthesized momentum), a sample value and the
;; final system (evolved after L epsilon-steps).
(define (hmc-step last-sys epsilon L grad-potential-fn thunk zone)
  (define init-sys (hmc-system-evolve-P last-sys (λ (x p) (synthesize-P-db x zone))))
  (define step-result
    (hmc-leapfrog-proposal epsilon L
                           grad-potential-fn
                           init-sys
                           thunk
                           zone))
  (match step-result
    [(list 'okay sample-value new-sys) 
     (list 'okay init-sys sample-value new-sys)]
    [(list 'fail reason)  step-result]))

;; ============================================================

;; db-Denergy : (Hashof Address (Vectorof PartialDerivatives))
;;              ZonePattern
;;              -> (Address DB[Position] -> PotentialEnergy)
;;
;; Computes the partial derivative of the energy U(x) of total system x with
;; respect to the random variable xk with address k. 
;;
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
(define ((db-Denergy param-partials zone) k x)
  (define (indicator i) (if (equal? k i) 1 0))
  (when #f 
    (when (verbose?)
      (eprintf " computing derivative of energy with respect to ~e\n" k)))

  (define denergy/dk
    (for/sum ([(i xi) (in-hash x)]
              #:when (some-zone-matches? (entry-zones xi) zone)
              )
      (define xi-dist (entry-dist xi))
      (define xi-value (entry-value xi))
      (define i-param-vec (hash-ref param-partials i))
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
        (when #f
          (when (verbose?)
            (eprintf "  dU(~e)/d~e = ~s\n" i k denergy-i/dk)))
        denergy-i/dk)))
  
  (when #f
    (when (verbose?)
      (eprintf " denergy/d~e = ~s\n"
               k
               denergy/dk)))

  denergy/dk)

;; nth-value : (-> (Values Any ...)) Exact-Nonnegative-Integer -> Any
;;
;; (nth-value producer n) evaluates producer and returns just its nth result.
(define (nth-value producer n)
  (call-with-values producer
    (λ results
      (list-ref results n))))


;; ============================================================

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
|#
(define hmc-transition%
  (class mh-transition-base%
    (init-field epsilon 
                L
                [zone #f])
    (field [gradients #f])
    (super-new)

    (define/override (info i)
      (iprintf i "== Transition (hmc ~e ~e #:zone ~e)\n" epsilon L zone)
      (super info i))

    (define/override (run* thunk last-trace)
      (define last-trace*
        (cond [(and gradients (not (equal? last-trace init-trace)))
               last-trace]
              [else
               (run/collect-gradients thunk last-trace)]))
      (when #f
        (eprintf "collected gradients ~e\n" gradients))
      (run/hmc thunk last-trace*))

    (define/private (run/collect-gradients thunk last-trace)
      (define delta-db '#hash())
      (define last-db (trace-db last-trace))
      (define ctx 
        (new db-stochastic-derivative-ctx%
             (last-db last-db)
             (delta-db delta-db)))
      (define result (send ctx run thunk))
      (define ans-db (get-field current-db ctx))
      (define grads (get-field derivatives ctx))
      (when (verbose?)
        (eprintf " (recorded derivatives are) ~e\n" grads)
        (eprintf " (relevant labels were) ~e \n" (get-field relevant-labels ctx)))
      (match result
        [(cons 'okay ans)
         (set! gradients grads)
         (trace ans ans-db
                (get-field nchoices ctx)
                (get-field ll-free ctx)
                (get-field ll-obs ctx))]
        [(cons 'fail fail-reason)
         (run/collect-gradients thunk last-trace)]))

    (define/private (run/hmc thunk last-trace)
      (define last-accepted-sys
        (hmc-system (trace-db last-trace) (make-hash)))
      (define step-result
        (hmc-step last-accepted-sys epsilon L (db-Denergy gradients zone)
                  thunk zone))
      (match step-result
        [(list 'fail reason)
         step-result]
        [(list 'okay initial-sys val proposal-sys)
         (let-values ([(proposal-energy alpha)
                       (hmc-acceptance-threshold initial-sys proposal-sys)])
           (when (verbose?)
             (eprintf "# Previous system energy: ~e (K = ~e + U = ~e)\n"
                      (hmc-system-energy initial-sys)
                      (db-kinetic-energy (hmc-system-P initial-sys))
                      (db-potential-energy (hmc-system-X initial-sys)))
             (eprintf "# Proposal energy: ~e (K = ~e + U = ~e)\n"
                      proposal-energy
                      (db-kinetic-energy (hmc-system-P proposal-sys))
                      (db-potential-energy (hmc-system-X proposal-sys)) ))
           (cons alpha (hmc-system->trace last-trace val proposal-sys)))]))

    (define/private (hmc-system->trace last-trace sample-value proposal-sys)
      (defmatch (trace _ _ last-nchoices _ _) last-trace)
      (define proposal-db (hmc-system-X proposal-sys))
      ; XXX - is this right?
      (define ll-free 0.0)
      (define ll-obs 0.0)
      (trace sample-value proposal-db last-nchoices ll-free ll-obs))

    ))
