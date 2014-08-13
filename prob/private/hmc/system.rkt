;; Copyright (c) 2014 BAE Systems, Inc.
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.
#lang racket/base

(require racket/match
         "../db.rkt"
         (only-in "../interfaces.rkt"
                  some-zone-matches?)
         (only-in "../../dist.rkt"
                  normal-dist)
         (only-in "../dist.rkt"
                  dist-sample
                  dist-pdf))

(provide (all-defined-out))

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
  (match-define (struct hmc-system (x p)) old-sys)
  (hmc-system (evolve-X x p) p))

;; hmc-system-evolve-P : HMC-System (DB[X] DB[P] -> DB[P]) -> HMC-System
;;
;; Construct a new system where the momentum has evolved using the given function.
;; N.B.: the new system aliases the position database of the old system.
(define (hmc-system-evolve-P old-sys evolve-P)
  (match-define (struct hmc-system (x p)) old-sys)
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
