;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble
(require racket/math
         (rename-in racket/match [match-define defmatch]))
(provide (all-defined-out))

;; Adapted from Mitch Wand's particle filter example.

;; ------------------------------------------------------------
;; State & Dynamics

;; A State is a Real -- hypothetical x-location for the airplane

(define XMAX 1000)
(define XINITMAX 500)
(define AIRSPEED 10)     ;; pixels/tick

(define/s (airplane-init _)
  (sample (uniform-dist 0 XINITMAX)))

;; airplane-after-step : State -> State
(define/s (airplane-after-step p)
  (+ p AIRSPEED (airspeed-noise)))

(define (airspeed-noise)  
  (sample (normal-dist 0 (/ AIRSPEED 4))))

;; ------------------------------------------------------------
;; Terrains

;; A Terrain is (State -> Real)

;; A very simple map
;; start at 0 and go to 1000 feet.
(define/s (terrain1 x)
  (* x 2))

;; A more complicated map
;; we're flying into the mountains...
;; maximum elevation is about 1600 feet
(define/s (terrain2 x)
  (* 4.0 
     (max 0.2 (abs (sin (/ (* x pi) 500)))) ; secular variation
     (+ (min (* x 2) 300)  ;; generally upwards slant, then level
        (* 100 (sin 
                (/ (* x pi) 100)))))) ; finer variation

(define/s (terrain3 x)
  ;; plateu [300,600]
  (cond [(< x 300)
         (terrain2 x)]
        [(> x 600)
         (terrain2 (- x 300))]
        [else
         (terrain2 300)]))

;; ------------------------------------------------------------
;; Altimeter

;; Altitude above sea level.
(define ALTITUDE 5000)

;; true-altimeter-reading : State Terrain -> Real
(define (true-altimeter-reading p terrain)
  (- ALTITUDE (terrain p)))

;; observed-altimeter-reading : State Terrain -> Real
(define (observed-altimeter-reading p terrain)
  (+ (true-altimeter-reading p terrain)
     (altimeter-noise)))

(define altimeter-std-dev 200)
(define (altimeter-noise)
  (sample (normal-dist 0 altimeter-std-dev)))

;; ------------------------------------------------------------
;; Observations

;; airplane-score : Terrain Real -> State -> State
;; Returns the same state; we're scoring the particle's state, 
;; not changing it.
(define/s ((airplane-score terrain observed-alt) p)
  (observe (observed-altimeter-reading p terrain) observed-alt)
  p)
