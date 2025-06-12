;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang gamble
(require slideshow/pict
         (rename-in racket/match [match-define defmatch])
         "par-pf-airplane-model.rkt")
(provide (all-defined-out))

;; The model needs to be in a separate module from the GUI code, else
;; the places try to restart racket/gui (error).

;; ------------------------------------------------------------
;; Simulation & Inference

(define NPARTICLES 100)
(define STEPS 100)
(define terrain terrain3)

(define xtrues (make-vector STEPS))
(vector-set! xtrues 0 (airplane-init #f))
(for ([i (in-range 1 STEPS)])
  (vector-set! xtrues i (airplane-after-step (vector-ref xtrues (sub1 i)))))

(define observed-altitudes
  (for/vector ([xtrue (in-vector xtrues)])
    (observed-altimeter-reading xtrue terrain)))

(define particless (make-vector STEPS))
(for ([i (in-range STEPS)]
      [obs (in-vector observed-altitudes)])
  (let* ([prev-ps
          (cond [(zero? i)
                 (particles-update (make-parallel-particles NPARTICLES #f)
                                   airplane-init)]
                [else
                 (particles-resample
                  (vector-ref particless (sub1 i)))])]
         [curr-ps (particles-update prev-ps airplane-after-step)]
         [curr-ps (particles-score curr-ps (airplane-score terrain obs))])
    (vector-set! particless i curr-ps)))

(define (particles-mean ps)
  (define-values (ssum wsum)
    (for/fold ([ssum 0] [wsum 0])
              ([wstate (in-vector (particles-weighted-states ps))])
      (defmatch (cons s w) wstate)
      (values (+ ssum (* s w)) (+ wsum w))))
  (/ ssum wsum))

;; ============================================================
;; Visualization

(require racket/class
         racket/vector
         plot/pict
         gamble/viz/multi)

(define (visualize [plotter plot-both])
  (define mv (make-multi-viz-frame #:label "Airplane Localization"))
  (for ([xtrue xtrues]
        [ps particless])
    (send mv add-pict (plotter xtrue ps))))

(define (plot-both xtrue ps)
  (hc-append 10 (plot-simulation xtrue ps) (plot-likelihood xtrue ps)))

(define (plot-simulation xtrue ps)
  (define wstates (particles-weighted-states ps))
  (define positions (vector-map car wstates))
  (define xest (particles-mean ps))
  (plot
   (list (function-interval terrain (lambda (p) 0) #:color "green")
         (lines (list (vector xest (+ ALTITUDE 100)) (vector xest (- ALTITUDE 100)))
                #:label "estimate" #:style 'dot #:color "red")
         (points (for/list ([p positions]) (vector p ALTITUDE))
                 #:label "particles" #:size 2 #:color "red")
         (points (list (vector xtrue ALTITUDE))
                 #:label "true position"
                 #:size 4
                 #:sym 'fulldiamond
                 #:color "blue"))
   #:x-label "position"
   #:y-label "altitude"
   #:x-min 0 #:x-max XMAX
   #:y-min 0 #:y-max 6000))

(define (plot-likelihood xtrue ps)
  (define wstates (particles-weighted-states ps))
  (define positions (vector-map car wstates))
  (define weights (vector-map log (vector-map cdr wstates)))
  (define xest (particles-mean ps))
  (plot 
   (list (lines (list (vector xtrue 0) (vector xtrue -10))
                #:color "blue"
                #:style 'long-dash
                #:label "target")
         (lines (list (vector xest 0) (vector xest -10))
                #:color "red"
                #:style 'dot
                #:label "mean estimate")
         (points (vector->list (vector-map vector positions weights))
                 #:label "log probability"
                 #:size 8
                 #:color 'red))
   #:x-label "position"
   #:y-label "log likelihood"
   #:x-min 0 #:x-max XMAX
   #:y-min -10 #:y-max 0))
