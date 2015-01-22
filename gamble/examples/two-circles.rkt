#lang gamble
(require racket/class
         (prefix-in p: plot))

(define OBS-STDDEV 0.01)
(define HBURN-IN 5000)
(define HSAMPLES 5000)

(define sh
  (hmc-sampler
   (define mode (with-zone 'Mode (flip)))
   (define radiusSq (if mode 2.1 0.9))
   (define x (with-zone 'Point (label 'x (derivative (normal 0 4) #f #f))))
   (define y (with-zone 'Point (label 'y (derivative (normal 0 4)
                                   #f
                                   #f))))
   (define r2 (+ (* x x) (* y y)))
   (label 'obs
          (with-zone 'Point
                     (derivative (observe-sample (normal-dist (- r2 radiusSq) OBS-STDDEV) 0)
                                 [(x y)
                                  (lambda (x y) (values (* 2 x) (* 2 y)))]
                                 #f)))
   (vector x y #|(sqrt r2)|#)
   #:epsilon 0.01
   #:L 30))

(send sh set-transition (multi-site))
(void (for ([i HBURN-IN]) (sh)))
(send sh set-transition
      (cycle (multi-site)
             (hmc 0.005 30 #:zone 'Point)
             (hmc 0.005 30 #:zone 'Point)))

(define samples-h (repeat sh HSAMPLES))

(p:plot #:x-min -2 #:x-max 2 #:y-min -2 #:y-max 2 (p:points samples-h))
(send sh info)
