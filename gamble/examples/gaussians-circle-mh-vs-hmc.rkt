#lang gamble
(require racket/class
         (prefix-in p: plot))

;; With these settings, HMC sometimes has accept ratio ~ 0%.
;; Maybe this is extreme sensitivity to starting position?
;; (Maybe MH then HMC would be a better strategy?)

(define OBS-STDDEV 0.01)
(define BURN-IN 5000)
(define SAMPLES 5000)
(define HBURN-IN 5000)
(define HSAMPLES 5000)

;; Both do much worse if OBS-STDDEV = 0.05

(define s
  (mh-sampler
   (define x (normal 0 1))
   (define y (normal x 1))
   (define r2 (+ (* x x) (* y y)))
   (observe-sample (normal-dist r2 OBS-STDDEV) 1)
   (vector x y #|(sqrt r2)|#)))

(define sh
  (hmc-sampler
   (define x (label 'x (derivative (normal 0 1) #f #f)))
   (define y (label 'y (derivative (normal x 1)
                                   [(x) (lambda (x) 1)]
                                   #f)))
   (define r2 (+ (* x x) (* y y)))
   (label 'obs
          (derivative (observe-sample (normal-dist r2 OBS-STDDEV) 1)
                      [(x y)
                       (lambda (x y) (values (* 2 x) (* 2 y)))]
                      #f))
   (vector x y #|(sqrt r2)|#)
   #:epsilon 0.01
   #:L 30))

(void (for ([i BURN-IN]) (s)))
(send sh set-transition (multi-site))
(void (for ([i HBURN-IN]) (sh)))
(send sh set-transition (hmc 0.005 30))

(define samples (repeat s SAMPLES))
(define samples-h (repeat sh HSAMPLES))

(p:plot #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5 (p:points samples))
(send s info)

(p:plot #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5 (p:points samples-h))
(send sh info)
