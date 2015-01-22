#lang gamble
(require math/statistics
         rackunit)

#|

TODO:
  
Maybe make this test suite into a scribble page, so it can be self-documenting.  

A nice display with a histogram of the bootstrap values and the expected value.
Do a little research on the behavior of bootstrap estimates, so there can be some 
justification in the text. 

Same test cases as in summer school and for abstract interpreter. [?]

|#

;;;;;;;;;;;;;;;Some testing utilities ;;;;;;;;;;;;;;;;;

(define-syntax-rule (tc name body ...)
  (test-case name (printf "Test: ~a\n" name) body ...))

;; compare-results : num, [a], ([a] -> num) -> void
;;
;; For cases where we know the correct values for some statistics of the posterior 
;; distribution. If we believe that the expectation of "fn" over the posterior from
;; "samples" is drawn is "expected", then the value of "fn" over most resamplings of
;; "samples" should be near "expected".  Specifically, "expected" should be in the 
;; central 95% of the values of "fn" over those (5000, but shrink if patience fails)
;; resamplings.
(define (compare-results expected samples fn)
  (let* ((bounds (bootstrap-bounds samples fn 2000))
         (low (exact->inexact (car bounds)))
         (high (exact->inexact (cadr bounds))))
      (cond [(< low expected high) 
             (printf "Expected value ~a is in central 95% interval [~a,~a]\n\n"
                     expected low high)]
            [else 
             (printf "Expected value ~a is NOT in central 95% interval [~a,~a]\n\n" 
                     expected low high)
             (error "value not in interval")])))

;; trim-and-thin : [a], int, int -> [a]
;;
;; Drop the first burn-in samples and then take every lag-th one
;; thereafter.  This is not clever or lazy.  Must have big list in hand
;; to thin.
;;
;; TODO: maybe build trimming off burn-in and thinning samples in to Ryan's 
;; sampling procedures, or allow lazy composition therewith...
(define (trim-and-thin samples burn-in lag)
  (let ((s (list-tail samples burn-in)))
    (take-every lag s)))

(define (take-every k lst)
  (for/list ([x (in-list lst)]
             [i (in-naturals)]
             #:when (zero? (remainder i k)))
    x))


;; resample : [a] ~~> [a]
;;
;; Given list sample draw at most 3000 items (with replacement) from 
;; that list.
;;
;; TODO:
;; Not taking a sample of the same size as the original violates 
;; official bootstrap practice, so if I want to continue with this
;; I have to figure out the correction factor.
(define (resample sample)
  (let ([sample (list->vector sample)])
    (let ((n (vector-length sample)))
      (repeat (lambda () (vector-ref sample (random n))) (min n 8000)))))


;; bootstrap-bounds : [a], ([a] -> num), int ~~> (num,num)
;;
;; Given list "sample" and function fn, resample n-reps times and then
;; compute quantiles for central 95% of fn over random resamplings.
(define (bootstrap-bounds sample fn n-reps)
  (define (inner n ms)
    (if (= n 0)
         (list      
          (quantile 0.025 < ms)
          (quantile 0.975 < ms))
        (let ((s (resample sample)))
          (inner (- n 1) (cons (fn s) ms)))))
  (inner n-reps '()))

(define first car)
(define second cadr)
(define third caddr)
(define (one-if x)
  (if x 1 0))


;;;;;;;;;;;;;;; Some test problems.  ;;;;;;;;;;;;;;;;;;;;;

;; "Asia" BN--expected results from Netica
;; With no evidence:
(define asia-marginal
  (mh-sampler
   (define visit-to-asia (flip 0.01))
   (define smoker (flip 0.5))
   (define tuberculosis (if visit-to-asia (flip 0.05) (flip 0.01)))
   (define lung-cancer (if smoker (flip 0.1) (flip 0.01)))
   (define bronchitis (if smoker (flip 0.6) (flip 0.3)))
   (define tb-or-c (or tuberculosis lung-cancer))
   (define x-ray-abnormal (if tb-or-c (flip 0.98) (flip 0.05)))
   (define dyspnea (cond [(and tb-or-c bronchitis) (flip 0.9)]
                         [tb-or-c (flip 0.7)]
                         [bronchitis (flip 0.8)]
                         [else (flip 0.1)]))
   (map one-if 
        (list dyspnea tuberculosis lung-cancer)) 
   ))

(define asia-marginal-samples 
  (trim-and-thin (repeat asia-marginal 10000) 3500 5))

(tc "dyspnea"
    (define dyspnea-marginal-samples 
      (map first asia-marginal-samples))
    (printf "Marginal probability of dyspnea.\n")
    (compare-results 0.436 dyspnea-marginal-samples mean))
(tc "tuberculosis"
    (define tb-marginal-samples 
      (map second asia-marginal-samples))
    (printf "Marginal probability of tuberculosis.\n")
    (compare-results 0.0104 tb-marginal-samples mean))
(tc "lung cancer"
    (define lung-cancer-marginal-samples 
      (map third asia-marginal-samples))
    (printf "Marginal probability of lung cancer.\n")
    (compare-results 0.055 lung-cancer-marginal-samples mean))


;; With an abnormal x-ray
;; Lung-cancer posterior here is currently (as of 6/26/14) much too low.
;; Webchurch gets it right, so we are not reproducing their algorithm.
(define asia-abnormal-x-ray
  (mh-sampler
   (define visit-to-asia (flip 0.01))
   (define smoker (flip 0.5))
   (define tuberculosis (if visit-to-asia (flip 0.05) (flip 0.01)))
   (define lung-cancer (if smoker (flip 0.1) (flip 0.01)))
   (define bronchitis (if smoker (flip 0.6) (flip 0.3)))
   (define tb-or-c (or tuberculosis lung-cancer))
   (define x-ray-abnormal (if tb-or-c (flip 0.98) (flip 0.05)))
   (define dyspnea (cond [(and tb-or-c bronchitis) (flip 0.9)]
                         [tb-or-c (flip 0.7)]
                         [bronchitis (flip 0.8)]
                         [else (flip 0.1)]))
   (map one-if 
        (list dyspnea tuberculosis lung-cancer)) 
   #:when x-ray-abnormal
   ))


(define asia-ab-x-samples 
  (trim-and-thin (repeat asia-abnormal-x-ray 150000) 3500 50))

(tc "dyspnea given abnormal x-ray"
    (define dyspnea-ab-x-samples 
      (map first asia-ab-x-samples))
    (printf "Probability of dyspnea given abnormal x-ray.\n")
    (compare-results 0.641 dyspnea-ab-x-samples mean))
(tc "tuberculosis given abnormal x-ray"
    (define tb-ab-x-samples 
      (map second asia-ab-x-samples))
    (printf "Probability of tuberculosis given abnormal x-ray.\n")
    (compare-results 0.0924 tb-ab-x-samples mean))
(tc "lung cancer given abnormal x-ray"
    (define lung-cancer-ab-x-samples 
      (map third asia-ab-x-samples))
    (printf "Probability of lung cancer given abnormal x-ray.\n")
    (compare-results 0.489 lung-cancer-ab-x-samples mean))


;; With an abnormal x-ray and smoking
(define asia-abnormal-x-ray-and-smoker
  (mh-sampler
   (define visit-to-asia (flip 0.01))
   (define smoker (flip 0.5))
   (define tuberculosis (if visit-to-asia (flip 0.05) (flip 0.01)))
   (define lung-cancer (if smoker (flip 0.1) (flip 0.01)))
   (define bronchitis (if smoker (flip 0.6) (flip 0.3)))
   (define tb-or-c (or tuberculosis lung-cancer))
   (define x-ray-abnormal (if tb-or-c (flip 0.98) (flip 0.05)))
   (define dyspnea (cond [(and tb-or-c bronchitis) (flip 0.9)]
                         [tb-or-c (flip 0.7)]
                         [bronchitis (flip 0.8)]
                         [else (flip 0.1)]))
   (map one-if 
        (list dyspnea tuberculosis lung-cancer)) 
   #:when (and x-ray-abnormal smoker)
   ))


(define asia-ab-x-smoker-samples 
  (trim-and-thin 
   (repeat asia-abnormal-x-ray-and-smoker 150000) 3500 50))

(tc "dyspnea given abnormal x-ray and smoker"
    (define dyspnea-ab-x-smoker-samples 
      (map first asia-ab-x-smoker-samples))
    (printf "Probability of dyspnea given abnormal x-ray and smoker.\n")
    (compare-results 0.732 dyspnea-ab-x-smoker-samples mean))
(tc "tuberculosis given abnormal x-ray and smoker"
    (define tb-ab-x-smoker-samples 
      (map second asia-ab-x-smoker-samples))
    (printf "Probability of tuberculosis given abnormal x-ray and smoker.\n")
    (compare-results 0.0672 tb-ab-x-smoker-samples mean))
(tc "lung cancer given abnormal x-ray and smoker"
    (define lung-cancer-ab-x-smoker-samples 
      (map third asia-ab-x-smoker-samples))
    (printf "Probability of lung cancer given abnormal x-ray and smoker.\n")
    (compare-results 0.646 lung-cancer-ab-x-smoker-samples mean))



;; Binomial.  Mean should be n*p.  Variance should be n*p*(1-p)
(define (sum-n-flips n p)
    (if (zero? n)
        0
        (+ (if (flip p) 1 0)
           (sum-n-flips (sub1 n) p))))

;; Unconditional.  Mean should be around 10, Var around 9.
(define s-flips 
  (mh-sampler 
   (sum-n-flips 100 0.1)))


(define s-flip-samples 
  (trim-and-thin (repeat s-flips 8000) 2500 5))
(printf "Mean sum of weighted coin flips.\n")
(compare-results 10 s-flip-samples mean)
(printf "Variance of sum of weighted coin flips.\n")
(compare-results 9 s-flip-samples variance)

;; Conditional. Given that at least 5 flips are true, how
;; many out of 10 are true, with p = 0.3?
;; Exact answer from octave: 




;; Coin flips. Conditional on one or the other being true, prob
;; of first one being true is 2/3.
(define two-coins
  (mh-sampler 
   (define c1 (flip))
   (define c2 (flip))
   (if c1 1 0)
   #:when (or c1 c2)))

(printf "First of two coin flips, given that at least one is true.\n")
(compare-results 0.666667 (trim-and-thin (repeat two-coins 8000) 2500 5) mean)


;; Beta/bernoulli--we know that with prior (beta 1 2) and five "true" observations,
;; the posterior is (beta 6 2), which has mean 6/(6+2) = 0.75, and variance
;; 6*2 / [(6 + 2)^2 * (6 + 2 + 1)] = 12/8*8*9 = 3/2*8*9 = 1/2*8*3 = 1/48.
(define beta-the-hard-way
  (mh-sampler
   (define p (beta 1 2))
   (define coin (mem (lambda (n) (flip p))))
   p
   #:when (and (coin 1) (coin 2) (coin 3) (coin 4)
               (coin 5))))
(let ((pp-draws (trim-and-thin (repeat beta-the-hard-way 8000) 2500 5)))
  (begin (printf "Draws from beta(6,2) posterior, we hope.\n Mean:\n")
         (compare-results 0.75 pp-draws mean)
         (printf "Variance:\n")
         (compare-results (/ 1.0 48.0) pp-draws variance)))
