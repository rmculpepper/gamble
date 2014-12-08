#lang prob
(require racket/list
         racket/vector
         (only-in math/statistics quantile)
         (except-in rackunit fail))
(provide (all-defined-out))

(define NOISY? #f)

(define INTERVAL 95)

;;;;;;;;;;;;;;;Some testing utilities ;;;;;;;;;;;;;;;;;

(define (gensamples s [n 3000])
  (generate-samples s n #:burn 3000 #:thin 5))

;; compare-results : num [a] -> void
;;
;; For cases where we know the correct values for some statistics of the posterior 
;; distribution. If we believe that the expectation of "fn" over the posterior from
;; "samples" is drawn is "expected", then the value of "fn" over most resamplings of
;; "samples" should be near "expected".  Specifically, "expected" should be in the 
;; central 95% of the values of "fn" over those (5000, but shrink if patience fails)
;; resamplings.
(define (compare-results expected samples)
  ;; (eprintf "sample size = ~s\n" (vector-length samples))
  (let* ([bounds (bootstrap-bounds samples 2000)]
         [low (exact->inexact (car bounds))]
         [high (exact->inexact (cadr bounds))])
      (cond [(<= low expected high)
             (when NOISY?
               (printf "Expected value ~a is in central ~a% interval [~a,~a]\n\n"
                       expected INTERVAL low high))]
            [else
             (when NOISY?
               (printf "Expected value ~a is NOT in central ~a% interval [~a,~a]\n\n" 
                       expected INTERVAL low high))
             (error 'compare-results
                    (string-append
                     "~a% interval does not contain expected value"
                     "\n  interval: [~e, ~e]"
                     "\n  expected value: ~e")
                    INTERVAL low high expected)])))

;; bootstrap-bounds : [a] int ~~> (num,num)
;;
;; Given list "sample" and function fn, resample n-reps times and then
;; compute quantiles for central 95% of fn over random resamplings.
(define (bootstrap-bounds sample n-reps)
  (define ms
    (for/list ([i (in-range n-reps)])
      (resample-mean sample)))
  (list (quantile (/ (- 100 INTERVAL) 200.0) < ms)
        (quantile (- 1.0 (/ INTERVAL 200.0)) < ms)))

;; resample-mean : [a] ~~> a
;;
;; Given list sample draw at most 3000 items (with replacement) from 
;; that list.
;;
;; TODO:
;; Not taking a sample of the same size as the original violates 
;; official bootstrap practice, so if I want to continue with this
;; I have to figure out the correction factor.
(define (resample-mean sample)
  (define n (vector-length sample))
  (define n* (min n 8000))
  (/ (for/sum ([i (in-range n*)])
       (vector-ref sample (random n)))
     (exact->inexact n*)))

(define (one-if x) (if x 1 0))

(define-syntax-rule (tc0 who name body ...)
  (test-case (format "~a : ~a" who name)
    (printf "Test: ~a : ~a\n" who name)
    body ...))


;;;;;;;;;;;;;;; Some test problems.  ;;;;;;;;;;;;;;;;;;;;;

(define (asia observe-x-ray? observe-smoker?)
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

  (when observe-x-ray? (unless x-ray-abnormal (fail)))
  (when observe-smoker? (unless smoker (fail)))

  (map one-if (list dyspnea tuberculosis lung-cancer)))

;; Same as above, but no structural choices
(define (asia* observe-x-ray? observe-smoker?)
  (define visit-to-asia (flip 0.01))
  (define smoker (flip 0.5))
  (define tuberculosis (flip (if visit-to-asia 0.05 0.01)))
  (define lung-cancer (flip (if smoker 0.1 0.01)))
  (define bronchitis (flip (if smoker 0.6 0.3)))
  (define tb-or-c (or tuberculosis lung-cancer))
  (define x-ray-abnormal (flip (if tb-or-c 0.98 0.05)))
  (define dyspnea (flip (cond [(and tb-or-c bronchitis) 0.9]
                              [tb-or-c 0.7]
                              [bronchitis 0.8]
                              [else 0.1])))

  (when observe-x-ray? (unless x-ray-abnormal (fail)))
  (when observe-smoker? (unless smoker (fail)))

  (map one-if (list dyspnea tuberculosis lung-cancer)))

;; "Asia" BN--expected results from Netica
(define (run-asia-tests who asia make-sampler)

  (define-syntax-rule (tc observe-x-ray? observe-smoker? nsamples
                          [name selector expected] ...)
    (begin
      (define sampler (make-sampler (lambda () (asia observe-x-ray? observe-smoker?))))
      (define samples (gensamples sampler nsamples))
      (let ([complete-name
             (string-append name
                            (cond [(and observe-x-ray? observe-smoker?)
                                   " given abnormal x-ray and smoker"]
                                  [observe-x-ray?
                                   " given abnormal x-ray"]
                                  [observe-smoker?
                                   " given smoker"]
                                  [else ""]))])
        (tc0 who complete-name
          (define specific-samples (vector-map selector samples))
          (when NOISY? (printf "Marginal probability of ~a.\n" complete-name))
          (compare-results expected specific-samples)))
      ...))

  (tc #f #f 4000
      ["dyspnea"      first  0.436]
      ["tuberculosis" second 0.0104]
      ["lung cancer"  third  0.055])

  ;; With an abnormal x-ray
  ;; Lung-cancer posterior here is currently (as of 6/26/14) much too low.
  ;; Webchurch gets it right, so we are not reproducing their algorithm.
  (tc #t #f 6000
      ["dyspnea"      first  0.641]
      ["tuberculosis" second 0.0924]
      ["lung cancer"  third  0.489])

  ;; With an abnormal x-ray and smoking
  (tc #t #t 6000
      ["dyspnea"      first  0.732]
      ["tuberculosis" second 0.0672]
      ["lung cancer"  third  0.646])
  )

(define (run-bb-test who make-sampler)
  ;; Beta/bernoulli--we know that with prior (beta 1 2) and five "true" observations,
  ;; the posterior is (beta 6 2), which has mean 6/(6+2) = 0.75, and variance
  ;; 6*2 / [(6 + 2)^2 * (6 + 2 + 1)] = 12/8*8*9 = 3/2*8*9 = 1/2*8*3 = 1/48.
  (define beta-the-hard-way
    (make-sampler
     (lambda ()
       (define p (beta 1 2))
       (define (coin) (flip p))
       (for ([i 5]) (unless (coin) (fail)))
       p)))

  (tc0 who "beta-bernoulli"
    (define pp-draws (gensamples beta-the-hard-way 2000))
    ;; Draws from beta(6,2) posterior, we hope.
    (compare-results 0.75 pp-draws) ;; mean
    ;; (compare-results (/ 1.0 48.0) pp-draws variance))
    ))

;; ============================================================

(define (run)
  (define (run-both-tests name asia make-sampler)
    (run-asia-tests name asia make-sampler)
    (run-bb-test name make-sampler))

  (when #f
  (run-asia-tests "enumeration" asia
                  (lambda (thunk) (define d (enumerate (thunk))) (rejection-sampler (sample d))))
  (run-asia-tests "enumeration, NS" asia*
                  (lambda (thunk) (define d (enumerate (thunk))) (rejection-sampler (sample d))))
  )
  (run-both-tests "rejection" asia
                  (lambda (thunk) (rejection-sampler (thunk))))
  (run-both-tests "rejection, NS" asia*
                  (lambda (thunk) (rejection-sampler (thunk))))

  (run-both-tests "mh single-site" asia
                  (lambda (thunk) (mh-sampler (thunk))))
  (run-both-tests "mh single-site, NS" asia*
                  (lambda (thunk) (mh-sampler (thunk))))
;;  (run-asia-tests "mh multi-site" asia
;;                  (lambda (thunk) (mh-sampler (thunk) #:transition (multi-site))))
;;  (run-asia-tests "mh multi-site, NS" asia*
;;                  (lambda (thunk) (mh-sampler (thunk) #:transition (multi-site))))

  ;; Only allowed on asia*, no structural choices
  (run-asia-tests "mh enum-gibbs, asia*" asia*
                  (lambda (thunk) (mh-sampler (thunk) #:transition (enumerative-gibbs))))
  (run-both-tests "mh slice, asia*" asia*
                  (lambda (thunk) (mh-sampler (thunk) #:transition (slice))))
  )

(run)
