;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract/base
         racket/class
         data/order
         racket/dict
         "interfaces.rkt"
         "context.rkt"
         "../dist.rkt"
         "util.rkt")
(provide (contract-out
          [mem (-> procedure? procedure?)]
          [sample (-> dist? any)]
          [observe-at (-> dist? any/c any)]
          [fail (->* [] [any/c] any)]))

;; mem and sample wrappers

(define (mem f) (send (current-stochastic-ctx) mem f))
(define (observe-at dist val) (send (current-stochastic-ctx) observe-at dist val))
(define (fail [reason #f]) (send (current-stochastic-ctx) fail reason))
(define (sample* dist) (send (current-stochastic-ctx) sample dist))

(define (sample dist)
  (define obs (observing?))
  (cond [(observation? obs)
         (with-continuation-mark
             obs-mark 'ok ;; overwrite this frame's 'unknown mark
           (let ()
             (define cc (get-observe-context))
             (define value (observe-context-adjust-value cc obs))
             (when (verbose?)
               (eprintf "** CC = ~e\n" cc)
               (eprintf "** sample -> condition: ~e @ ~e\n" dist value))
             (observe-at dist value)
             value))]
        [else (sample* dist)]))

;; == Finite distributions ==

(provide
 (contract-out
  [flip
   (->* [] [probability?] 
        any)]
  [bernoulli
   (->* [] [probability?] 
        any)]
  [categorical
   (-> (vectorof (>=/c 0)) 
       any)]
  [discrete
   (-> (or/c exact-positive-integer?
             (listof (cons/c any/c (>=/c 0))))
       any)]
  [discrete*
   (->* [list?] [(listof (>=/c 0))]
       any)]))

;; flip : Prob -> (U #t #f)
(define (flip [prob 1/2])
  (positive?
   (sample (bernoulli-dist prob))))

;; bernoulli : Prob -> (U 1 0)
(define (bernoulli [prob 1/2])
  (sample (bernoulli-dist prob)))

;; categorical : (Vectorof Prob) -> Nat
(define (categorical weights)
  (sample (categorical-dist weights)))

;; discrete : Nat -> Nat
(define (discrete n/dist)
  (discrete/weights 'discrete (map car n/dist) (map cdr n/dist)))

;; discrete* : (Listof A) (Listof Prob) -> A
(define (discrete* vals [weights #f])
  (cond [(eq? weights #f)
         (unless (pair? vals)
           (error 'discrete* "empty values list"))
         (list-ref vals (discrete-uniform (length vals)))]
        [else
         (unless (= (length vals) (length weights))
           (error 'discrete*
                  "values list and weights list have unequal lengths\n  values: ~e\n  weights: ~e"
                  vals weights))
         (discrete/weights 'discrete* vals weights)]))

;; discrete-uniform : Nat -> Nat
(define (discrete-uniform n)
  (sample (categorical-dist (make-vector n (/ n)))))

;; discrete/weights : Symbol (Listof A) (Listof Prob) -> A
(define (discrete/weights who vals probs)
  (unless (positive? (apply + probs))
    (error who "weights list sum is not positive\n  weights: ~e" probs))
  (list-ref vals (inexact->exact
                  (sample (categorical-dist (list->vector probs))))))

;; == Countable distributions ==

(provide
 (contract-out
  [geometric
   (->* [] [probability?] any)]
  [poisson
   (-> (>/c 0) any)]
  [binomial
   (-> exact-nonnegative-integer? probability? any)]))

;; binomial : Nat Prob -> Integer
;; FIXME: discretizable
(define (binomial n p)
  (sample (binomial-dist n p)))

;; geometric : Prob -> Integer
;; FIXME: discretizable
(define (geometric [p 1/2])
  (sample (geometric-dist p)))

;; poisson : Real -> Integer
(define (poisson mean)
  (sample (poisson-dist mean)))

;; == Continuous distributions ==

(provide
 (contract-out
  [beta
   (-> (>/c 0) (>/c 0) any)]
  [cauchy
   (->* [] [real? (>/c 0)] any)]
  [exponential
   (->* [] [(>/c 0)] any)]
  [gamma
   (->* [] [(>/c 0) (>/c 0)] any)]
  [logistic
   (->* [] [real? (>/c 0)] any)]
  [normal
   (->* [] [real? (>/c 0)] any)]
  [pareto
   (-> (>/c 0) (>/c 0) any)]
  [uniform
   (->* [] [real? real?] any)]
  ))

;; beta : PositiveReal PositiveReal -> Real in [0,1]
(define (beta a b)
  (sample (beta-dist a b)))

(define (cauchy [mode 0] [scale 1])
  (sample (cauchy-dist mode scale)))

;; exponential : PositiveReal -> PositiveReal
;; NOTE: mean aka scale = 1/rate
(define (exponential [mean 1])
  (sample (exponential-dist mean)))

;; gamma : PositiveReal PositiveReal -> Real
;; NOTE: scale = 1/rate
(define (gamma [shape 1] [scale 1])
  (sample (gamma-dist shape scale)))

;; logistic : Real Real -> Real
(define (logistic [mean 0] [scale 1])
  (sample (logistic-dist mean scale)))

;; normal : Real PositiveReal -> Real
;; NOTE: stddev = (sqrt variance)
(define (normal [mean 0] [stddev 1])
  (sample (normal-dist mean stddev)))

;; pareto : Real Real -> Real
(define (pareto shape scale)
  (sample (pareto-dist shape scale)))

;; uniform : Real Real -> Real
(define uniform
  (case-lambda
    [() (uniform 0 1)]
    [(max) (uniform 0 max)]
    [(min max)
     (sample (uniform-dist min max))]))

;; ========================================

(provide sampler->discrete-dist
         sampler->mean+variance
         sampler->means+covariance
         indicator/value
         indicator/predicate)

(define (sampler->discrete-dist s n [f values] #:normalize? [normalize? #t])
  (define h (make-hash))
  (cond [(is-a? s sampler<%>)
         (for ([i (in-range n)])
           (let ([a (f (send s sample))])
             (hash-set! h a (add1 (hash-ref h a 0)))))]
        [(is-a? s weighted-sampler<%>)
         (for ([i (in-range n)])
           (let* ([a+p (send s sample/weight)]
                  [a (f (car a+p))]
                  [p (cdr a+p)])
             (hash-set! h a (+ p (hash-ref h a 0)))))]
        [else (raise-argument-error 'sampler->discrete-dist "sampler" s)])
  (table->discrete-dist h normalize?))

(define (table->discrete-dist h normalize?)
  (define total-w
    (for/sum ([(a w) (in-hash h)]) w))
  (define entries
    (for/list ([(a w) (in-hash h)])
      (cons a (exact->inexact (/ w total-w)))))
  (make-discrete-dist entries #:normalize? normalize?))

(define (sampler->mean+variance s n [f values])
  (define-values (sum-w sum-f sum-f^2)
    (cond [(is-a? s weighted-sampler<%>)
           (for/fold ([sum-w 0.0] [sum-f 0.0] [sum-f^2 0.0]) ([i (in-range n)])
             (let* ([v+w (send s sample/weight)]
                    [v (f (car v+w))]
                    [w (cdr v+w)])
               (values (+ sum-w w) (+ sum-f (* w v)) (+ sum-f^2 (* w v v)))))]
          [else (raise-argument-error 'sampler->mean+variance "sampler" s)]))
  (define Ef (/ sum-f sum-w))
  (values Ef
          (- (/ sum-f^2 sum-w) (* Ef Ef))))

(define (sampler->means+covariance s n [f values])
  ;; Cov[x,y] = E[(x-E[x])(y-E[y])^T] = E[x*y^T] - E[x]*E[y]^T
  (define sum-fs #f)
  (define cov #f)
  (define (check-real-vector fv)
    (unless (and (vector? fv) (for/and ([e (in-vector fv)]) (real? e)))
      (error 'sampler->means+covariance
             "value is not a vector of reals\n  value: ~e" fv)))
  (define (handle-sample fv)
    (for ([ei (in-vector fv)] [i (in-naturals)])
      (vector-set! sum-fs i (+ (vector-ref sum-fs i) ei))
      (define cov_i (vector-ref cov i))
      (for ([ej (in-vector fv)] [j (in-naturals)])
        (vector-set! cov_i j (+ (vector-ref cov_i j) (* ei ej))))))
  (let ([v (send s sample)])
    (define fv (f v))
    (check-real-vector fv)
    (set! sum-fs (make-vector (vector-length fv)))
    (set! cov (make-vector (vector-length fv) #f))
    (for ([i (in-range (vector-length fv))])
      (vector-set! cov i (make-vector (vector-length fv) 0)))
    (handle-sample fv))
  (for ([i (in-range (sub1 n))])
    (define v (send s sample))
    (define fv (f v))
    (check-real-vector fv)
    (unless (= (vector-length fv) (vector-length sum-fs))
      (error 'sampler->means+covariance
             "vector has wrong number of elements\n  expected: ~e\n  value: ~e"
             (vector-length sum-fs) fv))
    (handle-sample fv))
  (for ([i (in-range (vector-length sum-fs))])
    (vector-set! sum-fs i (/ (vector-ref sum-fs i) n)))
  (for ([meani (in-vector sum-fs)] [i (in-naturals)])
    (define cov_i (vector-ref cov i))
    (for ([meanj (in-vector sum-fs)] [j (in-naturals)])
      (vector-set! cov_i j (- (/ (vector-ref cov_i j) n) (* meani meanj)))))
  (values sum-fs cov))

;; ============================================================
;; Statistics

;; Reference: "Numerically Stable, Single-Pass, Parallel Statistics
;; Algorithms" by Bennett et al.

(provide (struct-out statistics)
         sampler->statistics)

;; A Statistics is (statistics Nat (Vectorof Real) (Vectorof (Vectorof Real)))
(struct statistics (n mean covariance) #:transparent)

;; sampler->statistics : (Sampler A) Nat [(A -> Vector)] -> Statistics
(define (sampler->statistics s n [f values])
  (sampler->statistics* (lambda () (f (send s sample))) n))

(define (sampler->statistics* s n)
  (define v (s))
  (check-real-vector 'sampler->statistics v)
  (define L (vector-length v))
  (do-stats s n L))

(define (do-stats s n L)
  ;; After step k:
  ;; M2[i] = Σ{a=1..k} (s_a[i] - mean[i])^2
  ;; mean[i] = 1/k * Σ{a=1..k} s_a[i]
  ;; cov[i,j] = Σ{a=1..k} (s_a[i] - mean[i])*(s_a[j] - mean[j])
  ;; Note: M2 is redundant w/ diagonal of cov
  (define M2 (make-vector L 0))
  (define mean (make-vector L 0))
  (define cov (make-vector L #f))
  (for ([i (in-range L)])
    (vector-set! cov i (make-vector L 0)))

  (define (handle-sample k v)
    ;; Note: don't reorder; update cov before mean is overwritten!
    ;; FIXME: only need to update cov[i,j] where i != j, because cov[i,i] = M2[i]
    ;; FIXME: could only update triangle, since symmetric
    (for ([ei (in-vector v)] [meani (in-vector mean)] [i (in-naturals)])
      (define covi (vector-ref cov i))
      (for ([ej (in-vector v)] [meanj (in-vector mean)] [j (in-naturals)])
        ;; C2'[i,j] = C2[i,j] + k/(k+1) * (v[i] - mean[i])*(v[j] - mean[j])
        (vector-set! covi j
                     (+ (vector-ref covi j)
                        (* (/ k (add1 k))
                           (- ei meani)
                           (- ej meanj))))))
    (for ([ei (in-vector v)] [meani (in-vector mean)] [i (in-naturals)])
      ;; μ' = μ + (v - μ)/(k+1)
      (vector-set! mean i (+ meani (/ (- ei meani) (add1 k))))
      ;; M2 = M2 + (v - μ)*(v - μ')
      (vector-set! M2 i (+ (vector-ref M2 i)
                           (* (- ei meani) (- ei (vector-ref mean i))))))
    (set! k (add1 k)))

  (for ([k (in-range n)])
    (define v (s))
    (check-real-vector 'sampler->statistics v)
    (unless (= (vector-length v) L)
      (error 'sampler->statistics
             "vector has wrong number of elements\n  expected: ~e\n  value: ~e"
             L v))
    (handle-sample k v))

  ;; likewise for covariance, cov
  ;; produce sample variance (FIXME: reconsider?)
  (for ([covi (in-vector cov)] [i (in-naturals)])
    (for ([covij (in-vector covi)] [j (in-naturals)])
      (vector-set! covi j (/ covij (sub1 n)))))

  (statistics n mean cov))

(define (check-real-vector who fv)
    (unless (and (vector? fv) (for/and ([e (in-vector fv)]) (real? e)))
      (error who "value is not a vector of reals\n  value: ~e" fv)))

;; ============================================================

(define ((indicator/value v) x)
  (if (equal? x v) 1 0))

(define ((indicator/predicate p?) x)
  (if (p? x) 1 0))

(provide
 (contract-out
  [discrete-dist-error
   (-> discrete-dist? discrete-dist?
       (>=/c 0))]
  [sampler->KS
   (-> (-> real?) exact-positive-integer? dist?
       real?)]))

;; discrete-dist-error : (Discrete-Dist A) (Discrete-Dist A) -> Real
(define (discrete-dist-error a b)
  ;; Why 1/2? Because every error is counted twice: 
  ;; once for being present where it shouldn't be, 
  ;; and again for being absent from where it should be.
  (* 1/2
     ;; a U b = a U (b - a)   --- membership means positive pdf
     (+ (for/sum ([aval (in-vector (discrete-dist-values a))])
          (define aweight (dist-pdf a aval))
          (define bweight (dist-pdf b aval))
          (if (positive? aweight)
              (abs (- aweight bweight))
              0))
        (for/sum ([bval (in-vector (discrete-dist-values b))]
                  #:when (zero? (dist-pdf a bval)))
          (define bweight (dist-pdf b bval))
          (abs bweight)))))

;; sampler->KS : Sampler Nat Dist -> Real
;; Compute Kolmogorov–Smirnov statistic for samples and dist.
(define (sampler->KS sampler iters dist)
  (define v (list->vector (sort (repeat sampler iters) <)))
  (KS/samples v (lambda (x) (dist-cdf dist x))))

;; KS/samples : (Vectorof Real) (Real -> Real) Boolean -> Real
;; pre: v is sorted ascending, cdf is monotone nondecreasing w/ onto (0,1)
;; Note: need continuity to calculate sup_{x in (xa,xb)} cdf(x) = cdf(xb)
(define (KS/samples v cdf)
  (define continuous? #f)
  (define n (vector-length v))
  (define (cdf_i i) (cdf (vector-ref v i)))
  (define (ecdf_i i) (/ (add1 i) n)) ;; note, (ecdf_i -1) = 0
  (max (for/fold ([acc 0]) ([i (in-range n)])
         (max acc
              (abs (- (cdf_i i) (ecdf_i i)))
              (if continuous?
                  (abs (- (cdf_i i) (ecdf_i (sub1 i))))
                  0)))
       (if continuous? 
           (abs (- (cdf_i (sub1 n)) 1))
           0)))
