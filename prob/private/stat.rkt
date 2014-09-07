#lang racket/base
(require racket/class
         racket/vector
         (rename-in racket/match [match-define defmatch])
         "interfaces.rkt")
(provide (struct-out statistics)
         sampler->statistics)

;; Reference: "Numerically Stable, Single-Pass, Parallel Statistics
;; Algorithms" by Bennett et al.

;; A Statistics is (statistics Nat Nat (Vectorof Real) (Vectorof (Vectorof Real)))
(struct statistics (dim n mean cov) #:transparent)

;; merge-statistics : Statistics Statistics -> Statistics
(define (merge-statistics st1 st2)
  (defmatch (statistics dim1 n1 mean1 cov1) st1)
  (defmatch (statistics dim2 n2 mean2 cov2) st2)
  (unless (= dim1 dim2)
    (error 'merge-statistics "statistics have different dimensions"))
  (define mean (vector-copy mean1))
  (define C21 (cov->C2/copy cov1 n1))
  (define C22 (cov->C2/copy cov2 n2))
  (merge-incstats dim1 n1 mean C21 n2 mean2 C22)
  (define n (+ n1 n2))
  (C2->cov! C21 n)
  (statistics dim1 n mean C21))

;; sampler->statistics : (Sampler A) Nat [(A -> Vector)] -> Statistics
(define (sampler->statistics s n [f values])
  (sampler->statistics* (lambda () (f (send s sample))) n))

(define (sampler->statistics* s n)
  (define v (s))
  (check-real-vector 'sampler->statistics v #f)
  (define dim (vector-length v))
  (define mean (make-vector dim 0))
  (define C2 (make-square-matrix dim 0))

  ;; After step k:
  ;;   mean[i] = 1/k * Σ{a=1..k} s_a[i]
  ;;   C2[i,j] = Σ{a=1..k} (s_a[i] - mean[i])*(s_a[j] - mean[j])
  (for ([k (in-range n)])
    (define v (s))
    (check-real-vector 'sampler->statistics v dim)
    (update-incstats1 dim k mean C2 v))

  (C2->cov! C2 n)
  (statistics dim n mean C2))

;; samples->statistics : (Vectorof (Vectorof Real)) -> Statistics
(define (samples->statistics vs)
  (define n (vector-length vs))
  (when (zero? n)
    (error 'samples->statistics "empty sample set"))
  (define dim (vector-length (vector-ref vs 0)))
  (for ([v (in-vector vs)])
    (check-real-vector 'samples->statistics v dim))

  (define mean (make-vector dim 0))
  (for ([v (in-vector vs)])
    (for ([vi (in-vector v)] [i (in-range dim)])
      (vector-set! mean (+ (vector-ref mean i) vi))))
  (for ([i (in-range dim)])
    (vector-set! mean i (/ (vector-ref mean i) n)))

  (define C2 (make-square-matrix dim 0))
  (for ([v (in-vector vs)])
    (for ([i (in-range dim)]
          [vi (in-vector v)]
          [C2i (in-vector C2)]
          [meani (in-vector mean)])
      (for ([j (in-range dim)]
            [vj (in-vector v)]
            [C2ij (in-vector C2i)]
            [meanj (in-vector mean)])
        (vector-set! C2i j (+ C2ij (* (- vi meani) (- vj meanj)))))))

  (C2->cov! C2 n)
  (statistics dim n mean C2))

;; ----------------------------------------

;; Incremental statistics (incstats) are represented by three values:
;; { n    : Nat                         -- population
;;   mean : (Vectorof Real)             -- mean so far
;;   C2   : (Vectorof (Vectorof Real))  -- inc intermediate for covariance }

(define (update-incstats1 dim k mean C2 v)
  ;; k is number of previous samples
  ;; Note: don't reorder; must update C2 before mean is overwritten!
  ;; FIXME: could only update triangle, since symmetric
  (for ([ei (in-vector v)] [meani (in-vector mean)] [i (in-naturals)])
    (define C2i (vector-ref C2 i))
    (for ([ej (in-vector v)] [meanj (in-vector mean)] [j (in-naturals)])
      ;; C2'[i,j] = C2[i,j] + k/(k+1) * (v[i] - mean[i])*(v[j] - mean[j])
      (vector-set! C2i j
                   (+ (vector-ref C2i j)
                      (* (/ k (add1 k))
                         (- ei meani)
                         (- ej meanj))))))
  (for ([ei (in-vector v)] [meani (in-vector mean)] [i (in-naturals)])
    ;; μ' = μ + (v - μ)/(k+1)
    (vector-set! mean i (+ meani (/ (- ei meani) (add1 k))))))

(define (merge-incstats dim n1 mean1 C21 n2 mean2 C22)
  ;; updates mean1, C21
  ;; Note: don't reorder; must update C2 before mean is overwritten!
  ;; FIXME: could only update triangle, since symmetric
  (for ([i (in-range dim)]
        [mean1i (in-vector mean1)]
        [mean2i (in-vector mean2)]
        [C21i (in-vector C21)]
        [C22i (in-vector C22)])
    (for ([j (in-range dim)]
          [mean1j (in-vector mean1)]
          [mean2j (in-vector mean2)]
          [C21ij (in-vector C21i)]
          [C22ij (in-vector C22i)])
      (define deltai (- mean2i mean1i))
      (define deltaj (- mean2j mean2j))
      (vector-set! C21i j (+ C21ij C22ij (* n1 n2 (/ (+ n1 n2)) deltai deltaj)))))
  (for ([i (in-range dim)]
        [mean1i (in-vector mean1)]
        [mean2i (in-vector mean2)])
    (define delta (- mean2i mean1i))
    (vector-set! mean1 i (+ mean1i mean2i (* n1 n2 delta delta (/ (+ n1 n1)))))))

(define (C2->cov! C2 n)
  ;; convert C2 to cov
  ;; produce sample variance (FIXME: reconsider?)
  (when (> n 1) ;; else C2 = 0, cov = 0
    (for ([C2i (in-vector C2)] [i (in-naturals)])
      (for ([C2ij (in-vector C2i)] [j (in-naturals)])
        (vector-set! C2i j (/ C2ij (sub1 n)))))))

(define (cov->C2/copy cov n)
  (define dim (vector-length cov))
  (define C2 (make-square-matrix dim 0))
  (for ([i (in-range dim)] [covi (in-vector cov)] [C2i (in-vector C2)])
    (for ([j (in-range dim)] [covij (in-vector covi)])
      (vector-set! C2i j (* covij (sub1 n)))))
  C2)

(define (check-real-vector who v dim)
  (unless (and (vector? v) (for/and ([e (in-vector v)]) (real? e)))
    (error who "value is not a vector of reals\n  value: ~e" v))
  (when dim
    (unless (= (vector-length v) dim)
      (error who "vector has wrong number of elements\n  expected: ~e\n  value: ~e"
             dim v))))

(define (make-square-matrix len [fill 0])
  (define m (make-vector len))
  (for ([i (in-range len)])
    (vector-set! m i (make-vector len fill))))


;; ============================================================
;; Old versions

;; Retained for debugging

(provide sampler->mean+variance
         sampler->means+covariance)

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
