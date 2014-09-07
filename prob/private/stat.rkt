#lang racket/base
(require racket/class
         "interfaces.rkt")
(provide (struct-out statistics)
         sampler->statistics)

;; Reference: "Numerically Stable, Single-Pass, Parallel Statistics
;; Algorithms" by Bennett et al.

;; A Statistics is (statistics Nat Nat (Vectorof Real) (Vectorof (Vectorof Real)))
(struct statistics (dim n mean cov) #:transparent)

;; sampler->statistics : (Sampler A) Nat [(A -> Vector)] -> Statistics
(define (sampler->statistics s n [f values])
  (sampler->statistics* (lambda () (f (send s sample))) n))

(define (sampler->statistics* s n)
  (define v (s))
  (check-real-vector 'sampler->statistics v)
  (define dim (vector-length v))
  (do-stats s n dim))

(define (do-stats s n dim)
  ;; After step k:
  ;; mean[i] = 1/k * Σ{a=1..k} s_a[i]
  ;; cov[i,j] = Σ{a=1..k} (s_a[i] - mean[i])*(s_a[j] - mean[j])
  (define mean (make-vector dim 0))
  (define cov (make-vector dim #f))
  (for ([i (in-range dim)])
    (vector-set! cov i (make-vector dim 0)))

  (define (handle-sample k v)
    ;; Note: don't reorder; update cov before mean is overwritten!
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
      (vector-set! mean i (+ meani (/ (- ei meani) (add1 k)))))
    (set! k (add1 k)))

  (for ([k (in-range n)])
    (define v (s))
    (check-real-vector 'sampler->statistics v)
    (unless (= (vector-length v) dim)
      (error 'sampler->statistics
             "vector has wrong number of elements\n  expected: ~e\n  value: ~e"
             dim v))
    (handle-sample k v))

  ;; likewise for covariance, cov
  ;; produce sample variance (FIXME: reconsider?)
  (for ([covi (in-vector cov)] [i (in-naturals)])
    (for ([covij (in-vector covi)] [j (in-naturals)])
      (vector-set! covi j (/ covij (sub1 n)))))

  (statistics dim n mean cov))

(define (check-real-vector who fv)
    (unless (and (vector? fv) (for/and ([e (in-vector fv)]) (real? e)))
      (error who "value is not a vector of reals\n  value: ~e" fv)))


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
