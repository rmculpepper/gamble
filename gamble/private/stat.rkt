#lang racket/base
(require racket/class
         racket/vector
         (rename-in racket/match [match-define defmatch])
         "interfaces.rkt"
         "../matrix.rkt"
         "util/samples.rkt"
         "dist.rkt")
(provide (struct-out statistics)
         statistics-scalar-mean
         statistics-scalar-variance
         statistics-vector-mean
         real-vector-like?

         sampler->statistics
         samples->statistics
         sampler->mean+variance
         samples->mean+variance
         sampler->mean
         samples->mean
         samples->KS)

;; Reference: "Numerically Stable, Single-Pass, Parallel Statistics
;; Algorithms" by Bennett et al.

;; ============================================================
;; Statistics objects

;; A Statistics is (statistics PosNat PosNat ColumnMatrix Matrix)
(struct statistics (dim n mean cov) #:transparent)

;; Scalar special cases
(define (statistics-scalar-mean s)
  (check-dim1 'statistics-scalar-mean s)
  (matrix11->value (statistics-mean s)))
(define (statistics-scalar-variance s)
  (check-dim1 'statistics-scalar-variance s)
  (matrix11->value (statistics-cov s)))

;; Vector special cases
(define (statistics-vector-mean s)
  (matrix->vector (statistics-mean s)))

(define (check-dim1 who s)
  (unless (= 1 (statistics-dim s))
    (error who "expected scalar statistics (dimension 1)\n  given: ~e" s)))

;; merge-statistics : Statistics Statistics -> Statistics
(define (merge-statistics st1 st2)
  (defmatch (statistics dim1 n1 mean1 cov1) st1)
  (defmatch (statistics dim2 n2 mean2 cov2) st2)
  (unless (= dim1 dim2)
    (error 'merge-statistics "statistics have different dimensions"))
  (define mean1v (vector-copy (matrix->vector mean1)))
  (define mean2v (matrix->vector mean2))
  (define C21 (cov->C2 cov1 n1 #t))
  (define C22 (cov->C2 cov2 n2 #f))
  (merge-incstats! dim1 n1 mean1v C21 n2 mean2v C22)
  (define n (+ n1 n2))
  (statistics dim1 n (array->immutable-array (->col-matrix mean1v)) (C2->cov C21 n)))

;; ============================================================
;; RealVectors

;; An RV is one of
;; - real
;; - (vectorof real)
;; - col-matrix

(define (real-vector-like? x)
  (or (real? x)
      (and (vector? x) (for/and ([xi (in-vector x)]) (real? xi)))
      (col-matrix? x)))

(define (rv-length x)
  (cond [(real? x) 1]
        [(vector? x) (vector-length x)]
        [(matrix? x) (matrix-num-rows x)]))

(define (rv-ref x i)
  (cond [(real? x) x]
        [(vector? x) (vector-ref x i)]
        [(matrix? x) (matrix-ref x i 0)]))

;; check-rv-vector : Symbol Vector -> PosNat
(define (check-rv-vector who vs)
  (when (zero? (vector-length vs))
    (error who "empty vector XXXX"))
  (define rv0 (vector-ref vs 0))
  (define dim (check-rv who rv0 #f))
  (for ([v (in-vector vs)])
    (check-rv who v dim))
  dim)

;; check-rv : Symbol Any PosNat/#f -> PosNat
(define (check-rv who v dim)
  (unless (real-vector-like? v)
    (error who "expected a real, real vector, or real column matrix\n  value: ~e" v))
  (define rvlen (rv-length v))
  (when dim
    (unless (= rvlen dim)
      (error who "real vector has wrong dimension\n  expected: ~e\n  value: ~e"
             dim v)))
  rvlen)

;; ============================================================
;; Incremental Statistics

;; Incremental statistics (incstats) are represented by three values:
;; { n    : Nat                 -- population
;;   mean : (Vectorof Real)     -- mean so far
;;   C2   : Matrix              -- inc intermediate for covariance }

;; update-incstats1 : PosNat PosNat (Vectorof Real) MutableMatrix -> Void
;; Update incstats with one new value.
(define (update-incstats1 dim k mean C2 v)
  ;; k is number of previous samples
  ;; Note: don't reorder; must update C2 before mean is overwritten!
  ;; FIXME: could only update triangle, since symmetric
  (for ([meani (in-vector mean)]
        [i (in-naturals)])
    (define ei (rv-ref v i))
    (for ([meanj (in-vector mean)] [j (in-naturals)])
      (define ej (rv-ref v j))
      ;; C2'[i,j] = C2[i,j] + k/(k+1) * (v[i] - mean[i])*(v[j] - mean[j])
      (matrix-set! C2 i j
                   (+ (matrix-ref C2 i j)
                      (* (/ k (add1 k))
                         (- ei meani)
                         (- ej meanj))))))
  (for ([meani (in-vector mean)] [i (in-naturals)])
    (define ei (rv-ref v i))
    ;; μ' = μ + (v - μ)/(k+1)
    (vector-set! mean i (+ meani (/ (- ei meani) (add1 k))))))

;; merge-incstats! : PosNat
;;                   PosNat (Vectorof Real) MutableMatrix
;;                   PosNat (Vectorof Real) Matrix
;;                -> Void
;; Merges second incstats into first, mutating first.
(define (merge-incstats! dim n1 mean1 C21 n2 mean2 cov2)
  ;; updates mean1, C21
  ;; Note: don't reorder; must update C2 before mean is overwritten!
  ;; FIXME: could only update triangle, since symmetric
  (for ([i (in-range dim)]
        [mean1i (in-vector mean1)]
        [mean2i (in-vector mean2)])
    (for ([j (in-range dim)]
          [mean1j (in-vector mean1)]
          [mean2j (in-vector mean2)])
      (define deltai (- mean2i mean1i))
      (define deltaj (- mean2j mean2j))
      (matrix-set! C21 i j
                   (+ (matrix-ref C21 i j)
                      (* (matrix-ref cov2 i j) (sub1 n2))
                      (* n1 n2 (/ (+ n1 n2)) deltai deltaj)))))
  (for ([i (in-range dim)]
        [mean1i (in-vector mean1)]
        [mean2i (in-vector mean2)])
    (define delta (- mean2i mean1i))
    (vector-set! mean1 i (+ mean1i mean2i (* n1 n2 delta delta (/ (+ n1 n1)))))))

(define (C2->cov C2 n)
  ;; convert C2 to cov
  ;; produce sample variance (FIXME: reconsider?)
  (when (> n 1) ;; else C2 = 0, cov = 0
    (array/ C2 (array (sub1 n)))))

(define (cov->C2 cov n mutable-copy?)
  (let ([C2 (array* cov (array (sub1 n)))])
    (cond [mutable-copy? (array->mutable-array C2)]
          [else C2])))

;; ============================================================
;; Statistics from Samples/Samplers

(define (sampler->mean+variance s n [f values]
                                #:burn [burn 0]
                                #:thin [thin 0])
  (samples->mean+variance*
   'sampler->mean+variance
   (generate-samples s n f #:burn burn #:thin thin)))

(define (samples->mean+variance vs)
  (samples->mean+variance* 'samples->mean+variance vs))

(define (samples->mean+variance* who vs)
  (define st (samples->statistics* who vs))
  (unless (= 1 (statistics-dim st))
    (error who "statistics has wrong dimension (non-scalar)\n  statistics: ~e" st))
  (values (statistics-scalar-mean st)
          (statistics-scalar-variance st)))

;; ----------------------------------------

;; sampler->statistics : (Sampler A) Nat [(A -> Vector)] -> Statistics
(define (sampler->statistics s n [f values]
                             #:burn [burn 0]
                             #:thin [thin 0])
  (samples->statistics* 'sampler->statistics
                        (generate-samples s n f #:burn burn #:thin thin)))

;; samples->statistics : (Vectorof RV) -> Statistics
(define (samples->statistics vs)
  (samples->statistics* 'samples->statistics vs))

;; samples->statistics* : Symbol (Vectorof (Vectorof Real)) -> Statistics
;; Batch version (non-incremental).
(define (samples->statistics* who vs)
  (define dim (check-rv-vector who vs))
  (define n (vector-length vs))

  (define mean (make-vector dim 0))
  (for ([v (in-vector vs)])
    (for ([i (in-range dim)])
      (vector-set! mean i (+ (vector-ref mean i) (rv-ref v i)))))
  (for ([i (in-range dim)])
    (vector-set! mean i (/ (vector-ref mean i) n)))

  (define C2 (make-mutable-matrix dim dim 0))
  (for ([v (in-vector vs)])
    (for ([i (in-range dim)]
          [meani (in-vector mean)])
      (for ([j (in-range dim)]
            [meanj (in-vector mean)])
        (matrix-set! C2 i j
                     (+ (matrix-ref C2 i j)
                        (* (- (rv-ref v i) meani)
                           (- (rv-ref v j) meanj)))))))

  (statistics dim n (array->immutable-array (->col-matrix mean)) (C2->cov C2 n)))

;; samples->statistics*/incremental : Symbol (Vectorof (Cons RV Real)) -> Statistics
;; Incremental version.
(define (samples->statistics*/incremental who vs)
  (define dim (check-rv-vector who vs))
  (define n (vector-length vs))
  (define mean (make-vector dim 0))
  (define C2 (make-mutable-matrix dim dim 0))

  ;; After step k:
  ;;   mean[i] = 1/k * Σ{a=1..k} s_a[i]
  ;;   C2[i,j] = Σ{a=1..k} (s_a[i] - mean[i])*(s_a[j] - mean[j])
  (for ([v (in-vector vs)]
        [k (in-range n)])
    (update-incstats1 dim k mean C2 v))

  (statistics dim n (array->immutable-array (->col-matrix mean)) (C2->cov C2 n)))


;; ============================================================
;; Shape-generic mean

(define (sampler->mean s n [f values]
                       #:burn [burn 0]
                       #:thin [thin 0])
  (samples->mean (generate-weighted-samples s n f #:burn burn #:thin thin)))

(define (samples->mean wvs)
  (define wv0 (vector-ref wvs 0))
  (define-values (sum weight)
    (for/fold ([sum (car wv0)] [weight (cdr wv0)])
              ([v+w (in-vector wvs 1)])
      (defmatch (cons v w) v+w)
      (values (generic+ sum (generic* v w)) (+ weight w))))
  (generic* sum (/ weight)))

;; generic+ : Any Any -> Any
(define (generic+ a b)
  (cond [(and (number? a) (number? b))
         (+ a b)]
        [(and (pair? a) (pair? b))
         (cons (generic+ (car a) (car b))
               (generic+ (cdr a) (cdr b)))]
        [(and (null? a) (null? b))
         '()]
        [(and (vector? a) (vector? b)
              (= (vector-length a) (vector-length b)))
         (build-vector (vector-length a)
                       (lambda (i) (generic+ (vector-ref a i) (vector-ref b i))))]
        [(and (array? a) (array? b)
              (equal? (array-shape a) (array-shape b)))
         (array+ a b)]
        [else +nan.0]))

;; generic* : Any Real -> Any
(define (generic* a b)
  (cond [(number? a)
         (* a b)]
        [(pair? a)
         (cons (generic* (car a) b) (generic* (cdr a) b))]
        [(null? a)
         '()]
        [(vector? a)
         (build-vector (vector-length a) (lambda (i) (generic* (vector-ref a i) b)))]
        [(array? a)
         (array* a (array b))]
        [else +nan.0]))


;; ============================================================
;; Kolmogorov-Smirov statistic

;; samples->KS : (Vectorof Real) Dist -> Real
(define (samples->KS v0 dist)
  (for ([x (in-vector v0)])
    (unless (real? x)
      (raise-argument-error 'samples->KS "(vectorof real?)" 0 v0 dist)))
  (define v (list->vector (sort (vector->list v0) <)))
  (KS v (lambda (x) (dist-cdf dist x))))

;; KS : (Vectorof Real) (Real -> Real) -> Real
;; pre: v is sorted ascending, cdf is monotone nondecreasing w/ onto (0,1)
;; Note: need continuity to calculate sup_{x in (xa,xb)} cdf(x) = cdf(xb)
(define (KS v cdf)
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


;; ============================================================
;; Old versions

;; Retained for debugging

(provide OLD-sampler->mean+variance
         OLD-sampler->means+covariance)

(define (OLD-sampler->mean+variance s n [f values])
  (define-values (sum-w sum-f sum-f^2)
    (cond [(is-a? s weighted-sampler<%>)
           (for/fold ([sum-w 0.0] [sum-f 0.0] [sum-f^2 0.0]) ([i (in-range n)])
             (let* ([v+w (send s sample/weight)]
                    [v (f (car v+w))]
                    [w (cdr v+w)])
               (values (+ sum-w w) (+ sum-f (* w v)) (+ sum-f^2 (* w v v)))))]
          [else (raise-argument-error 'OLD-sampler->mean+variance "sampler" s)]))
  (define Ef (/ sum-f sum-w))
  (values Ef
          (- (/ sum-f^2 sum-w) (* Ef Ef))))

(define (OLD-sampler->means+covariance s n [f values])
  ;; Cov[x,y] = E[(x-E[x])(y-E[y])^T] = E[x*y^T] - E[x]*E[y]^T
  (define sum-fs #f)
  (define cov #f)
  (define (check-real-vector fv)
    (unless (and (vector? fv) (for/and ([e (in-vector fv)]) (real? e)))
      (error 'OLD-sampler->means+covariance
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
      (error 'OLD-sampler->means+covariance
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

(define (check-real-vector who v dim)
  (unless (and (vector? v) (for/and ([e (in-vector v)]) (real? e)))
    (error who "value is not a vector of reals\n  value: ~e" v))
  (when dim
    (unless (= (vector-length v) dim)
      (error who "vector has wrong number of elements\n  expected: ~e\n  value: ~e"
             dim v))))
