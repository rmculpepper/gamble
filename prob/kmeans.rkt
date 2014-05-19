#lang racket/base
(require racket/vector)
(provide (all-defined-out))

;; References:
;; - http://en.wikipedia.org/wiki/K-means_clustering
;; - http://www.mathworks.com/help/stats/kmeans.html
;; - http://en.wikipedia.org/wiki/K-means%2B%2B

;; Point is (Vectorof Real)
;; Dist is NonnegativeReal

;; Options
;;  #:repeat reps -- only sensible when choosing initial means randomly
;;  #:on-empty action -- 'error or 'drop

;; kmeans : PosNat (Vectorof Point) ...
;;       -> (values (Vectorof Point) (Vectorof Nat) Dist)
(define (kmeans K points initial-means
                #:iterations [iterations 1]
                #:on-empty [on-empty 'error])
  (define dim (check-dimensions points))
  (define-values (means assignment wcss)
    (for/fold ([acc-means #f] [acc-assignment #f] [acc-wcss +inf.0])
        ([iteration (in-range iterations)])
      (define means0 (compute-initial-means K dim points initial-means on-empty))
      (define-values (new-means new-assignment new-iters)
        (kmeans* K dim points means0 on-empty))
      (define new-wcss (compute-wcss points new-assignment new-means))
      ;; (eprintf "new-wcss = ~s, new-iters = ~s\n" new-wcss new-iters)
      (if (< new-wcss acc-wcss)
          (values new-means new-assignment new-wcss)
          (values acc-means acc-assignment acc-wcss))))
  (values means assignment wcss))

;; kmeans : PositiveInteger (Vectorof Point) -> ???
(define (kmeans* K dim points means0 on-empty)
  (define assignment0 (compute-assignment points means0))
  ;; loop until fixed point:
  ;;   assignment: partition points by proximity to means
  ;;   update: compute new means = centroids of new clusters
  (let loop ([means means0] [assignment assignment0] [iterations 1])
    (define new-means (compute-new-means K dim points assignment on-empty))
    (define new-assignment (compute-assignment points new-means))
    (cond [(same-assignment? new-assignment assignment)
           (values new-means new-assignment iterations)]
          [else
           (loop new-means new-assignment (add1 iterations))])))

;; check-dimensions : (VectorOf Point) -> Nat
(define (check-dimensions points)
  (unless (positive? (vector-length points))
    (error 'kmeans "no points"))
  (define dim (dimension (vector-ref points 0)))
  (for ([p (in-vector points)])
    (unless (= (dimension p) dim)
      (error 'kmeans "inconsistent dimensions\n  point: ~e\n  expected dimension: ~e"
             p dim)))
  dim)

;; compute-initial-means : PosNat PosNat (Vectorof Point) ??? -> (Vectorof Point)
(define (compute-initial-means K dim points initial-means on-empty)
  (cond [(vector? initial-means)
         initial-means]
        [(eq? initial-means 'sample)
         (cond [(> (vector-length points) K)
                (sample-initial-means K (vector-copy points))]
               [(= (vector-length points) K)
                points]
               [(eq? on-empty 'drop)
                points]
               [else (error 'kmeans "cannot sample initial means: too few points")])]
        ;; FIXME: other initialization methods???
        [else (error 'kmeans "bad initial means argument\n  value: ~e" initial-means)]))

;; sample-initial-means : PosNat (Vectorof Point) -> (Vectorof Point)
;; Note: mutates points (call with copy).
(define (sample-initial-means K points)
  (for ([index (in-range K)])
    (define pick (+ index (random (- (vector-length points) index))))
    (vector-swap! points index pick))
  (vector-copy points 0 K))

;; compute-new-means : PosNat PosNat (Vectorof Point) (Vectorof Nat) -> (Vectorof Point)
(define (compute-new-means K dim points assignment on-empty)
  (define sums (for/vector #:length K ([i (in-range K)]) (make-vector dim 0)))
  (define counts (for/vector #:length K ([i (in-range K)]) 0))
  (for ([p (in-vector points)]
        [a (in-vector assignment)])
    (vector-add! (vector-ref sums a) p)
    (vector-set! counts a (add1 (vector-ref counts a))))
  ;; Convert sums to means, move empty clusters to end.
  (define last-valid ;; index of last non-empty cluster
    (let loop ([index 0] [last (sub1 K)])
      (cond [(> index last)
             last]
            [else
             (cond [(zero? (vector-ref counts index))
                    (vector-set! sums index (vector-ref sums last))
                    (vector-set! counts index (vector-ref counts last))
                    (loop index (sub1 last))]
                   [else
                    (vector-div! (vector-ref sums index) (vector-ref counts index))
                    (loop (add1 index) last)])])))
  (cond [(= last-valid (sub1 K))
         sums]
        [else
         (when (eq? on-empty 'error)
           (error 'kmeans "cluster became empty"))
         (vector-copy sums 0 (add1 last-valid))]))

;; compute-assignment : (Vectorof Point) (Vectorof Point) -> (Vectorof Nat)
;; Creates mapping of point index => nearest mean index.
(define (compute-assignment points means)
  (define mapping (make-vector (vector-length points) #f))
  (for ([i (in-range (vector-length points))])
    (vector-set! mapping i (nearest-point-index (vector-ref points i) means)))
  mapping)

;; nearest-point-index : Point (Vectorof Point) -> Nat
(define (nearest-point-index p targets)
  (define-values (nearest-index nearest-sqrdist)
    (for/fold ([acc-index #f] [acc-sqrdist +inf.0])
        ([index (in-range (vector-length targets))]
         [target (in-vector targets)])
      (let ([new-sqrdist (sqrdist p target)])
        (if (< new-sqrdist acc-sqrdist)
            (values index new-sqrdist)
            (values acc-index acc-sqrdist)))))
  nearest-index)

;; same-assignment? : (Vectorof Nat) (Vectorof Nat) -> Boolean
;; FIXME: means may shift order ???
(define (same-assignment? a1 a2)
  (equal? a1 a2))

;; compute-wcss : (Vectorof Point) (Vectorof Nat) (Vectorof Point) -> Real
(define (compute-wcss points assignment means)
  (for/sum ([p (in-vector points)]
            [a (in-vector assignment)])
    (sqrdist p (vector-ref means a))))

;; ============================================================
;; Point functions

;; dimension : Point -> Nat
(define (dimension p)
  (vector-length p))

;; dist, sqrdist : Point Point -> Dist
(define (dist a b)
  (sqrt (sqrdist a b)))
(define (sqrdist a b)
  (for/sum ([ae (in-vector a)]
            [be (in-vector b)])
    (* (- ae be) (- ae be))))

;; vector-add! : Point Point -> Void
(define (vector-add! a b)
  (for ([i (in-range (vector-length a))])
    (vector-set! a i (+ (vector-ref a i) (vector-ref b i)))))

;; vector-div! : Point Real -> Void
(define (vector-div! a s)
  (for ([i (in-range (vector-length a))])
    (vector-set! a i (/ (vector-ref a i) s))))

;; vector-swap! : Vector Nat Nat -> Void
(define (vector-swap! v a b)
  (let ([va (vector-ref v a)]
        [vb (vector-ref v b)])
    (vector-set! v a vb)
    (vector-set! v b va)))

;; ============================================================
;; Test data

;; t1 : just some numbers I made up
(define t-points1
  '#[ #(1 2) #(3 2)
      #(9 9) #(7 8)
      ])
(define t-means1
  '#[ #(1 1)
      #(9 9)
      ])

;; t2 : clustered around 4 points of square
(define t-means2
  '#[ #(1 1)
      #(9 1)
      #(1 9)
      #(9 9)
      ])
(define t-points2
  (for*/vector ([mean (in-vector t-means2)]
                [n 100])
    (for/vector ([xi (in-vector mean)])
      (+ xi (* 6 (+ (random) (random) -1))))))

;; t3 : cloud around origin
(define t-points3
  (for/vector ([n 100])
    (for/vector ([xi 2])
      (* 10 (+ (random) (random) -1)))))

;; t4 : clustered around two points
(define t-means4
  '#[ #(0 0)
      #(8 0)
      ])
(define t-points4
  (for*/vector ([mean (in-vector t-means4)]
                [n 100])
    (for/vector ([xi (in-vector mean)])
      (+ xi (* 4 (+ (random) (random) -1))))))
