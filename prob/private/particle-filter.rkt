;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         racket/math
         racket/vector
         (only-in "prob-util.rkt" [resample u:resample])
         "prob-syntax.rkt")
(provide (all-defined-out))

(define (make-particles count [initial-state #f])
  (new particles%
       (states (make-vector count initial-state))
       (weights (make-vector count 1))))

(define (particles? ps)
  (is-a? ps particles<%>))

(define (particles-count ps)
  (send ps get-count))

(define (particles-update ps f [iters 1])
  (send ps update f iters #t))

(define (particles-score ps f [iters 1])
  (send ps update f iters #f))

(define (particles-resample ps [count (particles-count ps)]
                            #:alg [alg #f])
  (send ps resample count alg))

(define (particles-effective-count ps)
  (send ps effective-sample-size))
(define (particles-effective-ratio ps)
  (/ (particles-effective-count ps)
     (particles-count ps)))

(define (particles-states ps)
  (vector-map car (particles-weighted-states ps)))

;; particles-weighted-states : Particles -> (Vectorof (cons State Real))
(define (particles-weighted-states ps)
  (send ps get-weighted-states))

;; in-particles : Particles -> (Sequenceof (Values State Real))
;; FIXME: add in-particles sequence syntax?
(define (in-particles ps)
  (define wstates (send ps get-weighted-states))
  (make-do-sequence
   (lambda ()
     (values
      ;; pos -> element
      (lambda (i) (let ([wst (vector-ref wstates i)]) (values (car wst) (cdr wst))))
      ;; next-pos
      add1
      ;; initial pos
      0
      ;; continue-with/after-pos/val
      (lambda (i) (< i (vector-length wstates)))
      #f #f))))

;; ----

(define particles<%>
  (interface ()
    update ;; (State -> State) Nat Boolean -> ParticleSet
    resample ;; ... -> ParticleSet
    get-count ;; -> Nat
    get-weighted-states ;; -> (vectorof (cons State Real))
    effective-sample-size ;; -> Real
    ))

(define particles%
  (class* object% (particles<%>)
    ;; states : (Vectorof State)
    ;; weights : (Vectorof Real)
    (init-field states weights)
    (unless (= (vector-length states) (vector-length weights))
      (error 'particles% "states and weights have different lengths"))
    (define ctx (new importance-stochastic-ctx%))
    (super-new)

    ;; update : (State -> State) Nat Boolean -> ParticleSet
    ;; If update? is #f, don't store result as new state (but do update weight).
    (define/public (update f iters update?)
      (cond [(zero? iters)
             this]
            [else
             (define count (vector-length states))
             (define states* (vector-copy states))
             (define weights* (make-vector count 1))
             (update1 f count states weights states* weights* update?)
             (for ([i (in-range (sub1 iters))])
               (update1 f count states* weights* states* weights* update?))
             (new particles% (states states*) (weights weights*))]))

    (define/private (update1 f count states weights states* weights* update?)
      (for ([i (in-range count)]
            [si (in-vector states)]
            [wi (in-vector weights)])
        (cond [(positive? wi)
               (set-field! weight ctx 1)
               (match (send ctx run (lambda () (f si)))
                 [(cons 'okay si*)
                  (when update?
                    (vector-set! states* i si*))
                  (vector-set! weights* i (* wi (get-field weight ctx)))]
                 [(cons 'fail _)
                  (vector-set! weights* i 0)])]
              [else
               (vector-set! weights* i 0)])))

    (define/public (get-count)
      (vector-length states))

    (define/public (get-weighted-states)
      (for/vector ([si (in-vector states)]
                   [wi (in-vector weights)]
                   #:when (positive? wi))
        (cons si wi)))

    (define/public (effective-sample-size)
      ;; Neff = 1 / (SUM (sqr normalized-weight))
      ;;      = (sqr wsum) / (SUM (sqr unnormalized-weights))
      (/ (sqr (for/sum ([w (in-vector weights)]) w))
         (for/sum ([w (in-vector weights)]) (sqr w))))

    (define/public (resample count* alg)
      (define states* (u:resample states weights count* #:alg alg))
      (define weights* (make-vector count* 1))
      (new particles% (states states*) (weights weights*)))
    ))

;; ============================================================

(require racket/vector
         "place-util.rkt"
         "serializable-lambda.rkt")
(provide make-parallel-particles)

;; Parallel Particle Sets are implemented by a list of
;; HistoricalWorkers where State = ParticleSet. Use will-executor to
;; forget previous times on GC.

(define pp-executor (make-will-executor))
(define pp-executor-thread
  (thread
   (lambda ()
     (let loop ()
       (will-execute pp-executor)
       (loop)))))

(define (make-parallel-particles n [initial-state #f])
  (define managers (get-managers))
  (define num-workers (length managers))
  ;; Round up, so may not get exactly n particles.
  (define particles/worker (ceiling (/ n num-workers)))
  ;; Create workers.
  (define workers
    (for/list ([manager (in-list managers)])
      (new historical-worker% [manager manager])))
  ;; Initialize each worker with its own ParticleSet.
  (define nows
    (for/list ([worker (in-list workers)])
      (send worker update (send worker get-now)
            (lambda/s (_) (make-particles particles/worker initial-state)))))
  (new parallel-particles% [workers workers] [nows nows]))

(define parallel-particles%
  (class* object% (particles<%>)
    ;; nows should always be list of |workers| copies of same number
    (init-field workers nows)
    (super-new)

    (will-register pp-executor this (lambda (self) (send self finalize)))

    ;; update : (State -> State) Nat Boolean -> ParticleSet
    ;; If update? is #f, don't store result as new state (but do update weight).
    (define/public (update f iters update?)
      (define nows*
        (for/list ([w (in-list workers)]
                   [now (in-list nows)])
          (send w update now (lambda/s (ps) (send ps update f iters update?)))))
      (new parallel-particles% [workers workers] [nows nows*]))

    (define/public (get-count)
      (for/sum ([w (in-list workers)]
                [now (in-list nows)])
        (send w view now (lambda/s (ps) (send ps get-count)))))

    (define/public (get-weighted-states)
      (define wstatess
        (for/list ([w (in-list workers)]
                   [now (in-list nows)])
          (send w view now (lambda/s (ps) (send ps get-weighted-states)))))
      (apply vector-append wstatess))

    (define/public (effective-sample-size)
      ;; FIXME: is this actually additive?
      (for/sum ([w (in-list workers)])
        (send w view (lambda/s (ps) (send ps effective-sample-size)))))

    (define/public (resample count alg)
      (define count/worker (ceiling (/ count (length workers))))
      (define nows*
        (for/list ([w (in-list workers)]
                   [now (in-list nows)])
          (send w update now (lambda/s (ps) (send ps resample count/worker alg)))))
      (new parallel-particles% [workers workers] [nows nows*]))

    (define/public (finalize)
      ;; Tell worker places to forget state.
      (for ([w (in-list workers)]
            [now (in-list nows)])
        (send w forget now)))
    ))
