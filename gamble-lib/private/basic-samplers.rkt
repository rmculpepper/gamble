;; Copyright 2014-2020 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/match
         racket/class
         "dist/base.rkt"
         "util/debug.rkt"
         "interfaces.rkt")
(provide (all-defined-out))

;; ============================================================
;; Rejection sampling

(define-syntax (rejection-sampler stx)
  (syntax-parse stx
    [(rejection-query def:expr ... result:expr)
     #'(rejection-sampler* (lambda () def ... result))]))

(define (rejection-sampler* thunk)
  (new rejection-sampler% (thunk thunk)))

(define rejection-sampler%
  (class sampler-base%
    (init-field thunk)
    (field [successes 0]
           [rejections 0])
    (super-new)

    (define/override (info)
      (printf "== Rejection sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections))

    (define/override (sample)
      (define ctx (new rejection-stochastic-ctx%))
      (define v (send ctx run thunk))
      (case (car v)
        [(okay)
         (set! successes (add1 successes))
         (cdr v)]
        [(fail)
         (set! rejections (add1 rejections))
         (sample)]))
    ))

(define rejection-stochastic-ctx%
  (class plain-stochastic-ctx/run%
    (inherit fail run)
    (super-new)

    (define/private (not-supported who [more ""])
      (error who "not supported by rejection sampler~a" more))

    (define/override (lscore ll)
      (not-supported 'lscore))
    (define/override (nscore l)
      (not-supported 'nscore))

    (define/override (observe dist val scale)
      (define-values (d ddim) (dist-density dist val #t))
      (cond [(zero? ddim)
             ;; FIXME: assumes total weight is 1!
             (unless (< (log (random)) d)
               (fail 'observation))]
            [else (not-supported 'observe (format "\n  distribution: ~e" dist))]))

    (define/override (trycatch p1 p2)
      (match (run p1)
        [(cons 'okay value)
         value]
        [(cons 'fail _)
         (p2)]))
    ))

;; ============================================================
;; Importance sampling

(define-syntax (importance-sampler stx)
  (syntax-parse stx
    [(importance-sampler def:expr ... result:expr)
     #'(importance-sampler*
        (lambda () def ... result))]))

(define (importance-sampler* thunk)
  (new importance-sampler% (thunk thunk)))

(define importance-sampler%
  (class* object% (weighted-sampler<%>)
    (init-field thunk)
    (field [successes 0]
           [rejections 0]
           [ddim-rejections 0]
           [min-ddim +inf.0]
           [bad-samples 0])
    (super-new)

    (define/public (info)
      (printf "== Importance sampler\n")
      (printf "Samples produced: ~s\n" successes)
      (printf "Rejections: ~s\n" rejections)
      (printf "Density dimension: ~s\n" min-ddim)
      (unless (zero? ddim-rejections)
        (printf "Density dimension rejections: ~s\n" ddim-rejections))
      (unless (zero? bad-samples)
        (printf "Bad samples emitted (wrong density dimension): ~s" bad-samples)))

    (define/public (sample/weight)
      (define ctx (new importance-stochastic-ctx%))
      (define v (send ctx run thunk))
      (case (car v)
        [(okay)
         (define ddim (get-field obs-ddim ctx))
         (when (< ddim min-ddim)
           (unless (zero? successes)
             (eprintf "WARNING: previous ~s samples are meaningless; wrong density dimension\n"
                      successes))
           (vprintf "Lower density dimension seen: ~s\n" ddim)
           (set! bad-samples successes)
           (set! min-ddim ddim))
         (cond [(<= ddim min-ddim)
                (set! successes (add1 successes))
                (cons (cdr v) (get-field weight ctx))]
               [else
                (set! ddim-rejections (add1 ddim-rejections))
                (set! rejections (add1 rejections))
                (sample/weight)])]
        [(fail)
         (set! rejections (add1 rejections))
         (sample/weight)]))
    ))

(define importance-stochastic-ctx%
  (class rejection-stochastic-ctx%
    (field [weight 1]
           [obs-ddim 0])
    (inherit fail run)
    (super-new)

    (define/override (lscore ll)
      (nscore (exp ll) 'lscore))
    (define/override (nscore l [who 'nscore])
      (set! weight (* weight l))
      (unless (positive? l) (fail who)))

    (define/override (observe dist val scale)
      (define-values (d ddim) (dist-density dist val))
      (unless (zero? ddim) (set! obs-ddim (add1 obs-ddim)))
      (nscore d 'observe))

    (define/override (trycatch p1 p2)
      (define saved-weight weight)
      (define saved-obs-ddim obs-ddim)
      (match (run p1)
        [(cons 'okay value)
         value]
        [(cons 'fail _)
         (set! weight saved-weight)
         (set! obs-ddim saved-obs-ddim)
         (p2)]))
    ))
