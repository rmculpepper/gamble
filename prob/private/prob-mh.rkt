;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         "interfaces.rkt"
         "mh/base.rkt"
         "mh/transitions.rkt"
         "mh/slice.rkt"
         "mh/enum-gibbs.rkt"
         "mh/hmc.rkt")
(provide mh-sampler*
         mh-transition?
         cycle
         sequence
         single-site
         multi-site
         hmc
         slice
         enumerative-gibbs)

(define (mh-transition? x) (is-a? x mh-transition<%>))

(define (sequence . txs)
  (new sequence-mh-transition% (transitions txs)))
(define (cycle . txs)
  (new cycle-mh-transition% (transitions txs)))
(define (single-site #:zone [zone #f] #:record-obs? [record-obs? #t])
  (new single-site-mh-transition% [zone zone] [record-obs? record-obs?]))
(define (multi-site #:zone [zone #f] #:record-obs? [record-obs? #t])
  (new multi-site-mh-transition% [zone zone] [record-obs? record-obs?]))
(define (mixture transitions [weights #f])
  (define transitions* (if (list? transitions) (list->vector transitions) transitions))
  (define weights*
    (cond [(list? weights) (list->vector weights)]
          [(vector? weights) weights]
          [else (let ([len (length transitions)]) (make-vector len (/ len)))]))
  (new mixture-mh-transition% [transitions transitions*] [weights weights*]))
(define (hmc [epsilon 0.01] [L 10] #:zone [zone #f])
  (new hmc-transition% [epsilon epsilon] [L L] [zone zone]))
(define (slice #:scale [scale-factor 1] #:zone [zone #f])
  (new single-site-slice-mh-transition% (scale-factor scale-factor) (zone zone)))
(define (enumerative-gibbs #:zone [zone #f] #:record-obs? [record-obs? #t])
  (new enumerative-gibbs-mh-transition% (zone zone) (record-obs? record-obs?)))
(define (rerun)
  the-rerun-mh-transition)

(define the-rerun-mh-transition (new rerun-mh-transition%))

;; ============================================================

(define (mh-sampler* thunk [transition (single-site)])
  (new mh-sampler% (thunk thunk) (transition transition)))

(define mh-sampler%
  (class sampler-base%
    (init-field thunk
                transition)
    (field [last-trace init-trace]
           [accepts 0]
           [cond-rejects 0]
           [mh-rejects 0]
           [transition-stack null])
    (super-new)

    (define/public (set-transition tx)
      (set! transition tx))
    (define/public (push-transition tx)
      (set! transition-stack (cons transition transition-stack))
      (set! transition tx))
    (define/public (pop-transition)
      (cond [(pair? transition-stack)
             (define tx transition)
             (set! transition (car transition-stack))
             (set! transition-stack (cdr transition-stack))
             tx]
            [else #f]))

    ;; Note: {MAP,MLE}-estimate is argmax over *all* unconditioned variables.
    ;; FIXME: figure out how to do subsets.
    (define/public (MAP-estimate iters)
      (*estimate iters trace-ll))
    (define/public (MLE-estimate iters)
      (*estimate iters trace-ll-obs))

    (define/private (*estimate iters get-trace-ll)
      (void (sample))
      (define best-trace
        (for/fold ([best-trace last-trace]) ([n (in-range iters)])
          (void (sample))
          (if (> (get-trace-ll last-trace) (get-trace-ll best-trace))
              last-trace
              best-trace)))
      (trace-value best-trace))

    (define/override (sample)
      (sample/transition transition))

    (define/public (rerun)
      (define lt last-trace)
      (sample/transition the-rerun-mh-transition)
      (not (eq? lt last-trace)))

    (define/public (sample/transition tx)
      (match (send tx run thunk last-trace)
        [(? trace? t)
         (set! last-trace t)
         (trace-value t)]
        [#f
         (if (eq? last-trace init-trace)
             (sample)
             (trace-value last-trace))]))

    (define/public (info)
      (send transition info 0)
      (when (pair? transition-stack)
        (printf "\n== Pushed transitions\n")
        (for ([tx (in-list transition-stack)])
          (send tx info 2))))
    ))
