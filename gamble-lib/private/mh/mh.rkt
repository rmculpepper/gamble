;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         "interfaces.rkt"
         "mh/base.rkt"
         "mh/db.rkt"
         "mh/proposal.rkt"
         "mh/transitions.rkt"
         "mh/slice.rkt"
         "mh/enum-gibbs.rkt")
(provide mh-sampler*
         mh-sampler%
         mh-transition?
         mixture
         rerun
         single-site
         multi-site
         slice
         enumerative-gibbs
         proposal?
         proposal:resample
         proposal:drift
         default-proposal)

(define (mh-transition? x) (is-a? x mh-transition<%>))

(define (mixture transitions [weights #f])
  (define transitions* (if (list? transitions) (list->vector transitions) transitions))
  (define weights*
    (cond [(list? weights) (list->vector weights)]
          [(vector? weights) weights]
          [else (let ([len (length transitions)]) (make-vector len (/ len)))]))
  (new mixture-mh-transition% [transitions transitions*] [weights weights*]))

(define (single-site [proposal (default-proposal)]
                     #:zone [zone #f])
  (new single-site-mh-transition%
       [proposal (->proposal proposal)]
       [zone zone]))

(define (multi-site [proposal (default-proposal)]
                    #:zone [zone #f])
  (new multi-site-mh-transition%
       [proposal (->proposal proposal)]
       [zone zone]))

(define (slice #:method [method 'double] #:w [w 1] #:m [m +inf.0] #:zone [zone #f])
  (new slice-mh-transition% (method method) (W w) (M m) (zone zone)))

(define (enumerative-gibbs #:zone [zone #f])
  (new enumerative-gibbs-mh-transition% (zone zone)))

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
           [last-txinfo #f]
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
          (if (trace>? last-trace best-trace) last-trace best-trace)))
      (trace-value best-trace))

    (define/override (sample)
      (if (eq? last-trace init-trace)
          (sample! the-rerun-mh-transition +inf.0)
          (sample! transition 0))
      (trace-value last-trace))

    ;; Updates last-sample; returns #t for new sample, #f if unchanged (tx failed).
    (define/public (sample! tx retries)
      (match (send tx run thunk last-trace)
        [(cons (? trace? t) txinfo)
         (set! last-trace t)
         (set! last-txinfo txinfo)
         #t]
        [(cons #f txinfo)
         (set! last-txinfo txinfo)
         (if (zero? retries)
             #f
             (sample! tx (sub1 retries)))]))

    ;; Call reinitialize after model and/or observations have
    ;; changed. To find a valid state that extends the current state,
    ;; use default rerun transition; otherwise, can supply other
    ;; transition to get back to legal state.
    (define/public (reinitialize [transition the-rerun-mh-transition] [attempts +inf.0])
      (sample! transition attempts))

    (define/public (rerun [transition the-rerun-mh-transition])
      (sample! transition 0))

    (define/override (info)
      (print-accinfo (accinfo) 0))

    (define/public (accinfo)
      (Info "== MH sampler"
            [nested "Transition"
                    (send transition accinfo)]
            [nested "Pushed transitions"
                    (for/list ([tx transition-stack]) (send tx accinfo))]))
    ))

(define (->proposal x)
  (cond [(proposal? x) x]
        [(procedure? x) (x)]))
