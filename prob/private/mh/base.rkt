;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         (rename-in racket/match [match-define defmatch])
         racket/math
         racket/vector
         "db.rkt"
         "../interfaces.rkt"
         "../dist.rkt"
         "../../dist/discrete.rkt")
(provide (all-defined-out))

;; ============================================================

(define mh-transition<%>
  (interface ()
    run  ;; (-> A) Trace -> TransitionResult
    info ;; Nat -> Void
    ))

;; A TransitionResult is one of
;; - Trace  -- run completed, threshold already checked (if applicable)
;; - #f     -- run failed or mh-rejected

;; A Trace is (trace Any DB Nat Real Real)
(struct trace (value db nchoices ll-free ll-obs))
(define init-trace (trace #f '#hash() 0 0 0))

;; ============================================================

(define mh-transition-base%
  (class* object% (mh-transition<%>)
    (field [accepts 0]
           [mh-rejects 0]
           [cond-rejects 0])
    (super-new)

    (define/public (info i)
      (define total (+ accepts cond-rejects mh-rejects))
      (iprintf i "Total runs: ~s\n" total)
      (iprintf i "Accepted traces: ~s, ~a%\n"
               accepts (%age accepts total))
      (iprintf i "Traces rejected by condition: ~s, ~a%\n"
               cond-rejects (%age cond-rejects total))
      (iprintf i "Traces rejected by MH threshold: ~s, ~a%\n"
               mh-rejects (%age mh-rejects total)))

    ;; run : (-> A) Trace -> RunResult
    (define/public (run thunk last-trace)
      (match (run* thunk last-trace)
        [(cons (? real? threshold) trace)
         (when (verbose?)
           (eprintf "# accept threshold = ~s\n" (exp threshold)))
         (define u (log (random)))
         (cond [(< u threshold)
                (when (verbose?)
                  (eprintf "# Accepted MH step with ~s\n" (exp u)))
                (set! accepts (add1 accepts))
                trace]
               [else
                (when (verbose?)
                  (eprintf "# Rejected MH step with ~s\n" (exp u)))
                (set! mh-rejects (add1 mh-rejects))
                #f])]
        [(cons 'fail reason)
         (set! cond-rejects (add1 cond-rejects))
         (when (verbose?)
           (eprintf "# Rejected condition (~s)\n" reason))
         #f]))

    ;; run* : (-> A) Trace -> (U (cons Real Trace) (cons 'fail any))
    (abstract run*)
    ))

;; ============================================================

;; ProposalMap = hash[ Zone => (listof ProposalFun) ])
;; where ProposalFun = (Dist Value -> (U (cons Value Real) #f))
;; The function returns a new value and the proposal's R-F.

;; extend-proposal-map : ProposalMap {Zone ProposalFun}* -> ProposalMap
(define (extend-proposal-map pm . args)
  (unless (even? (length args))
    (error 'extend-proposal-map "expected an even number of {zone,function} arguments"))
  (let loop ([args args])
    (cond [(and (pair? args) (pair? (cdr args)))
           (extend-proposal-map1 (loop (cddr args)) (car args) (cadr args))]
          [else pm])))

;; extend-proposal-map1 : ProposalMap Zone ProposalFun -> ProposalMap
(define (extend-proposal-map1 pm zone fun)
  (hash-set pm zone (cons fun (hash-ref pm zone null))))

;; apply-proposal-map : ProposalMap (Listof Zone) Dist Value -> (U (cons Value Real) #f)
;; Try all functions for primary (closest enclosing) zone, then for second zone, etc.
(define (apply-proposal-map pm zones dist val)
  (or (apply-proposal-map* pm zones dist val)
      (apply-proposal-map* pm '(#f) dist val)))

(define (apply-proposal-map* pm zonelist dist val)
  (for/or ([z (in-list zonelist)])
    (for/or ([fun (in-list (hash-ref pm z null))])
      (fun dist val))))

;; proposal-map : (parameterof ProposalMap)
(define proposal-map (make-parameter '#hash()))

(define (make-default-proposal scale-factor)
  (lambda (dist value)
    (or (*drift dist value scale-factor)
        #f)))

;; used by slice sampler
(define (real-dist-adjust-value dist value scale-factor)
  (*slice-adjust dist value scale-factor))

(proposal-map
 (extend-proposal-map
  (proposal-map)
  #f (make-default-proposal 1/4)))

;; ============================================================

(define (trace-ll t) (+ (trace-ll-free t) (trace-ll-obs t)))

(define (iprintf i fmt . args)
  (display (make-string i #\space))
  (apply printf fmt args))

(define (%age nom denom)
  (/ (* 100.0 nom) (exact->inexact denom)))
