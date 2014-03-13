;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/list
         racket/match
         data/order
         "context.rkt"
         "util.rkt"
         "prob-hooks.rkt")
(provide mh-sampler*
         print-db
         make-db-ERP
         db-mem)

;; Unlike bher, use two databases---better for detecting collisions (debugging).

;; An Entry is (entry ERPTag Dist Any)
;; where the value is appropriate for the ERP denoted by the tag.
(struct entry (tag dist value) #:prefab)

(define (print-db db)
  (define entries (hash-map db list))
  (define sorted-entries
    (sort entries (order-<? datum-order) #:key car))
  (for ([entry (in-list sorted-entries)])
    (printf "~s => ~s\n" (car entry) (cadr entry))))

;; ----

#|
MH Acceptance

Let X, X' be full traces (dbs)

X  = Xsame  U Xdiff  U Xstale  U Xfresh
X' = Xsame' U Xdiff' U Xstale' U Xfresh'

  Xsame = Xsame' -- part of trace that stayed same
  Xdiff != Xdiff' -- part of trace w/ changed params, kept value
                  -- OR is the directly perturbed part
  Xstale -- part of old trace not reused
  Xfresh' -- new part of new trace

  Note: Xfresh = empty, Xstale' = empty

MH acceptance ratio (from Sean):

  P(X' | Obs)   Q(X | X')
  ----------- * ---------
  P(x  | Obs)   Q(X' | X)

  where Q is whole-trace proposal distribution

MH acceptance ratio from Bher paper:

  P(X')   Kt(x|x', theta)   P(Xstale)
  ----- * --------------- * ----------
  P(X)    Kt(x'|x, theta)   P(Xfresh')

  where x,x' are values of a single ERP

Many things cancel, resulting in

  P(Xdiff')   Kt(x|x', theta)
  --------- * ---------------
  P(Xdiff)    Kt(x'|x, theta)

So, need to accumulate

  lldiff = log Kt(x|x', theta) - log Kt(x'|x, theta)
           + log P(Xdiff') - log P(Xdiff')

depending on only choices reused w/ different params.

|#

;; RUN-FROM-TOP? : Boolean
;; If #t, then run from top (re-pick choice to change) on rejection;
;; if #f, then run with same choice.
(define RUN-FROM-TOP? #t)

;; MH-threshold-method : (U 'simple 'stale/fresh)
(define MH-threshold-method 'stale/fresh)

;; FIXME: could parameterize over perturb! function, or parameters thereof

(define (mh-sampler* thunk pred [project values])
  (let ([last-db #f]
        [current-db #f])
    (define (run)
      (run/picked-key (and current-db (pick-a-key current-db))))
    (define (run/picked-key key-to-change)
      ;; FIXME: last-db vs saved-current-db ...
      (define saved-last-db last-db)
      (define saved-current-db (and current-db (hash-copy current-db)))
      ;; Prepare
      ;; TODO: avoid so many hash-copies
      (set! last-db current-db)
      (set! current-db (make-hash))
      ;; Retry undoes prepare step
      (define (retry)
        (set! last-db saved-last-db)
        (set! current-db saved-current-db)
        (if RUN-FROM-TOP? (run) (run/picked-key key-to-change)))
      ;; Rotate & perturb
      (define R-F (if key-to-change (perturb! last-db key-to-change) 0))
      ;; Run
      (define result
        (parameterize ((current-ERP (make-db-ERP (or last-db '#hash()) current-db))
                       (current-mem db-mem))
          (apply/delimit thunk)))
      ;; Accept/reject
      ;; NOTE: use saved-current-db; last-db is saved-current-db BUT with perturbation
      ;; FIXME: yuck
      (define threshold (accept-threshold R-F current-db saved-current-db))
      (when (verbose?)
        (eprintf "# accept thresold = ~s\n" threshold))
      (define u (log (random)))
      (cond [(< u threshold)
             (when (verbose?)
               (eprintf "# Accepted MH step with ~s\n" u))
             (cond [(pred result)
                    ;; Post???
                    (when (verbose?)
                      (eprintf "# Accepted condition\n"))
                    (project result)]
                   [else
                    (when (verbose?)
                      (eprintf "# Rejected condition"))
                    (retry)])]
            [else
             (when (verbose?)
               (eprintf "# Rejected MH step with ~s\n" u))
             (retry)]))
    run))

(define (accept-threshold R-F current-db prev-db)
  (define prev-ll (if prev-db (db-ll prev-db) -inf.0))
  (define current-ll (db-ll current-db))
  (case MH-threshold-method
    [(simple)
     (+ R-F (- current-ll prev-ll))]
    [(stale/fresh)
     (define stale (db-ll/difference (or prev-db '#hash()) current-db))
     (define fresh (db-ll/difference current-db (or prev-db '#hash())))
     (+ R-F (- current-ll prev-ll) (- stale fresh))]))

;; pick-a-key : DB -> (U Address #f)
(define (pick-a-key db)
  (let ([n (hash-count db)])
    (and (positive? n)
         (let* ([index (random n)]
                [iter
                 (for/fold ([iter (hash-iterate-first db)])
                     ([i (in-range index)])
                   (hash-iterate-next db iter))])
           (hash-iterate-key db iter)))))

(require math/distributions)

;; perturb! : DB Address (BoxOf Real) -> Real
(define (perturb! db key-to-change)
  (when (verbose?)
    (eprintf "perturb: changing ~s\n" key-to-change))
  (match (hash-ref db key-to-change)
    [(entry tag dist value)
     ;; update! : ... -> Real
     (define (update! R F value*)
       (when (verbose?)
         (eprintf "  from ~e to ~e\n" value value*)
         (eprintf "  R = ~s, F = ~s\n" R F))
       (hash-set! db key-to-change (entry tag dist value*))
       (- R F))
     (match tag
       [`(normal ,mean ,stddev)
        (define forward-dist
          (let ([mean* value]
                [stddev* (/ stddev 2.0)])
            (make-dist normal #:params (mean* stddev*) #:enum #f)))
        (define value* (dist-sample forward-dist))
        (define backward-dist
          (let ([mean* value*]
                [stddev* (/ stddev 2.0)])
            (make-dist normal #:params (mean* stddev*) #:enum #f)))
        (define R (dist-pdf backward-dist value #t))
        (define F (dist-pdf forward-dist value* #t))
        (update! R F value*)]
       ;; FIXME: insert specialized proposal distributions here
       [_ ;; Fallback: Just resample from same dist.
        ;; Then Kt(x|x') = Kt(x) = (dist-pdf dist value)
        ;;  and Kt(x'|x) = Kt(x') = (dist-pdf dist value*)
        ;; All cancel, so just pass 0 for all.
        (update! 0 0 (dist-sample dist))])]))

(define (db-ll db)
  (for/sum ([(k v) (in-hash db)])
    (match v
      [(entry tag dist value)
       (dist-pdf dist value #t)])))

(define (db-ll/difference db exclude)
  (for/sum ([(k v) (in-hash db)])
    (match v
      [(entry tag dist value)
       (match (hash-ref exclude k #f)
         [(entry ex-tag ex-dist ex-value)
          (if (equal? tag ex-tag) ;; FIXME: approx!
              0
              (dist-pdf dist value #t))]
         [_ (dist-pdf dist value #t)])])))

;; ----

;; FIXME: handle same w/ different params

(define ((make-db-ERP last-db current-db) tag dist)
  (define context (get-context))
  (define (mem-context?)
    (and (pair? context)
         (let ([frame (last context)])
           (and (list? frame) (memq 'mem frame)))))
  (define (new!)
    (define result (dist-sample dist))
    (hash-set! current-db context (entry tag dist result))
    result)
  (cond [(hash-ref current-db context #f)
         => (lambda (e)
              (cond [(not (equal? (entry-tag e) tag))
                     (when (or (verbose?) #t)
                       (eprintf "- MISMATCH ~a ~s / ~s: ~s\n"
                                (if (mem-context?) "MEMOIZED" "COLLISION")
                                (entry-tag e) tag context))
                     (new!)]
                    [(mem-context?)
                     (when (verbose?)
                       (eprintf "- MEMOIZED ~s: ~s\n" tag context))
                     (entry-value e)]
                    [else
                     (when (verbose?)
                       (eprintf "- COLLISION ~s: ~s\n" tag context))
                     (entry-value e)]))]
        [(hash-ref last-db context #f)
         => (lambda (e)
              (cond [(equal? (entry-tag e) tag)
                     (when (verbose?)
                       (eprintf "- REUSED ~s: ~s\n" tag context))
                     (define result (entry-value e))
                     (hash-set! current-db context (entry tag dist result))
                     result]
                    [(and (tags-compatible? (entry-tag e) tag)
                          (positive? (dist-pdf dist (entry-value e))))
                     (define value (entry-value e))
                     (hash-set! current-db context (entry tag dist value))
                     value]
                    [(not (equal? (entry-tag e) tag))
                     (when (verbose?)
                       (eprintf "- MISMATCH ~s / ~s: ~s\n" (entry-tag e) tag context))
                     (new!)]))]
        [else
         (when (verbose?)
           (eprintf "- NEW ~s: ~s\n" tag context))
         (new!)]))

(define (tags-compatible? old-tag new-tag)
  (and (eq? (car old-tag) (car new-tag))))

(define (db-mem f)
  (let ([context (get-context)])
    (lambda args
      (apply/delimit
       (lambda ()
         (with-continuation-mark CM-KEY (list 'mem args context)
           (apply f args)))))))
