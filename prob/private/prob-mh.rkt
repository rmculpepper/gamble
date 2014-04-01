;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/list
         racket/class
         racket/match
         data/order
         "context.rkt"
         "util.rkt"
         "sampler.rkt"
         "prob-hooks.rkt"
         "prob-util.rkt")
(provide mh-sampler*
         print-db
         make-db-ERP
         db-mem)

;; Unlike bher, use two databases---better for detecting collisions (debugging).

;; An Entry is (entry ERPTag Dist Any)
;; where the value is appropriate for the ERP denoted by the tag.
(struct entry (tag dist value pinned?) #:prefab)

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

;; default-reject-mode : (U 'top 'same-choice)
;; - 'retry-from-top : re-pick db key to change on rejection
;; - 'retry/same-choice : keep same db key to change on rejection
;; - 'last : replay last state (ie, produce same sample as prev)
(define default-reject-mode 'retry-from-top)

;; default-threshold-mode : (U 'simple 'stale/fresh)
;; - 'simple : R - F + ll_new - ll_old
;; - 'stale/fresh : R - F + ll_new - ll_old + ll_stale - ll_fresh
(define default-threshold-mode 'simple)

;; ----

(define (mh-sampler* thunk spconds)
  (new mh-sampler% (thunk thunk) (spconds spconds)))

(define mh-sampler%
  (class* object% (sampler<%>)
    (init-field thunk
                spconds)
    (field [last-db #f]
           [accepts 0]
           [cond-rejects 0]
           [mh-rejects 0]
           [reject-mode default-reject-mode]
           [threshold-mode default-threshold-mode])
    (super-new)

    (define/public (sample)
      (sample/picked-key (and last-db (pick-a-key last-db))))

    (define/private (sample/picked-key key-to-change)
      (define (reject)
        (case reject-mode
          [(retry-from-top) (sample)]
          [(retry/same-choice) (sample/picked-key key-to-change)]
          [(last) (sample/picked-key #f)]))
      ;; FIXME: avoid so many hash-copies
      (when (verbose?)
        (eprintf "# perturb: changing ~s\n" key-to-change))
      (define perturbed-last-db (if last-db (hash-copy last-db) '#hash()))
      (define R-F (if key-to-change (perturb! perturbed-last-db key-to-change) 0))
      (define current-db (make-hash))
      ;; Run program
      (define result
        (let/ec escape
          (parameterize ((current-ERP (make-db-ERP perturbed-last-db current-db spconds))
                         (current-mem db-mem)
                         (current-fail (lambda (r) (escape (cons 'fail r)))))
            (cons 'okay (apply/delimit thunk)))))
      ;; Accept/reject
      ;; FIXME: check condition or MH step first?
      (match result
        [(cons 'okay sample-value)
         (define threshold (accept-threshold R-F current-db last-db))
         (when (verbose?)
           (eprintf "# accept threshold = ~s\n" threshold))
         (define u (log (random)))
         (cond [(< u threshold)
                (when (verbose?)
                  (eprintf "# Accepted MH step with ~s\n" u))
                (set! last-db current-db)
                (set! accepts (add1 accepts))
                (when (verbose?)
                  (eprintf "# Accepted condition\n"))
                sample-value]
               [else
                (set! mh-rejects (add1 mh-rejects))
                (when (verbose?)
                  (eprintf "# Rejected MH step with ~s\n" u))
                (reject)])]
        [(cons 'fail fail-reason)
         (set! cond-rejects (add1 cond-rejects))
         (when (verbose?)
           (eprintf "# Rejected condition (~s)" fail-reason))
         (reject)]))

    (define/private (accept-threshold R-F current-db prev-db)
      (define prev-ll (if prev-db (db-ll prev-db) -inf.0))
      (define current-ll (db-ll current-db))
      (case threshold-mode
        [(simple)
         (+ R-F (- current-ll prev-ll))]
        [(stale/fresh)
         (define stale (db-ll/difference (or prev-db '#hash()) current-db))
         (define fresh (db-ll/difference current-db (or prev-db '#hash())))

         ;; Copy stale to current-db
         (db-copy-stale (or prev-db '#hash()) current-db)

         (+ R-F (- current-ll prev-ll) (- stale fresh))]))

    (define/public (info)
      (define total (+ accepts cond-rejects mh-rejects))
      (cond [(zero? total)
             (printf "No samples taken.\n")]
            [else
             (printf "Accepted samples: ~s, ~a%\n"
                     accepts (* 100.0 (/ accepts total)))
             (printf "Samples rejected by condition: ~s, ~a%\n"
                     cond-rejects (* 100.0 (/ cond-rejects total)))
             (printf "Samples rejected by MH threshold: ~s, ~a%\n"
                     mh-rejects (* 100.0 (/ mh-rejects total)))]))
    ))

;; pick-a-key : DB -> (U Address #f)
;; Returns a key s.t. the value is not pinned.
;; FIXME: bleh
(define (pick-a-key db)
  (let ([n (for/sum ([(k v) (in-hash db)]
                     #:when (not (entry-pinned? v)))
             1)])
    (and (positive? n)
         (let ([index (random n)])
           (let loop ([iter (hash-iterate-first db)] [i index])
             (cond [(entry-pinned? (hash-iterate-value db iter))
                    (loop (hash-iterate-next db iter) i)]
                   [(zero? i)
                    (hash-iterate-key db iter)]
                   [else
                    (loop (hash-iterate-next db iter) (sub1 i))]))))))

;; perturb! : DB Address (BoxOf Real) -> Real
(define (perturb! db key-to-change)
  (match (hash-ref db key-to-change)
    [(entry tag dist value #f)
     ;; update! : ... -> Real
     (define (update! R F value*)
       (when (verbose?)
         (eprintf "  from ~e to ~e\n" value value*)
         (eprintf "  R = ~s, F = ~s\n" R F))
       (hash-set! db key-to-change (entry tag dist value* #f))
       (- R F))
     (match tag
       [`(normal ,mean ,stddev)
        (define forward-dist
          (make-normal-dist value (/ stddev 2.0)))
        (define value* (dist-sample forward-dist))
        (define backward-dist
          (make-normal-dist value* (/ stddev 2.0)))
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
      [(entry tag dist value _)
       (dist-pdf dist value #t)])))

(define (db-ll/difference db exclude)
  (for/sum ([(k v) (in-hash db)])
    (match v
      [(entry tag dist value _)
       (match (hash-ref exclude k #f)
         [(entry ex-tag ex-dist ex-value _)
          (cond [(and (tags-compatible? tag ex-tag)
                      (equal? value ex-value))
                 ;; kept value and rescored; don't count as fresh/stale
                 0]
                [else (dist-pdf dist value #t)])]
         [#f (dist-pdf dist value #t)])])))

(define (db-copy-stale old-db new-db)
  (for ([(k v) (in-hash old-db)])
    (unless (hash-has-key? new-db k)
      (hash-set! new-db k v))))

;; ----

;; FIXME: handle same w/ different params

(define ((make-db-ERP last-db current-db spconds) tag dist)
  (define context (get-context))
  (define (mem-context?)
    (and (pair? context)
         (let ([frame (last context)])
           (and (list? frame) (memq 'mem frame)))))
  (define (new!)
    (cond [(assoc (current-label) spconds)
           => (lambda (e)
                (match (cdr e)
                  [(spcond:equal value)
                   ;; FIXME: value might not match internal dist support (eg flip vs bernoulli)
                   (cond [(positive? (dist-pdf dist value))
                          (when (verbose?)
                            (eprintf "  CONDITIONED ~s = ~e\n" (current-label) value))
                          (hash-set! current-db context (entry tag dist value #t))
                          value]
                         [else
                          (fail 'condition)])]
                  [(spcond:drawn alt-dist)
                   (error "unimplemented")]))]
          [else
           (define result (dist-sample dist))
           (hash-set! current-db context (entry tag dist result #f))
           result]))
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
                     ;; Note: equal tags implies equal dists
                     (when (verbose?)
                       (eprintf "- REUSED ~s: ~s = ~s\n" tag context (entry-value e)))
                     (hash-set! current-db context e)
                     (entry-value e)]
                    [(and (tags-compatible? (entry-tag e) tag)
                          (positive? (dist-pdf dist (entry-value e))))
                     (when (verbose?)
                       (eprintf "- RESCORE ~s: ~s\n" tag context))
                     (define value (entry-value e))
                     (hash-set! current-db context (entry tag dist value (entry-pinned? e)))
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
         (parameterize ((the-context (list (list 'mem args context))))
           (apply f args)))))))
