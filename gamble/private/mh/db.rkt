;; Copyright (c) 2014-215 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/list
         racket/class
         (rename-in racket/match [match-define defmatch])
         racket/vector
         data/order
         "../context.rkt"
         "../interfaces.rkt"
         "../dist.rkt")
(provide (all-defined-out))

;; ============================================================

;; A Trace is (trace Any DB Real Real Nat)
(struct trace (value db ll-free ll-obs dens-dim))

(define init-trace (trace #f '#hash() 0 0 +inf.0))

;; trace-ll : Trace -> Real
(define (trace-ll t) (+ (trace-ll-free t) (trace-ll-obs t)))

;; trace-nchoices : Trace -> Nat
(define (trace-nchoices t) (db-count (trace-db t)))

;; ============================================================

;; A DB is (Hashof Address Entry)

;; An Entry is (entry (listof Zone) Dist Any Real)
(struct entry (zones dist value ll) #:prefab)

;; entry-in-zone? : Entry ZonePattern -> Boolean
(define (entry-in-zone? e zp)
  (or (not zp) (some-zone-matches? (entry-zones e) zp)))

;; ll-possible? : Real -> Boolean
;; Returns #t if (exp ll) is positive (ie, non-zero).
(define (ll-possible? ll)
  (> ll -inf.0))

;; print-db : DB -> Void
(define (print-db db)
  (define entries (hash-map db list))
  (define sorted-entries
    (sort entries (order-<? datum-order) #:key car))
  (for ([entry (in-list sorted-entries)])
    (printf "~s => ~s\n" (car entry) (cadr entry))))

;; db-count : DB [#:zone ZonePattern] -> Nat
(define (db-count db #:zone [zp #f])
  (if zp
      (for/sum ([(k v) (in-hash db)]
                #:when (entry-in-zone? v zp))
        1)
      (hash-count db)))

;; db-nth : DB Nat -> Address
;; FIXME: bleh, iteration
(define (db-nth db index #:zone [zp #f])
  (let loop ([iter (hash-iterate-first db)] [i index])
    (cond [(let ([e (hash-iterate-value db iter)])
             (not (entry-in-zone? e zp)))
           (loop (hash-iterate-next db iter) i)]
          [(zero? i)
           (hash-iterate-key db iter)]
          [else
           (loop (hash-iterate-next db iter) (sub1 i))])))

;; db-ll : DB -> Real
;; Returns sum of log-likelihoods of all choices in the db.
(define (db-ll db)
  (for/sum ([(k v) (in-hash db)]) (entry-ll v)))

;; db-pick-a-key : DB ZonePattern -> (U Address #f)
(define (db-pick-a-key db zone)
  ;; FIXME: what if zone has no choices?
  (define nchoices/zone (db-count db #:zone zone))
  (and (positive? nchoices/zone)
       (db-nth db (random nchoices/zone) #:zone zone)))

;; ============================================================

(define db-stochastic-ctx%
  (class* object% (stochastic-ctx/run<%>)
    (init-field last-db     ;; not mutated
                delta-db    ;; not mutated
                [on-fresh-choice #f] ;; (U #f (-> Any))
                [escape-prompt (make-continuation-prompt-tag)])

    (field [current-db (make-hash)]
           [ll-free  0]  ;; sum of ll of all entries in current-db
           [ll-obs   0]  ;; sum of ll of all observations
           [ll-diff  0]  ;; INVARIANT: ll-diff = ll-free - OLD(ll-free)
           [dens-dim 0]) ;; density dimension
    (super-new)

    ;; ll-diff = SUM_{k in K} (- (entry-ll current-db[k]) (entry-ll last-db[k]))
    ;;           where K = dom(current-db) intersected with dom(last-db)

    ;; Observations do not affect ll-diff, only ll-obs.

    ;; make-trace : Any -> Trace
    ;; Should only be called after run, once current-db has stopped changing.
    (define/public (make-trace value)
      (trace value current-db ll-free ll-obs dens-dim))

    ;; db-add! : Address Entry -> Void
    ;; Add entry to current-db and update ll-free, ll-obs.
    ;; When last-e is not #f, also update ll-diff.
    (define/private (db-add! context e [last-e #f])
      (hash-set! current-db context e)
      (set! ll-free (+ ll-free (entry-ll e)))
      (when last-e
        (set! ll-diff (+ ll-diff (- (entry-ll e) (entry-ll last-e))))))

    ;; run : (-> A) -> (U (cons 'okay A) (cons 'fail any))
    ;; Run a prob prog using this stochastic ctx, populate current-db, etc.
    (define/public (run thunk)
      (parameterize ((current-stochastic-ctx this))
        (call-with-continuation-prompt
         (lambda () (cons 'okay (apply/delimit thunk)))
         escape-prompt)))

    (define/public (fail reason)
      (abort-current-continuation
       escape-prompt
       (lambda () (cons 'fail reason))))

    ;; The sample method records random choices by mutating
    ;; current-db. At the end of execution, current-db contains a
    ;; complete record of all random choices made by the program;
    ;; if accepted, it typically becomes a new execution's last-db.

    (define/public (sample dist)
      (define context (ADDR-mark))
      ;; If choice address (context) is in current-db, likely error unless
      ;; memoized function (FIXME: shouldn't happen w/ real memoization).
      ;; Otherwise, consult delta-db (represents proposed changes), then
      ;; last-db; they are kept separate to avoid data structure copy and
      ;; also to provide more precise debugging messages.
      (cond [(hash-ref current-db context #f)
             => (lambda (e)
                  (sample/collision dist context e))]
            [(hash-ref delta-db context #f)
             => (lambda (e)
                  (sample/delta dist context e))]
            [(hash-ref last-db context #f)
             => (lambda (e)
                  (sample/last dist context e))]
            [else (sample/new dist context #t)]))

    (define/private (sample/collision dist context e)
      (vprintf "- COLLISION~a ~e: ~s\n" (if (mem-context? context) " (MEM)" "") dist context)
      (collision-error context))

    (define/private (sample/new dist context print?)
      (when on-fresh-choice (on-fresh-choice))
      (define value (dist-sample dist))
      (define ll (dist-pdf dist value #t))
      (when print? (vprintf "NEW ~e: ~s = ~e\n" dist context value))
      (db-add! context (entry (current-zones) dist value ll))
      value)

    (define/private (sample/delta dist context e)
      (define last-e (hash-ref last-db context #f))
      (cond [(not last-e)
             (error 'sample "internal error: choice in delta but not last\n  context: ~e"
                    context)]
            [(equal? (entry-dist e) dist)
             (vprintf "PERTURBED ~e: ~s = ~e\n" dist context (entry-value e))
             (db-add! context e last-e)
             (entry-value e)]
            [(and (dists-same-type? (entry-dist e) dist)
                  (let ([new-ll (dist-pdf dist (entry-value e) #t)])
                    (and (ll-possible? new-ll) new-ll)))
             ;; FIXME: delete this case? should be proposal!
             => (lambda (new-ll)
                  (define value (entry-value e))
                  (define new-e (entry (entry-zones e) dist value new-ll))
                  (vprintf "PERTURBED* ~e: ~s = ~e\n" dist context value)
                  (db-add! context new-e last-e)
                  value)]
            [else
             (vprintf "MISMATCH ~e / ~e: ~s\n" (entry-dist e) dist context)
             (error 'sample "internal error: choice in delta has wrong type\n  context: ~e"
                    context)]))

    (define/private (sample/last dist context e)
      (cond [(equal? (entry-dist e) dist)
             (vprintf "REUSED ~e: ~s = ~e\n" dist context (entry-value e))
             (db-add! context e)
             (entry-value e)]
            [(and (dists-same-type? (entry-dist e) dist)
                  (let ([new-ll (dist-pdf dist (entry-value e) #t)])
                    (and (ll-possible? new-ll) new-ll)))
             ;; FIXME: just fail if old value is impossible!
             => (lambda (new-ll)
                  (define value (entry-value e))
                  (define new-e (entry (entry-zones e) dist value new-ll))
                  (vprintf "RESCORE ~e: ~s = ~e\n" dist context value)
                  (db-add! context new-e e)
                  value)]
            [else
             (vprintf "MISMATCH ~e / ~e: ~s\n" (entry-dist e) dist context)
             (sample/new dist context #f)]))

    (define/public (observe-sample dist val scale)
      (define context (ADDR-mark))
      (observe-sample* dist val (log scale) context))

    (define/private (observe-sample* dist val lscale context)
      (cond [(hash-ref current-db context #f) ;; COLLISION
             => (lambda (e)
                  (vprintf "OBS COLLISION ~e / ~e: ~s\n" (entry-dist e) dist context)
                  (collision-error context))]
            [(hash-ref delta-db context #f) ;; impossible
             => (lambda (e)
                  (error 'observe-sample "internal error: cannot perturb an observation"))]
            [else
             (vprintf "OBS ~e: ~s = ~e\n"  dist context val)
             (define ll (+ lscale (dist-pdf dist val #t)))
             (unless (dist-has-mass? dist)
               (set! dens-dim (add1 dens-dim)))
             (cond [(ll-possible? ll)
                    (set! ll-obs (+ ll-obs ll))]
                   [else (fail 'observation)])]))

    (define/private (mem-context? context)
      (and (pair? context)
           (let ([frame (last context)])
             (and (list? frame) (memq 'mem frame)))))

    (define/private (collision-error context)
      (error 'sample
             (string-append "collision in random choice database"
                            ";\n check that the sampler module uses `#lang prob'"
                            "\n  address: ~e")
             context))

    (define/public (mem f)
      (let ([context (ADDR-mark)]
            [memo-table (make-hash)])
        (lambda args
          (call-with-immediate-continuation-mark OBS-mark
            (lambda (obs)
              (hash-ref! memo-table args
                         (lambda ()
                           (apply/delimit
                            (lambda ()
                              (with-continuation-mark ADDR-mark (list (list 'mem args context))
                                (with-continuation-mark OBS-mark obs
                                  (apply f args))))))))))))
    ))
