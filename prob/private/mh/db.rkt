;; Copyright (c) 2014 Ryan Culpepper
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

;; A DB is (Hashof Address Entry)

;; An Entry is (entry (listof Zone) Dist Any Real Boolean)
;; where the value is appropriate for the ERP denoted by the dist,
;; and pinned? indicates whether the value originated from a special condition
;; (and thus cannot be perturbed).
(struct entry (zones dist value ll pinned?) #:prefab)

;; entry-value-map : (Any -> Any) Entry Boolean -> Entry
(define (entry-value-map f e [update-ll #t])
  (defmatch (entry zones dist value ll pinned?) e)
  (define new-value (f value))
  (entry zones
         dist
         new-value
         (if update-ll (dist-pdf dist new-value #t) ll)
         pinned?))


;; entry-in-zone? : Entry ZonePattern -> Boolean
(define (entry-in-zone? e zp)
  (or (not zp) (some-zone-matches? (entry-zones e) zp)))

;; ll-possible? : Real -> Boolean
;; Returns #t if (exp ll) is positive (ie, non-zero).
(define (ll-possible? ll)
  (> ll -inf.0))

;; ------------------------------------------------------------

(define (print-db db)
  (define entries (hash-map db list))
  (define sorted-entries
    (sort entries (order-<? datum-order) #:key car))
  (for ([entry (in-list sorted-entries)])
    (printf "~s => ~s\n" (car entry) (cadr entry))))

;; db-count-unpinned : DB -> Nat
(define (db-count-unpinned db #:zone [zp #f])
  (for/sum ([(k v) (in-hash db)]
            #:when (not (entry-pinned? v))
            #:when (entry-in-zone? v zp))
    1))

;; db-nth-unpinned : DB Nat -> Address
;; FIXME: bleh, iteration
(define (db-nth-unpinned db index #:zone [zp #f])
  (let loop ([iter (hash-iterate-first db)] [i index])
    (cond [(let ([e (hash-iterate-value db iter)])
             (or (entry-pinned? e) (not (entry-in-zone? e zp))))
           (loop (hash-iterate-next db iter) i)]
          [(zero? i)
           (hash-iterate-key db iter)]
          [else
           (loop (hash-iterate-next db iter) (sub1 i))])))

;; db-ll : DB -> Real
;; Returns sum of log-likelihoods of all choices (including pinned)
;; in the db.
(define (db-ll db)
  (for/sum ([(k v) (in-hash db)])
    (match v
      [(entry _ dist value ll _) ll])))

;; db-ll-pinned : DB -> Real
;; Returns sum of log-likelihoods of all pinned choices (ie observations)
;; in the db.
(define (db-ll-pinned db)
  (for/sum ([(k v) (in-hash db)])
    (match v
      [(entry _ dist value ll #t) ll]
      [_ 0])))

;; db-ll/fresh : DB DB Address -> Real
;; Returns sum of log-likelihoods of *fresh* choices (including pinned)
;; in current-db, whose choices are partitioned into the following sets:
;; - perturbed: the address is in delta-db
;; - rescored: the address is in last-db (and not delta-db), but the
;;             type and value are the same (params may be different)
;; - fresh: otherwise
;; Note: swapping db and last-db yields the stale entries of last-db.
(define (db-ll/fresh current-db last-db delta-db)
  (for/sum ([(k e) (in-hash current-db)])
    (cond [(hash-ref delta-db k #f)
           ;; perturbed
           0]
          [(hash-ref last-db k #f)
           => (lambda (last-e)
                (cond [(and (dists-same-type? (entry-dist e) (entry-dist last-e))
                            (equal? (entry-value e) (entry-value last-e)))
                       ;; rescored
                       0]
                      [else
                       ;; fresh
                       (entry-ll e)]))]
          [else
           (entry-ll e)])))

(define (db-copy-stale old-db new-db)
  (for ([(k v) (in-hash old-db)])
    (unless (hash-has-key? new-db k)
      (hash-set! new-db k v))))

;; db-update! : (Entry -> Entry) DB #f -> Void
;; db-update! : (Address Entry -> Entry) DB #t -> Void
;;
;; Update each entry in the database by applying the given function to it.
;; if #:with-address #t also pass the entry address to the funciton
(define (db-update! f db #:with-address [with-address #f])
  (for ([(k v) (in-hash db)])
    (hash-set! db k (if with-address (f k v) (f v)))))

;; db-map : (Entry -> Entry) DB #f -> DB
;; db-map : (Address Entry -> Entry) DB #t -> DB
;;
;; Copy old-db to new-db and then update it using f.
(define (db-map f old-db #:with-address [with-address #f])
  (define new-db (make-hash))
  (db-copy-stale old-db new-db)
  (db-update! f new-db #:with-address with-address)
  new-db)

;; pick-a-key : Nat DB ZonePattern -> (U Address #f)
;; Returns a key s.t. the value is not pinned.
(define (pick-a-key nchoices db zone)
  ;; FIXME: what if zone has no choices?
  (define nchoices/zone
    (cond [(eq? zone #f) nchoices]
          [else (db-count-unpinned db #:zone zone)]))
  (and (positive? nchoices/zone)
       (db-nth-unpinned db (random nchoices/zone) #:zone zone)))

;; ============================================================

(define db-stochastic-ctx%
  (class* object% (stochastic-ctx/run<%>)
    (init-field last-db     ;; not mutated
                delta-db    ;; not mutated
                [record-obs? #t]
                [escape-prompt (make-continuation-prompt-tag)])

    ;; Optimization: if record-obs? is #f, then don't enter observations (pinned entries)
    ;; in current-db, and don't adjust ll-diff for them. This optimization is only safe
    ;; if the set of observations is known to be constant; we recover the full ll-diff
    ;; by adding difference of new ll-obs and last ll-obs.

    (field [current-db (make-hash)]
           [nchoices 0]  ;; number of unpinned entries in current-db
           [ll-free  0]  ;; sum of ll of all unpinned entries in current-db
           [ll-obs   0]  ;; sum of ll of all observations
           [ll-diff  0]) ;; if record-obs?: (ll-free + ll-obs) - OLD(ll-free + ll-obs)
                         ;; if not record-obs?: ll-free - OLD(ll-free)
    (super-new)

    ;; db-add! : Address Entry -> Void
    ;; Add entry to current-db and update nchoices, ll-free, ll-obs.
    (define/private (db-add! context entry)
      (hash-set! current-db context entry)
      (cond [(entry-pinned? entry)
             (set! ll-obs (+ ll-obs (entry-ll entry)))]
            [else
             (set! ll-free (+ ll-free (entry-ll entry)))
             (set! nchoices (add1 nchoices))]))

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
      (define context (get-context))
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
      (when (verbose?)
        (eprintf "- COLLISION~a ~e: ~s\n"
                 (if (mem-context? context) " (MEM)" "")
                 dist context))
      (collision-error context))

    (define/private (sample/new dist context print?)
      (define value (dist-sample dist))
      (define ll (dist-pdf dist value #t))
      (when (and print? (verbose?))
        (eprintf "- NEW ~e: ~s = ~e\n" dist context value))
      (db-add! context (entry (current-zones) dist value ll #f))
      value)

    (define/private (sample/delta dist context e)
      (define last-e (hash-ref last-db context #f))
      (cond [(not last-e)
             (error 'sample "internal error: choice in delta but not last\n  context: ~e"
                    context)]
            [(equal? (entry-dist e) dist)
             (when (verbose?)
               (eprintf "- PERTURBED ~e: ~s = ~e\n"
                        dist context (entry-value e)))
             (db-add! context e)
             (set! ll-diff (+ ll-diff (- (entry-ll e) (entry-ll last-e))))
             (entry-value e)]
            [(and (dists-same-type? (entry-dist e) dist)
                  (let ([new-ll (dist-pdf dist (entry-value e) #t)])
                    (and (ll-possible? new-ll) new-ll)))
             => (lambda (new-ll)
                  (define value (entry-value e))
                  (when (verbose?)
                    (eprintf "- PERTURBED* ~e: ~s = ~e\n" dist context value))
                  (db-add! context
                           (entry (entry-zones e) dist value new-ll (entry-pinned? e)))
                  (set! ll-diff (+ ll-diff (- new-ll (entry-ll last-e))))
                  value)]
            [else
             (when (verbose?)
               (eprintf "- MISMATCH ~e / ~e: ~s\n" (entry-dist e) dist context))
             (error 'sample "internal error: choice in delta has wrong type\n  context: ~e"
                    context)]))

    (define/private (sample/last dist context e)
      (cond [(equal? (entry-dist e) dist)
             (when (verbose?)
               (eprintf "- REUSED ~e: ~s = ~e\n"
                        dist context (entry-value e)))
             (db-add! context e)
             (entry-value e)]
            [(and (dists-same-type? (entry-dist e) dist)
                  (let ([new-ll (dist-pdf dist (entry-value e) #t)])
                    (and (ll-possible? new-ll) new-ll)))
             => (lambda (new-ll)
                  (define value (entry-value e))
                  (when (verbose?)
                    (eprintf "- RESCORE ~e: ~s = ~e\n" dist context value))
                  (db-add! context
                           (entry (entry-zones e) dist value new-ll (entry-pinned? e)))
                  (set! ll-diff (+ ll-diff (- new-ll (entry-ll e))))
                  value)]
            [else
             (when (verbose?)
               (eprintf "- MISMATCH ~e / ~e: ~s\n" (entry-dist e) dist context))
             (sample/new dist context #f)]))

    (define/public (observe-at dist val)
      (define context (get-context))
      (observe-at* dist val context))

    (define/private (observe-at* dist val context)
      (cond [(hash-ref current-db context #f) ;; COLLISION
             => (lambda (e)
                  (when (verbose?)
                    (eprintf "- OBS COLLISION ~e / ~e: ~s\n"
                             (entry-dist e) dist context))
                  (collision-error context))]
            [(hash-ref delta-db context #f) ;; impossible
             => (lambda (e)
                  (error 'observe-at "internal error: cannot perturb an observation"))]
            [(and record-obs? (hash-ref last-db context #f)) ;; RESCORE
             => (lambda (e)
                  ;; FIXME: better diagnostic messages. What are the
                  ;; relevant cases? Do we care if an obs value changed?
                  (when (verbose?)
                    (eprintf "- OBS UPDATE ....\n"))
                  (define ll (dist-pdf dist val #t))
                  (cond [(ll-possible? ll)
                         (db-add! context (entry (current-zones) dist val ll #t))
                         (set! ll-diff (+ ll-diff (- ll (entry-ll e))))
                         (void)]
                        [else (fail 'observation)]))]
            [else
             (when (verbose?)
               (eprintf "- OBS~a ~e: ~s = ~e\n" 
                        (if record-obs? "" " NEW")
                        dist context val))
             (define ll (dist-pdf dist val #t))
             (cond [(ll-possible? ll)
                    (cond [record-obs?
                           (db-add! context (entry (current-zones) dist val ll #t))]
                          [else
                           (set! ll-obs (+ ll-obs ll))])]
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
      (let ([context (get-context)]
            [memo-table (make-hash)])
        (lambda args
          (with-continuation-mark
              obs-mark 'ok
            (hash-ref! memo-table args
                       (lambda ()
                         (apply/delimit
                          (lambda ()
                            (parameterize ((the-context (list (list 'mem args context))))
                              (apply f args))))))))))
    ))

(define db-stochastic-derivative-ctx%
  (class* db-stochastic-ctx% (stochastic-ctx/run<%>)
    (field [derivatives (make-hash)]      ;; (Hashof Address Derivative)
           [relevant-labels (make-hash)]) ;; (Hashof Label Address)
    (super-new)

    (define/override (sample dist) 
      (record-current-label)
      (record-current-derivatives)
      (super sample dist))

    (define/override (observe-at dist val)
      (record-current-label)
      (record-current-derivatives)
      (super observe-at dist val))
    
    (define/private (record-current-label)
      (define lbl (current-label))
      (when lbl
        (define context (get-context))
        (hash-set! relevant-labels lbl context)))

    (define/private (record-current-derivatives)
      (define label-derivs (current-derivatives))
      (when label-derivs
        (define context (get-context))
        (define address-derivs
          (vector-map (λ (parameter-derivative)
                        (derivative-label->address parameter-derivative))
                      label-derivs))
        (hash-set! derivatives context address-derivs)))

    ;; (U #f (Pair (Vector Label) DerivativeFn))
    ;; -> (U #f (Pair (Vector Address) DerivativeFn))
    ;;
    ;; replace each label by its latest address.
    (define/private (derivative-label->address pd)
      (cond
       [pd
        (define labels (car pd))
        (define derivative-fn (cdr pd))
        (cons (vector-map (λ (lbl) (hash-ref relevant-labels lbl)) labels)
              derivative-fn)]
       [else #f]))))
