;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/list
         racket/class
         racket/match
         racket/vector
         data/order
         "context.rkt"
         "util.rkt"
         "interfaces.rkt"
         "../dist.rkt"
         (only-in "dist.rkt" dists-same-type?)
         "prob-util.rkt")
(provide (all-defined-out))

;; An Entry is (entry Dist Any Boolean)
;; where the value is appropriate for the ERP denoted by the dist,
;; and pinned? indicates whether the value originated from a special condition
;; (and thus cannot be perturbed).
(struct entry (dist value pinned?) #:prefab)

;; modify-entry-value : (Any -> Any) Entry -> Entry
(define (entry-value-map f e)
  (entry (entry-dist e) (f (entry-value e)) (entry-pinned? e)))
;; ------------------------------------------------------------

;; A DB is (Hashof Address Entry)

(define (print-db db)
  (define entries (hash-map db list))
  (define sorted-entries
    (sort entries (order-<? datum-order) #:key car))
  (for ([entry (in-list sorted-entries)])
    (printf "~s => ~s\n" (car entry) (cadr entry))))

;; db-count-unpinned : DB -> Nat
(define (db-count-unpinned db)
  (for/sum ([(k v) (in-hash db)]
            #:when (not (entry-pinned? v)))
    1))

;; db-nth-unpinned : DB Nat -> Address
;; FIXME: bleh, iteration
(define (db-nth-unpinned db index)
  (let loop ([iter (hash-iterate-first db)] [i index])
    (cond [(entry-pinned? (hash-iterate-value db iter))
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
      [(entry dist value _)
       (when #f
         (when (verbose?)
           (eprintf "  - ~e => ~e @ ~s\n" dist value (dist-pdf dist value))))
       (dist-pdf dist value #t)])))

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
                       (dist-pdf (entry-dist e) (entry-value e) #t)]))]
          [else
           (dist-pdf (entry-dist e) (entry-value e) #t)])))

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

;; ============================================================

(define db-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (init-field current-db  ;; mutated
                last-db     ;; not mutated
                delta-db    ;; not mutated
                spconds
                escape)
    (super-new)

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
                  (sample/old dist context e #t))]
            [(hash-ref last-db context #f)
             => (lambda (e)
                  (sample/old dist context e #f))]
            [else (sample/new dist context #t)]))

    (define/private (sample/collision dist context e)
      (cond [(not (equal? (entry-dist e) dist))
             (when (verbose?)
               (eprintf "- MISMATCH ~a ~e / ~e: ~s\n"
                        (if (mem-context? context) "MEMOIZED" "COLLISION")
                        (entry-dist e) dist context))
             (collision-error context)]
            [(mem-context? context)
             (when (verbose?)
               (eprintf "- MEMOIZED ~e: ~s = ~e\n" dist context (entry-value e)))
             (entry-value e)]
            [else
             (when (verbose?)
               (eprintf "- COLLISION ~e: ~s\n" dist context))
             (collision-error context)]))

    (define/private (sample/new dist context print?)
      (cond [(assoc (current-label) spconds)
             => (lambda (e)
                  (match (cdr e)
                    [(spcond:equal value)
                     ;; FIXME: value might not match internal dist support (eg flip vs bernoulli)
                     (cond [(positive? (dist-pdf dist value))
                            (when (verbose?)
                              (when print?
                                (eprintf "- NEW ~e: ~s = ~e\n" dist context value))
                              (eprintf "  CONDITIONED ~e = ~e\n" (current-label) value))
                            (hash-set! current-db context (entry dist value #t))
                            value]
                           [else
                            (fail 'observation)])]))]
            [else
             (define value (dist-sample dist))
             (when (and print? (verbose?))
               (eprintf "- NEW ~e: ~s = ~e\n" dist context value))
             (hash-set! current-db context (entry dist value #f))
             value]))

    (define/private (sample/old dist context e in-delta?)
      (cond [(equal? (entry-dist e) dist)
             (when (verbose?)
               (eprintf "- ~a ~e: ~s = ~e\n"
                        (if in-delta? "PERTURBED" "REUSED")
                        dist context (entry-value e)))
             (hash-set! current-db context e)
             (entry-value e)]
            [(and (dists-same-type? (entry-dist e) dist)
                  (positive? (dist-pdf dist (entry-value e))))
             (define value (entry-value e))
             (when (verbose?)
               (eprintf "- RESCORE ~e: ~s = ~e\n" dist context value))
             (hash-set! current-db context (entry dist value (entry-pinned? e)))
             value]
            [else
             (when (verbose?)
               (eprintf "- MISMATCH ~e / ~e: ~s\n" (entry-dist e) dist context))
             (sample/new dist context #f)]))

    (define/public (observe-at dist val)
      (define context (get-context))
      (cond [(hash-ref current-db context #f) ;; COLLISION
             => (lambda (e)
                  (cond [(mem-context? context)
                         (when (verbose?)
                           (eprintf "- OBS MEMOIZED ~e / ~e ~s\n"
                                    (entry-dist e) dist context))]
                        [else
                         (when (verbose?)
                           (eprintf "- OBS COLLISION ~e / ~e: ~s\n"
                                    (entry-dist e) dist context))
                         (collision-error context)]))]
            [(hash-ref delta-db context #f) ;; impossible
             => (lambda (e)
                  (error 'observe-at "internal error: cannot perturb an observation"))]
            [(hash-ref last-db context #f) ;; RESCORE
             => (lambda (e)
                  ;; FIXME: better diagnostic messages. What are the
                  ;; relevant cases? Do we care if an obs value changed?
                  (when (verbose?)
                    (eprintf "- OBS UPDATE ....\n"))
                  (cond [(positive? (dist-pdf dist val))
                         (hash-set! current-db context (entry dist val #t))
                         (void)]
                        [else (fail 'observation)]))]
            [else
             (when (verbose?)
               (eprintf "- OBS NEW ~e: ~s = ~e\n" dist context val))
             (cond [(positive? (dist-pdf dist val))
                    (hash-set! current-db context (entry dist val #t))]
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

    (define/public (fail reason)
      (escape (cons 'fail reason)))

    (define/public (mem f)
      (let ([context (get-context)])
        (lambda args
          (apply/delimit
           (lambda ()
             (parameterize ((the-context (list (list 'mem args context))))
               (apply f args)))))))
    ))

(define db-stochastic-derivative-ctx%
  (class* db-stochastic-ctx% (stochastic-ctx<%>)
    (init current-db
          last-db
          delta-db
          spconds
          escape)
    (field [derivatives (make-hash)]      ;; (Hashof Address Derivative)
           [relevant-labels (make-hash)]) ;; (Hashof Label Address)
    (super-new [current-db current-db]
               [last-db last-db]
               [delta-db delta-db]
               [spconds spconds]
               [escape escape])
    
    (define/override (sample dist) 
      (record-current-label)
      (record-current-derivatives)
      (super sample dist))
    
    (define (record-current-label)
      (define lbl (current-label))
      (when lbl
        (define context (get-context))
        (hash-set! relevant-labels lbl context)))
    
    (define (record-current-derivatives)
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
        
