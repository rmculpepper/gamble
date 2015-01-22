;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         racket/place
         racket/future
         racket/serialize
         "serializable-lambda.rkt")
(provide (all-defined-out))

;; FIXME/TODO:
;; - adjust protocols to propagate exceptions

;; Clean up place when manager client object is GC'd. Thus, every
;; worker should maintain ref to manager to keep alive.
;; Clean up worker thread when worker client object is GC'd.

(define p-executor (make-will-executor))
(define p-executor-thread
  (thread (lambda () (let loop () (will-execute p-executor) (loop)))))


;; == Manager protocols:

;; Spawn protocol
;; C->S : (list 'spawn Place-Channel (Serialized (Place-Channel -> Any)))

;; Quit protocol
;; C->S : (list 'quit)

(define manager%
  (class object%
    (super-new)
    (field [ws (place in (manager-server-loop in))])

    (will-register p-executor this (lambda (self) (send self finalize)))

    (define/public (spawn proc)
      (define-values (here there) (place-channel))
      (place-channel-put ws (list 'spawn there (serialize proc)))
      here)
    (define/public (quit)
      (place-channel-put ws (list 'quit)))

    (define/public (finalize)
      (place-channel-put ws (list 'quit))
      (sleep 0.01)
      (place-kill ws))
    ))

(define (manager-server-loop in)
  (let loop ()
    (match (sync in)
      [(list 'spawn ch s-proc)
       (define proc (deserialize s-proc))
       (thread (lambda () (proc ch)))
       (loop)]
      [(list 'quit)
       (void)])))

;; ------------------------------------------------------------

(define the-managers-cache (make-weak-box #f))

(define (get-managers)
  (let ([ms (weak-box-value the-managers-cache)])
    (or ms
        (let ([ms (for/list ([i (processor-count)]) (new manager%))])
          (set! the-managers-cache (make-weak-box ms))
          ms))))

;; ------------------------------------------------------------

;; A StatefulWorker maintains a state and updates it based on
;; state-transformer functions it receives.

;; StatefulWorker protocols:

;; Update protocol:
;; C->S : (list 'update (Serialized (State -> Any)))

;; View protocol:
;; C->S : (list 'view (Serialized (U #f (State -> Any))))
;; S->C : (list 'value (Serialized Any))

;; Set protocol:
;; C->S : (list 'set (Serialized Any))

;; Quit protocol:
;; C->S : (list 'quit)

(define stateful-worker%
  (class object%
    (init-field manager)
    (super-new)

    (field [sws (send manager spawn stateful-worker-loop)])

    (will-register p-executor this (lambda (self) (send self finalize)))

    (define/public (update proc)
      (place-channel-put sws (list 'update (serialize proc))))
    (define/public (get)
      (place-channel-put sws (list 'view (serialize #f)))
      (match (place-channel-get sws)
        [(list 'value value) value]))
    (define/public (view proc)
      (place-channel-put sws (list 'view (serialize proc)))
      (match (place-channel-get sws)
        [(list 'value value) value]))
    (define/public (set value)
      (place-channel-put sws (list 'set (serialize value))))
    (define/public (quit)
      (place-channel-put sws (list 'quit)))

    (define/public (finalize)
      (quit))
    ))

(define/s (stateful-worker-loop c)
  (let loop ([state #f])
    (match (sync c)
      [(list 'update s-proc)
       (define proc (deserialize s-proc))
       (loop (proc state))]
      [(list 'view s-proc)
       (define proc (or (deserialize s-proc) values))
       (place-channel-put c (list 'value (proc state)))
       (loop state)]
      [(list 'set s-val)
       (loop (deserialize s-val))]
      [(list 'quit)
       (void)])))

;; ------------------------------------------------------------

;; A HistoricalWorker is like a StatefulWorker but maintains a history
;; of previous states and only forgets them upon request. Note:
;; despite the use of integer timestamps, "history" is not linear: one
;; can create state 10 as an update of state 4, for example.

(define historical-worker%
  (class object%
    (init-field manager)
    (super-new)

    ;; sw : StatefulWorker[ImmutableHash[Nat => State]]
    (field [sw (new stateful-worker% [manager manager])]
           [now 0])
    (send sw set #hash())

    (define/public (get-now) now)

    (define/public (update then proc)
      (set! now (add1 now))
      (let ([now now]) ;; turn field into local variable for serializability
        (send sw update
              (lambda/s (h) (hash-set h now (proc (hash-ref h then #f))))))
      now)

    (define/public (get then)
      (send sw view (lambda/s (h) (hash-ref h then))))
    (define/public (view then proc)
      (send sw view (lambda/s (h) (proc (hash-ref h then)))))
    (define/public (set then value)
      (send sw update (lambda/s (h) (hash-set h then value))))
    (define/public (forget then)
      (send sw update (lambda/s (h) (hash-remove h then))))
    (define/public (quit)
      (send sw quit))
    ))
