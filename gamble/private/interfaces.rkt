;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/match
         "context.rkt"
         (for-syntax racket/base syntax/parse)
         (only-in "dist.rkt" dist-sample dist-pdf dist-has-mass?))
(provide sample
         mem
         observe-sample
         fail
         trycatch
         verbose?
         with-verbose>
         vprintf
         Info
         print-accinfo
         weighted-sampler<%>
         sampler<%>
         weighted-sampler?
         sampler?
         sampler-base%
         stochastic-ctx<%>
         stochastic-ctx/run<%>
         current-stochastic-ctx
         plain-stochastic-ctx%
         plain-stochastic-ctx/run%
         current-zones
         zone-matches?
         some-zone-matches?
         current-label)

;; Defines interfaces, base classes, and parameters.

;; ============================================================
;; Debugging

;; FIXME: add logger?

(define verbose? (make-parameter #f))
(define verbose-indent (make-parameter 0))

(define-syntax-rule (with-verbose> body ...)
  (parameterize ((verbose-indent (+ (verbose-indent) 2))) body ...))

(define-syntax-rule (vprintf fmt arg ...)
  (when #t
    (when (verbose?)
      (eprintf "# ~a" (make-string (verbose-indent) #\space))
      (eprintf fmt arg ...))))

;; ============================================================
;; Info

;; Designed to minimize allocation (hopefully)

;; An AccInfo is (vector AccInfoHeader Value ...)
;; An AccInfoHeader is (Listof (List String AccInfoKeyType)
;; An AccInfoKeyType is one of #:label, #:value, #:percent, #:nested

(define-syntax (Info stx)
  (define-syntax-class clause
    #:attributes (key value)
    #:datum-literals (% nested include)
    (pattern s:str
             #:with key #'(#f #:label)
             #:with value #''s)
    (pattern [s:str value:expr]
             #:with key #'(s #:value))
    (pattern [% s:str v:expr tot:expr]
             #:with key #'(s #:percent)
             #:with value #'(cons v tot))
    (pattern [include value:expr]
             #:with key #'(#f #:include))
    (pattern [nested s:str value:expr]
             #:with key #'(s #:nested)))
  (syntax-parse stx
    [(_ c:clause ...)
     #'(vector '(c.key ...) c.value ...)]))

(define (print-accinfo ai i)
  (for ([key (in-list (vector-ref ai 0))]
        [value (in-vector ai 1)])
    (match key
      [(list #f '#:include)
       (print-accinfo value i)]
      [(list #f '#:label)
       (iprintf i "~a\n" value)]
      [(list label '#:value)
       (iprintf i "~a: ~v\n" label value)]
      [(list label '#:percent)
       (match value
         [(cons x total)
          (iprintf i "~a: ~v, ~v%\n" label x (%age x total))])]
      [(list label '#:nested)
       (unless (null? value)
         (iprintf i "~a:\n" label)
         (let ([value (if (list? value) value (list value))])
           (if (null? value)
               (iprintf (+ i 2) "(empty)")
               (for ([sub value]) (print-accinfo sub (+ i 2))))))])))

(define (iprintf i fmt . args)
  (write-string (make-string i #\space))
  (apply printf fmt args))

(define (%age nom denom)
  (/ (* 100.0 nom) (exact->inexact denom)))

;; ============================================================
;; Samplers

(define weighted-sampler<%>
  (interface ()
    sample/weight  ;; -> (cons Any PositiveReal)
    info           ;; -> Void
    ))

;; A sampler is an applicable object taking zero arguments. When
;; applied, it produces a single sample.
(define sampler<%>
  (interface* (weighted-sampler<%>)
              ([prop:procedure (lambda (this) (send this sample))])
    sample  ;; -> Any
    ))

(define (weighted-sampler? x) (is-a? x weighted-sampler<%>))
(define (sampler? x) (is-a? x sampler<%>))

;; Automatic impl of weighted sampler from "ordinary" sampler.
(define sampler-base%
  (class* object% (sampler<%>)
    (super-new)
    (abstract sample)
    (abstract info)
    (define/public (sample/weight) (cons (sample) 1))))

;; ============================================================
;; Stochastic contexts

(define stochastic-ctx<%>
  (interface ()
    sample  ;; (Dist A) -> A
    observe-sample ;; (Dist A) A PosReal -> Void
    fail    ;; Any -> (escapes)
    mem     ;; Function -> Function
    ))

(define stochastic-ctx/run<%>
  (interface (stochastic-ctx<%>)
    run     ;; (-> A) -> (U (cons 'okay A) (cons 'fail Any))
    ))

(define stochastic-ctx/trycatch<%>
  (interface (stochastic-ctx<%>)
    trycatch ;; (-> A) (-> A) -> A
    ))

(define plain-stochastic-ctx%
  (class* object% (stochastic-ctx<%>)
    (super-new)

    (define/public (sample dist)
      (dist-sample dist))

    (define/public (observe-sample dist val scale)
      ;; No ambient weight to affect; just check likelihood is non-zero.
      (when (zero? (dist-pdf dist val))
        (fail 'observation)))

    (define/public (mem f)
      (let ([memo-table (make-hash)])
        (define (memoized-function . args)
          (call-with-immediate-continuation-mark OBS-mark
            (lambda (obs)
              (hash-ref! memo-table args
                         (lambda () (with-continuation-mark OBS-mark obs (apply f args)))))))
        memoized-function))

    (define/public (fail reason)
      (if reason
          (error 'fail "failed\n  reason: ~s" reason)
          (error 'fail "failed")))
    ))

(define plain-stochastic-ctx/run%
  (class* plain-stochastic-ctx% (stochastic-ctx/run<%>)
    (init-field [escape-prompt (make-continuation-prompt-tag)])
    (super-new)

    (define/public (run thunk)
      (parameterize ((current-stochastic-ctx this))
        (call-with-continuation-prompt
         (lambda () (cons 'okay (thunk)))
         escape-prompt)))

    (define/override (fail reason)
      (abort-current-continuation
       escape-prompt
       (lambda () (cons 'fail reason))))
    ))

(define current-stochastic-ctx
  (make-parameter (new plain-stochastic-ctx%)))

;; ============================================================
;; Primitive operations

(define (mem f) (send (current-stochastic-ctx) mem f))
(define (observe-sample dist val [scale 1])
  ;; scale only applies to continuous dists; discard here if dist has mass function
  ;; impl methods trust scale arg to be appropriate
  (send (current-stochastic-ctx) observe-sample dist val
        (if (dist-has-mass? dist) 1 scale)))
(define (fail [reason #f]) (send (current-stochastic-ctx) fail reason))
(define (sample* dist) (send (current-stochastic-ctx) sample dist))

(define (sample dist)
  (call-with-immediate-continuation-mark OBS-mark
    (lambda (obs)
      (cond [obs
             (define value (observation-value obs))
             (define scale (observation-scale obs))
             (when (verbose?)
               (vprintf "OBSERVE w/ context = ~e\n" obs)
               (vprintf "  sample -> condition: ~e @ ~e w/ scale = ~s\n" dist value
                        (if (dist-has-mass? dist) 1 scale)))
             (observe-sample dist value scale)
             value]
            [else (sample* dist)]))))

(define (trycatch p1 p2)
  (send (current-stochastic-ctx) trycatch p1 p2))

;; ============================================================
;; Zones

;; A Zone is any non-false value (but typically a symbol or list with
;; symbols and numbers).

;; A ZonePattern is one of
;; - Zone  -- matches that zone
;; - #f    -- matches anything

(define current-zones (make-parameter null))

(define (zone-matches? z zp)
  (or (not zp) (equal? z zp)))

(define (some-zone-matches? zs zp)
  (or (not zp) (for/or ([z (in-list zs)]) (equal? z zp))))

;; ============================================================
;; Labels

(define current-label (make-parameter #f))
