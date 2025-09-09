;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         "dist/base.rkt"
         "dist/discrete.rkt"
         "base.rkt"
         "util/density.rkt"
         "util/real.rkt")
(provide enumerate)

(define (enumerate mdl)
  (unless (model? mdl) (raise-argument-error 'enumerate "model?" mdl))
  (define ctx (new enumerate-stochastic-ctx%))
  (define (init-thunk) (send ctx run-top mdl))
  (define-values (dh)
    (let loop ([h (hash)] [dn one-density] [thunk init-thunk])
      (match (thunk)
        [(done v)
         (hash-set h v (density+ dn (hash-ref h v #f)))]
        [(? list? wdn+continue-list)
         (for/fold ([h h]) ([wdn+continue (in-list wdn+continue-list)])
           (match-define (cons wdn continue) wdn+continue)
           (loop h (density* wdn dn) continue))])))
  (hash->discrete-dist (for/fold ([h (hash)]) ([(v dn) (in-hash dh)])
                         (hash-set h v (density->real dn)))))

;; A (EnumTree A) is one of
;; - (done A)
;; - (listof (cons Density (-> (EnumTree A))))
(struct done (answer))

;; ============================================================

(define enumerate-stochastic-ctx%
  (class base-stochastic-ctx%
    (inherit run-model)
    (super-new)

    (define memo-key (gensym))
    (define ctag (make-continuation-prompt-tag))

    (define/override (-sample dist tag addr)
      (call/restore
       (lambda (restore)
         (for/list ([(v w) (in-dist dist)])
           (cons (density #f w) (lambda () (restore v)))))))

    (define/override (-dscore who dn)
      (if (density-zero? dn)
          (fail who)
          (call/restore
           (lambda (restore)
             (list (cons dn (lambda () (restore (void)))))))))

    (define/override (fail reason)
      (call/restore
       (lambda (restore)
         null)))

    (define/override (run-top mdl)
      (call (hash) (lambda () (done (run-model mdl #t)))))

    (define/private (call memo-table proc)
      (with-continuation-mark memo-key (box memo-table)
        (call-with-continuation-prompt proc ctag)))

    (define/private (call/restore who proc)
      (unless (continuation-prompt-available? ctag)
        (error who "used out of enumerate context"))
      (define memo-table (unbox (continuation-mark-set-first #f memo-key)))
      (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation ctag
          (lambda () (proc (lambda (v) (call memo-table (lambda () (k v))))))))
       ctag))

    (define/override (mem f addr)
      (define f-key (gensym))
      (define (memoized-function . args)
        (unless (continuation-prompt-available? ctag)
          (error 'memoized-function
                 (string-append "used out of enumerate context"
                                "\n  function: ~e\n  arguments: ~e\n")
                 f args))
        (define b (continuation-mark-set-first #f memo-key))
        (define key (cons f-key args))
        (cond [(hash-has-key? (unbox b) key)
               (hash-ref (unbox b) key)]
              [else
               ;; Call with creating context; may be outer enumeration!
               (define v (apply f args))
               ;; NOTE: outer b might be stale, if f called ERP!
               (define b (continuation-mark-set-first #f memo-key))
               (set-box! b (hash-set (unbox b) key v))
               v]))
      (procedure-reduce-arity memoized-function (procedure-arity f)))
    ))
