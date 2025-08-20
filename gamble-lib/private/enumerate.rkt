;; Copyright 2014-2025 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/match
         racket/class
         "dist/base.rkt"
         "dist/discrete.rkt"
         "interfaces.rkt"
         "util/density.rkt"
         "util/real.rkt")
(provide enumerate)

(define (enumerate mdl)
  (define ctx (new enumerate-stochastic-ctx%))
  (define (init-thunk) (send ctx run-top mdl))
  (define-values (dh ddim)
    (let loop ([h (hash)] [ddim #f] [dn one-density] [thunk init-thunk])
      (match (thunk)
        [(done v)
         (when (and ddim (not (= ddim (density-ddim dn))))
           (error 'enumerate "invalid program; observation density dimension varies"))
         (values (hash-set h v (density+ dn (hash-ref h v #f)))
                 (or ddim (density-ddim dn)))]
        [(? list? wdn+continue-list)
         (for/fold ([h h] [ddim ddim]) ([wdn+continue (in-list wdn+continue-list)])
           (match-define (cons wdn continue) wdn+continue)
           (loop h ddim (density* wdn dn) continue))])))
  (hash->discrete-dist (for/fold ([h (hash)]) ([(v dn) (in-hash dh)])
                         (hash-set h v (density->real dn)))))

;; A (EnumTree A) is one of
;; - (done A)
;; - (listof (cons Density (-> (EnumTree A))))
(struct done (answer))

;; ============================================================

(define enumerate-stochastic-ctx%
  (class plain-stochastic-ctx%
    (inherit run-model)
    (super-new)

    (define memo-key (gensym))
    (define ctag (make-continuation-prompt-tag))

    (define/override (sample dist _id)
      (call/restore
       (lambda (k restore)
         (for/list ([(v w) (in-dist dist)])
           (cons (density w 0 #f) (lambda () (restore (lambda () (k v)))))))))

    (define/override (dscore dn)
      (call/restore
       (lambda (k restore)
         (list (cons dn (lambda () (restore (lambda () (k (void))))))))))

    (define/override (fail reason)
      (call/restore
       (lambda (k restore)
         null)))

    (define/override (run-top mdl)
      (call (hash) (lambda () (done (run-model mdl)))))

    (define/private (call memo-table thunk)
      (parameterize ((current-stochastic-ctx this))
        (with-continuation-mark memo-key (box memo-table)
          (call-with-continuation-prompt thunk ctag))))

    (define/private (call/restore proc)
      (define memo-table (unbox (continuation-mark-set-first #f memo-key)))
      (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation ctag
          (lambda () (proc k (lambda (continue) (call memo-table continue))))))
       ctag))

    (define/override (mem f)
      (define f-key (gensym))
      (define (memoized-function . args)
        (unless (continuation-prompt-available? ctag)
          (error 'mem
                 (string-append "memoized function escaped its creating context"
                                "\n  function: ~e\n  arguments: ~e\n")
                 f args))
        (define b (continuation-mark-set-first #f memo-key))
        (define key (cons f-key args))
        (cond [(hash-has-key? (unbox b) key)
               (hash-ref (unbox b) key)]
              [else
               ;; Call with creating context; may be outer enumeration!
               (define v
                 (parameterize ((current-stochastic-ctx this))
                   (apply f args)))
               ;; NOTE: outer b might be stale, if f called ERP!
               (define b (continuation-mark-set-first #f memo-key))
               (set-box! b (hash-set (unbox b) key v))
               v]))
      memoized-function)
    ))
