#lang racket/base
(require racket/match
         math/distributions
         racket/control
         "prob.rkt")
(provide enumerate-possibilities
         enum-ERP
         enum-mem)

;; Limitation: all paths must terminate (not just "terminate with probability 1").

;; A (EnumDist A) is one of
;; - (only A)
;; - (split (listof (list Prob (EnumDist A))))
(struct only (answer))
(struct split (subs))

(define (enumerate-possibilities thunk #:limit [limit 1e-6])
  (parameterize ((current-ERP enum-ERP)
                 (current-mem enum-mem))
    (flatten-enum-dist
     (call-with-continuation-prompt
      (lambda () (only (thunk)))
      (default-continuation-prompt-tag)
      (lambda (f)
        (f 1 limit))))))

(define (enum-ERP tag _sampler get-dist)
  (let* ([dist (get-dist)]
         [vals (discrete-dist-values dist)]
         [probs (discrete-dist-probs dist)])
    (call-with-composable-continuation
     (lambda (k)
       (abort-current-continuation
        (default-continuation-prompt-tag)
        (lambda (current-path-prob limit)
          (split (for/list ([val (in-list vals)]
                            [prob (in-list probs)]
                            #:when (> (* prob current-path-prob) limit))
                   (list prob
                         (call-with-continuation-prompt
                          (lambda () (k val))
                          (default-continuation-prompt-tag)
                          (lambda (f) (f (* prob current-path-prob) limit))))))))))))

(define (flatten-enum-dist ed)
  (define prob-table (make-hash)) ;; a => prob
  (define seen '())
  (define (add! a p)
    (unless (hash-has-key? prob-table a)
      (set! seen (cons a seen)))
    (hash-set! prob-table a (+ p (hash-ref prob-table a 0))))
  (let loop ([ed ed] [p 1])
    (match ed
      [(only a) (add! a p)]
      [(split subs)
       (for/list ([sub (in-list subs)])
         (match sub
           [(list p* ed*)
            (loop ed* (* p p*))]))]))
  (for/list ([a (in-list (reverse seen))])
    (list a (hash-ref prob-table a))))

;; ----

;; normal memoization
(define (enum-mem f)
  (let ([memo-table (make-hash)])
    (lambda args
      (hash-ref! memo-table args (lambda () (apply f args))))))

;; FIXME: how does this interact with other modes, for example if memoized function
;; returned as a value from enumeration?
