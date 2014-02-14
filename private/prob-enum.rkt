#lang racket/base
(require racket/match
         math/distributions
         racket/control
         "prob.rkt")
(provide enumeration-query
         enum-ERP)

;; Limitation: all paths must terminate (not just "terminate with probability 1").

;; A (EnumDist A) is one of
;; - (only A)
;; - (split (listof (list Prob (EnumDist A))))
(struct only (answer))
(struct split (subs))

(define (enumeration-query thunk)
  (parameterize ((current-ERP enum-ERP))
    (flatten-enum-dist (reset (only (thunk))))))

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

(define (enum-ERP tag _sampler get-dist)
  (let* ([dist (get-dist)]
         [vals (discrete-dist-values dist)]
         [probs (discrete-dist-probs dist)])
    (shift k
      (split (for/list ([val (in-list vals)]
                        [prob (in-list probs)])
               (list prob (reset (k val))))))))
