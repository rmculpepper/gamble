#lang gamble

;; Original Venture code by Valentino Crespi
;; Ported to prob by Ryan Culpepper

;; PCG generating strings with a cerain prefix x. Cuts the generation
;; of sential forms that are too long.

;; A Grammar is Hash[Symbol => (DiscreteDist (listof Symbol))]

;; S ::= aSb     -- prob 1/2
;;    |  ε       -- prob 1/2
(define G
  (hash
   'S (discrete-dist ['(a S b) 1/2] ['() 1/2])))

;; generate : (listof Symbol) Nat (listof Symbol) -> (listof Symbol)
(define (generate sent-form limit prefix)
  (cond [(null? sent-form)
         (cond [(null? prefix) null]
               [else (fail "too short to match prefix")])]
        [(> (length sent-form) limit)
         ;;(fail "longer than limit")
         sent-form]
        [(hash-ref G (car sent-form) #f)
         ;; expand nonterminal
         => (lambda (prod-dist)
              (define expansion (sample prod-dist))
              (define new-sent-form (append expansion (cdr sent-form)))
              (generate new-sent-form limit prefix))]
        ;; First symbol in sent-form is terminal
        [(pair? prefix)
         (cond [(eq? (car sent-form) (car prefix))
                (cons (car sent-form)
                      (generate (cdr sent-form) (sub1 limit) (cdr prefix)))]
               [else
                (fail "prefix mixmatch")])]
        [else ;; done with prefix
         (cons (car sent-form)
               (generate (cdr sent-form) (sub1 limit) prefix))]))

;; --

(define (solve sentence prefix [limit (add1 (length sentence))])
  (enumerate
   (equal? (generate '(S) limit prefix)
           sentence)))

(define (solve/sample sentence prefix [limit (add1 (length sentence))])
  (rejection-sampler
   (equal? (generate '(S) limit prefix)
           sentence)))

(solve '(a a b b) '(a a))
