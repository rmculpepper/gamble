#lang gamble
(require "church-compat.rkt"
         racket/list
         (only-in mzlib/etc evcase)
         gamble/viz)

;; Ported from http://forestdb.org/models/scalar-implicature.html
;; 3rd code example

;; ----------------------------------------

(define (filter pred lst)
  (fold (lambda (x y)
          (if (pred x)
              (pair x y)
              y))
        '()
        lst))

(define (my-sample-integer n)
  (discrete n))

(define (extend a b)
  (if (equal? a '())
      b
      (extend (rest a) (pair (first a) b))))

(define (flatten-nonrecursive a)
  (fold (lambda (x y) (extend x y)) '() a))

(define (generate-subsets elements)
  (if (equal? (length elements) 0)
      '()
      (let ((first-element (first elements))
            (rest-subsets (generate-subsets (rest elements))))
        (let ((new-subsets (pair (list first-element)
                                 (map (lambda (x) (pair first-element x)) rest-subsets))))
          (extend new-subsets rest-subsets)))))

(define (sample-nonempty-subset elements)
  (let ((subsets (generate-subsets elements)))
    (list-ref subsets (my-sample-integer (length subsets)))))

(define (zip a b) (map list a b))

(define (member-of e elements)
  (if (pair? elements)
      (if (equal? e (first elements))
          #t
          (member-of e (rest elements)))
      #f))

;; _______________________________________________________________________________

(define states (list 'some 'all))
;;either bob went to the restaurant alone, or mary did, or both bob and mary went

;;the speaker either knows the exact state, or knows that at least bob went, or knows that at least mary went
(define knowledge-states (list (list 'some) (list 'all) (list 'some 'all)))
(define (knowledge-prior) (multinomial knowledge-states '(1 1 1)))

(define knowledge-state-combinations
  (flatten-nonrecursive (map (lambda (x) (map (lambda (y) (list x y)) x)) knowledge-states)))

(define (sample-from-knowledge-state knowledge-state)
  (uniform-draw knowledge-state))

(define utterances '(some all))
(define (get-utterance-prob utterance)
  (case utterance
    [(some all) 1]
    [(null) 0]))
(define (utterance-prior) (multinomial utterances (map get-utterance-prob utterances)))

(define prosodies (list #f #t))
(define (get-prosody-prob prosody)
  (case prosody
    [(#f) 1]
    [(#t) 1]))
(define (prosody-prior) (multinomial prosodies (map get-prosody-prob prosodies)))

(define (noise-model utterance prosody)
  (if prosody
      (lambda (x)
        (evcase x
          [utterance 0.99]
          ['some 0.005]
          ['all 0.005]
          [else 0]))
      (lambda (x)
        (evcase x
          [utterance 0.98]
          ['some 0.01]
          ['all 0.01]
          [else 0]))))

;;sample a new utterance from the noise model
(define (sample-noise-model utterance prosody)
  (let ((noise-dist (noise-model utterance prosody)))
    (multinomial utterances (map noise-dist utterances))))

(define (literal-meaning utterance)
  (case utterance
    [(some) (list 'some 'all)]
    [(all) (list 'all)]))

(define (literal-evaluation utterance state)
  (member-of state (literal-meaning utterance)))

(define (find-state-prob listener-probs state)
  (if (null? listener-probs)
      0
      (if (equal? state (second (first (first listener-probs))))
          (second (first listener-probs))
          (find-state-prob (rest listener-probs) state))))

(define (get-expected-surprisal knowledge-state listener)
  (let ((listener (zip (first listener) (second listener))))
    (let ((relevant-listener-probs
           (filter (lambda (x) (equal? knowledge-state (first (first x))))
                   listener)))
      (let ((state-probs
             (map (lambda (x) (find-state-prob relevant-listener-probs x)) knowledge-state)))
        (sum (map (lambda (x) (* (/ 1 (length knowledge-state)) (log x))) state-probs))))))

(define (speaker-utility knowledge-state utterance prosody depth)
  (let ((utterance-dist
         (zip utterances (map (noise-model utterance prosody) utterances))))
    (let ((utterance-dist
           (filter (lambda (x) (> (second x) 0)) utterance-dist)))
      (let ((listeners
             (map (lambda (x) (listener (first x) prosody (- depth 1))) utterance-dist)))
        (let ((surprisals
               (map (lambda (x) (get-expected-surprisal knowledge-state x)) listeners)))
          (sum (map (lambda (x y) (* (second x) y)) utterance-dist surprisals)))))))

(define (speaker-literal-evaluate knowledge-state utterance)
  (let ((truth-value
         (all (map (lambda (x) (literal-evaluation utterance x)) knowledge-state))))
    (if truth-value 1 0)))

(define speaker
  (mem (lambda (knowledge-state depth)
         (enumeration-query
          (define utterance (utterance-prior))
          (define prosody (prosody-prior))
          
          (list (sample-noise-model utterance prosody) prosody)
          #:when
          (factor (+ (* (- hardness 1) (log (get-utterance-prob utterance)))
                     (* (- hardness 1) (log (get-prosody-prob prosody)))
                     (* hardness (speaker-utility knowledge-state utterance prosody depth))))))))

(define listener
  (mem (lambda (utterance prosody depth)
         (enumeration-query
          (define knowledge-state (knowledge-prior))
          (define state (sample-from-knowledge-state knowledge-state))
          
          (list knowledge-state state)
          #:when
          (if (equal? depth 0)
              (let ((intended-utterance (utterance-prior)))
                (and (equal? utterance (sample-noise-model intended-utterance prosody))
                     (literal-evaluation intended-utterance state)))
              (equal? (list utterance prosody) (apply multinomial (speaker knowledge-state depth))))))))

(define hardness 2)
(map third (map (lambda (x) (second (speaker (list 'some) x))) (list 1 2 3 4 5 6 7 8 9 10)))
