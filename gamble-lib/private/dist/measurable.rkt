;; Copyright 2023 Ryan Culpepper

#lang racket/base
(require racket/match)
(provide (struct-out measurable)
         measurable:empty
         measurable:reals
         measurable-singleton
         measurable-interval
         measurable-contains?
         measurable-union
         measurable-intersect
         measurable-subtract
         ivls-contains?)

;; ------------------------------------------------------------
;; Measurable Sets

;; A Measurable is one of
;; - (measurable (Hashof Any #t) (list { InexactReal InexactReal } ...))
(struct measurable (atoms ivls) #:prefab)

(define measurable:empty '#s(measurable #hash() ()))
(define measurable:reals '#s(measurable #hash() (-inf.0 +inf.0)))

;; measurable-singleton : Any -> Measurable
(define (measurable-singleton x)
  (measurable (hash x #t) null))

;; measurable-interval : InexactReal InexactReal -> Measurable
(define (measurable-interval a b [include-a? #f] [include-b? include-a?])
  (if (< a b)
      (measurable '#hash() (list a b))
      measurable:empty))

;; measurable-contains? : Measurable Any -> Boolean
(define (measurable-contains? m v)
  (match-define (measurable atoms ivls) m)
  (or (hash-ref atoms v #f)
      (and (rational? v) (ivls-contains? v))))

(define (ivls-contains? ivls x)
  (match ivls
    ['() #f]
    [(list* a b ivls-rest)
     (or (< a x b) (ivls-contains? ivls-rest x))]))

;; ----------------------------------------

;; measurable-union : Measurable ... -> Measurable
(define measurable-union
  (case-lambda
    [() measurable:empty]
    [(m1) m1]
    [(m1 m2) (measurable-union2 m1 m2)]
    [(m1 . m2s) (foldl measurable-union2 m1 m2s)]))

;; measurable-union2 : Measurable Measurable -> Measurable
(define (measurable-union2 m1 m2)
  (match-define (measurable atoms1 ivls1) m1)
  (match-define (measurable atoms2 ivls2) m2)
  (define atoms (hashset-union atoms1 atoms2))
  (measurable-union* atoms ivls1 ivls2))

(define (hashset-union h1 h2)
  (for/fold ([h h1]) ([v (in-hash-keys h2)])
    (hash-set h v #t)))

(define (measurable-union* atoms ivls1 ivls2)
  (define (loop1 atoms ivls acc)
    (match ivls
      ['()
       (measurable atoms (reverse acc))]
      [(list* a b rest)
       (cond [(and (pair? acc) (= a (car acc)) (hash-ref atoms a #f))
              (let ([atoms (hash-remove atoms a)])
                (loop1 atoms rest (list* b (cdr acc))))]
             [else
              (loop1 atoms rest (list* b a acc))])]))
  (define (loop atoms ivls1 ivls2 acc)
    (cond [(null? ivls1) (loop1 atoms ivls2 acc)]
          [(null? ivls2) (loop1 atoms ivls1 acc)]
          [else
           (match-define (list* a1 b1 rest1) ivls1)
           (match-define (list* a2 b2 rest2) ivls2)
           (cond [(<= a1 a2) (loop* atoms b1 rest1 ivls2 (cons a1 acc))]
                 [else (loop* atoms b2 rest2 ivls1 (cons a2 acc))])]))
  (define (loop* atoms b1 rest1 ivls2 acc)
    (match ivls2
      ['()
       (loop atoms rest1 ivls2 (cons b1 acc))]
      [(list* a2 b2 rest2)
       (cond [(< b1 a2) ;; gap
              (loop atoms rest1 ivls2 (cons b1 acc))]
             [(= b1 a2) ;; gap unless in atoms...
              (cond [(hash-ref atoms b1 #f)
                     (let ([atoms (hash-remove atoms b1)])
                       (loop* atoms b2 rest2 rest1 acc))]
                    [else (loop atoms rest1 ivls2 (cons b1 acc))])]
             [(< b1 b2) ;; partial overlap
              (loop* atoms b2 rest2 rest1 acc)]
             [(= b1 b2) ;; overlap
              (loop atoms rest1 rest2 (cons b1 acc))]
             [else ;; b1 > b2, overlap
              (loop* atoms b1 rest1 rest2 acc)])]))
  (loop atoms ivls1 ivls2 null))

;; ----------------------------------------

;; measurable-intersect : Measurable Measurable ... -> Measurable
(define measurable-intersect
  (case-lambda
    [(m1) m1]
    [(m1 m2) (measurable-intersect2 m1 m2)]
    [(m1 . m2s) (foldl measurable-intersect2 m1 m2s)]))

;; measurable-intersect2 : Measurable Measurable -> Measurable
(define (measurable-intersect2 m1 m2)
  (match-define (measurable atoms1 ivls1) m1)
  (match-define (measurable atoms2 ivls2) m2)
  (define ivls (intersect-ivls ivls1 ivls2))
  (define atoms (intersect-atoms atoms1 atoms2 ivls1 ivls2))
  (measurable atoms ivls))

(define (intersect-atoms h1 h2 ivls1 ivls2)
  (for/fold ([h '#hash()])
            ([ha (in-list (list h1 h2))]
             [hb (in-list (list h2 h1))]
             [ivlsb (in-list (list ivls2 ivls1))])
    (for/fold ([h h]) ([v (in-hash-keys ha)])
      (if (or (hash-ref hb v #f)
              (and (rational? v) (ivls-contains? ivlsb v)))
          (hash-set h v #t)
          h))))

(define (intersect-ivls ivls1 ivls2)
  (define (loop ivls1 ivls2 acc)
    (cond [(or (null? ivls1) (null? ivls2))
           (reverse acc)]
          [else
           (match-define (list* a1 b1 rest1) ivls1)
           (match-define (list* a2 b2 rest2) ivls2)
           (cond [(<= a1 a2) (loop* b1 rest1 ivls2 acc)]
                 [else (loop* b2 rest2 ivls1 acc)])]))
  (define (loop* b1 rest1 ivls2 acc)
    (match ivls2
      ['()
       (loop rest1 ivls2 acc)]
      [(list* a2 b2 rest2)
       (cond [(<= b1 a2) ;; no overlap
              (loop rest1 ivls2 acc)]
             [(< b1 b2) ;; partial overlap
              (loop* b2 rest2 rest1 (list* b1 a2 acc))]
             [(= b1 b2) ;; overlap
              (loop rest1 rest2 (list* b2 a2 acc))]
             [else ;; b1 > b2, overlap
              (loop* b1 rest1 rest2 (list* b2 a2 acc))])]))
  (loop ivls1 ivls2 null))

;; ----------------------------------------

;; measurable-subtract : Measurable Measurable ... -> Measurable
(define measurable-subtract
  (case-lambda
    [(m1) m1]
    [(m1 m2) (measurable-subtract2 m1 m2)]
    [(m1 m2 . m2s) (measurable-subtract2 m1 (foldl measurable-union2 m2 m2s))]))

;; measurable-subtract2 : Measurable -> Measurable
(define (measurable-subtract2 m1 m2)
  (match-define (measurable atoms2 ivls2) m2)
  (define m1* (measurable-intersect m1 (reals-compl-ivls ivls2)))
  (subtract-atoms m1* atoms2))

(define (reals-compl-ivls ivls)
  (define points
    (for/fold ([h '#hash()]) ([point (in-list ivls)] #:when (rational? point))
      (hash-set h point #t)))
  (define (loop b+ivls)
    (match b+ivls
      [(list b)
       (cond [(= b +inf.0) '()]
             [else (list b +inf.0)])]
      [(list* b1 a2 b2+rest)
       (list* b1 a2 (loop b2+rest))]))
  (match ivls
    ['() measurable:reals]
    [(list* a b+rest)
     (cond [(= a -inf.0) (measurable points (loop b+rest))]
           [else (measurable points (list* -inf.0 a (loop b+rest)))])]))

(define (subtract-atoms m1 atoms2)
  (match-define (measurable atoms1 ivls1) m1)
  (define-values (atoms1* unsorted-ns)
    (for/fold ([atoms1 atoms1] [ns '()])
              ([n (in-hash-keys atoms2)] #:when (and (inexact-real? n) (rational? n)))
      (if (hash-ref atoms1 n #f)
          (values (hash-remove atoms1 n) ns)
          (values atoms1 (cons n ns)))))
  (define ns (sort unsorted-ns <))
  (define ivls1*
    (let loop ([ivls ivls1] [ns ns])
      (match* [ivls ns]
        [['() _] null]
        [[_ '()] ivls]
        [[(list* a b ivls-rest) (cons n ns-rest)]
         (cond [(<= n a)
                (loop ivls ns-rest)]
               [(< n b)
                (list* a n (loop (list* n b ivls-rest) ns-rest))]
               [else
                (list* a b (loop ivls-rest ns))])])))
  (measurable atoms1* ivls1*))

; ----------------------------------------

(module+ test
  (define m1 (measurable-interval 0.0 1.0))
  (define m2 (measurable-interval 2.0 4.0))
  (define m12 (measurable-union m1 m2))
  (define m3 (measurable-interval 1.0 3.0))
  (define m123 (measurable-union m12 m3))
  m123
  (define ms1 (measurable-singleton 1.0))
  (measurable-union m123 ms1)
  (begin))
