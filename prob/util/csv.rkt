;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/contract
         racket/string)
(provide (contract-out
          [csv-file->generator
           (->* [(or/c input-port? path-string?)]
                [#:header header/c #:data data-filter/c]
                procedure?)]
          [read-csv-file
           (->* [(or/c input-port? path-string?)]
                [#:header header/c #:data data-filter/c]
                list?)]
          [read-csv-line
           (->* [input-port?]
                [data-filter/c]
                list?)]
          [multiplex-generators*
           (-> (vectorof procedure?)
               (vectorof exact-nonnegative-integer?)
               procedure?)]
          [multiplex-generators
           (->* [] []
                #:rest (let ()
                         (define args/c
                           (recursive-contract
                            (or/c '()
                                  (cons/c procedure? 
                                          (cons/c exact-nonnegative-integer? 
                                                  args/c)))))
                         args/c)
                procedure?)])
         csv-data:auto
         csv-data:integer
         csv-data:try-real
         csv-data:inexact-real)

;; A Header is one of
;; - #f                       -- no header
;; - #t                       -- discard header
;; - (Vectorof String/Regexp) -- match expected header
(define header/c (or/c boolean? (vectorof (or/c string? regexp?))))

;; A Data Filter is one of
;; - (String -> Any)             -- apply to each column
;; - (Vectorof (String -> Any))  -- one converter per column
;; Some common cases:
;; - numeric
;; - double-precision (convert other strings to NaN)
;; - numeric or leave as string
(define data-filter/c (or/c procedure? (vectorof procedure?)))

;; csv-file->generator : (U InputPort PathString) -> (-> (U Vector #f))
(define (csv-file->generator in
                             #:close? [close? #f]
                             #:header [header #t]
                             #:data [data-filter #f])
  (cond [(path-string? in)
         (csv-file->generator (open-input-file in #:mode 'text)
                              #:close? #t
                              #:header header
                              #:data data-filter)]
        [else
         (when header
           (read/check-header 'csv-file->generator in header close?))
         (lambda ()
           (or (read-csv-line in data-filter)
               (begin (when close? (close-input-port in)) #f)))]))

(define (read/check-header who in expected close-on-err?)
  (define header (read-csv-line in))
  (unless header
    (when close-on-err? (close-input-port in))
    (error who "header missing due to end of file\n  file: ~e" in))
  (when (vector? expected)
    (unless (= (vector-length header) (vector-length expected))
      (when close-on-err? (close-input-port in))
      (error who "header has wrong number of columns\n  header: ~e\n  expected: ~e"
             header expected))
    (for ([hi (in-vector header)] [expi (in-vector expected)] [i (in-naturals)])
      (define (bad)
        (when close-on-err? (close-input-port in))
        (error who
               (string-append "header does not match expected header"
                              "\n  column: ~s\n  header: ~e\n  expected: ~e")
               i hi expi))
      (cond [(string? expi)
             (unless (equal? hi expi) (bad))]
            [(regexp? expi)
             (unless (regexp-match? hi expi) (bad))]))))

;; read-csv-file : (U InputPort PathString) -> (Listof Vector)
(define (read-csv-file in
                       #:header [header #t]
                       #:data [data-filter #f])
  (cond [(path-string? in)
         (call-with-input-file in
           (lambda (p) (read-csv-file p #:header header #:data data-filter))
           #:mode 'text)]
        [header
         (cons (read/check-header 'read-csv-file in header #f)
               (read-csv-file in #:header #f #:data data-filter))]
        [else
         (let loop ([acc null])
           (let ([next (read-csv-line in data-filter)])
             (cond [next
                    ;; data-filter
                    (loop (cons next acc))]
                   [else (reverse acc)])))]))

;; read-csv-line : InputPort -> (U Vector #f)
(define (read-csv-line in [data-filter #f])
  (define line (read-line in 'any))
  (cond [(eof-object? line)
         #f]
        [else
         (define parts (string-split line #rx"," #:trim? #f))
         (define v (list->vector parts))
         (apply-data-filter (or data-filter csv-data:auto) v)]))

(define (apply-data-filter data-filter v)
  (cond [(vector? data-filter)
         (unless (= (vector-length v) (vector-length data-filter))
           (error 'read-csv-line
                  "line has wrong number of elements\n  expected: ~s\n  line: ~e"
                  (vector-length data-filter)
                  v))
         (for ([vi (in-vector v)] [dfi (in-vector data-filter)] [i (in-naturals)])
           (vector-set! v i (dfi vi)))]
        [else
         (for ([vi (in-vector v)] [i (in-naturals)])
           (vector-set! v i (data-filter vi)))])
  v)

;; ----

(define (csv-data:auto s)
  (cond [(regexp-match #rx"^\"(.*)\"$" s)
         => (lambda (m) (cadr m))]
        [(string->number s)
         => (lambda (n) (if (rational? n) n s))]
        [else s]))

(define (csv-data:integer s)
  (define n (string->number s))
  (if (exact-integer? n)
      n
      (error 'csv-data:strict-integer "not an integer\n  string: ~e" s)))

(define (csv-data:try-real s)
  (define n (string->number s))
  (if (real? n) n s))

(define (csv-data:inexact-real s)
  (define n (string->number s))
  (if (real? n) (exact->inexact n) +nan.0))

;; ----------------------------------------

;; Multiplexing generators

;; Given generators g1..gN, column indexes c1...CN, multiplex into
;; generator that produces
;;   (vector t v1 ... vN)
;; where for all i, either vi is #f or vi is vector s.t. vi[ci] = t.
;; Each generator gi at ci must be real-valued and strictly monotonic
;; increasing (no duplicates!).

;; multiplex-generators : {Generator Nat} ... -> Generator
(define (multiplex-generators . args0)
  (let loop ([gens null] [indexes null] [args args0])
    (cond [(null? args)
           (multiplex-generators* (list->vector (reverse gens))
                                  (list->vector (reverse indexes)))]
          [(null? (cdr args))
           (error 'multiplex-generators
                  "expected even number of arguments\n  given: ~e" args)]
          [else
           (loop (cons (car args) gens)
                 (cons (cadr args) indexes)
                 (cddr args))])))

(define (multiplex-generators* generators column-indexes)
  (define len (vector-length generators))
  (define nexts (make-vector len #f))
  (for ([i (in-range len)] [gi (in-vector generators)])
    (vector-set! nexts i (gi)))
  (lambda ()
    (define t (vector-min/indexes nexts column-indexes))
    (cond [t
           (define r (make-vector (add1 len) #f))
           (vector-set! r 0 t)
           (for ([i (in-range len)]
                 [nexti (in-vector nexts)]
                 [ci (in-vector column-indexes)]
                 [gi (in-vector generators)]
                 #:when (and (vector? nexti)
                             (= (vector-ref nexti ci) t)))
             (vector-set! r (add1 i) nexti)
             (vector-set! nexts i (gi)))
           r]
          [else #f])))

(define (vector-min/indexes vs cs)
  (for/fold ([t #f])
            ([vi (in-vector vs)]
             [ci (in-vector cs)]
             #:when (vector? vi))
    (define vici (vector-ref vi ci))
    (if t (min t vici) vici)))
