;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base

;; ------------------------------------------------------------

(module base typed/racket/base
  (require racket/match
           (only-in racket/vector vector-map)
           (for-syntax racket/base)
           (prefix-in t: math/array)
           (prefix-in t: math/matrix))
  (require/typed scramble/struct
    [prop:auto-equal+hash Struct-Type-Property])
  (provide (struct-out ImmArray)
           (struct-out MutArray)
           Elem
           elem?
           real->elem
           list-reals->elems
           vector-reals->elems
           Array
           Array?
           Array-contents
           Matrix
           ImmMatrix
           MutMatrix)

  ;; ----------------------------------------
  ;; Printing

  ;; Wrapper printing is currently INCLUDED.

  ;; To OMIT the printing of the wrapper struct, make sure the
  ;;     #:property prop:custom-write ....
  ;; lines in the struct definitions below are UNCOMMENTED.
  ;;
  ;; To INCLUDE the printing of the wrapper struct, make sure the
  ;;     #:property prop:custom-write ....
  ;; lines in the struct definitions below are COMMENTED OUT.
  ;;
  ;; (Various factors conspire to make it obnoxious to make this
  ;; configurable more gracefully, eg at run time.)

  (: print-wrapped-array : Any Output-Port (U #t #f 0 1) -> Any)
  (define (print-wrapped-array wrapped out mode)
    (match wrapped
      [(ImmArray contents)
       (print-recur contents out mode)]
      [(MutArray contents)
       (print-recur contents out mode)]))

  (: print-recur : Any Output-Port (U #t #f 0 1) -> Any)
  (define (print-recur v out mode)
    (cond [(not mode) (display v out)]
          [(integer? mode) (print v out mode)]
          [else (write v out)]))

  ;; ----------------------------------------

  (: list-reals->elems : (Listof Real) -> (Listof Elem))
  (define (list-reals->elems xs) (map real->elem xs))

  (: vector-reals->elems : (Vectorof Real) -> (Vectorof Elem))
  (define (vector-reals->elems xs) (vector-map real->elem xs))

  #;
  (begin (define-type Elem Real)
         (define elem? real?)
         (define real->elem values))

  (begin (define-type Elem Flonum)
         (define elem? flonum?)
         (define real->elem real->double-flonum))

  (struct: ImmArray ([contents : (t:Array Elem)])
    #:property prop:auto-equal+hash #t
    #:property prop:custom-write print-wrapped-array
    #:property prop:custom-print-quotable 'never)
  (struct: MutArray ([contents : (t:Mutable-Array Elem)])
    #:property prop:auto-equal+hash #t
    #:property prop:custom-write print-wrapped-array
    #:property prop:custom-print-quotable 'never)
  (define-type Array (U ImmArray MutArray))

  (define (Array? x)
    (or (ImmArray? x) (MutArray? x)))

  (: Array-contents : (U ImmArray MutArray) -> (t:Array Elem))
  (define (Array-contents x)
    (cond [(ImmArray? x) (ImmArray-contents x)]
          [(MutArray? x) (MutArray-contents x)]))

  (define-type ImmMatrix ImmArray)
  (define-type MutMatrix MutArray)
  (define-type Matrix Array)

  (begin))

;; ------------------------------------------------------------

(module util typed/racket/base
  (require math/array
           math/matrix
           racket/math
           (only-in (submod ".." base) Elem elem? real->elem))
  (provide matrix11->value
           array->immutable-array
           make-mutable-matrix
           matrix-set!
           matrix-symmetric?
           array-sqrt/nan
           array-sqrt/err
           matrix-cholesky
           matrix-ldl)

  (: matrix11->value : (All (A) (Matrix A) -> A))
  (define (matrix11->value m)
    (unless (equal? (array-shape m) '#(1 1))
      (error 'matrix11->value "expected 1 by 1 matrix\n  given: ~e" m))
    (matrix-ref m 0 0))

  (: array->immutable-array : (All (A) (Array A) -> (Array A)))
  (define (array->immutable-array a)
    (array-map (inst values A) a))

  (: make-mutable-matrix : (All (A) Index Index A -> (Mutable-Array A)))
  (define (make-mutable-matrix n m a)
    ((inst array->mutable-array A) (make-matrix n m a)))

  (: matrix-set! : (All (A) (Mutable-Array A) Integer Integer A -> Void))
  (define (matrix-set! m i j v)
    (array-set! m (vector i j) v))

  (: matrix-symmetric? : (Matrix Number) -> Boolean)
  (define (matrix-symmetric? m)
    (cond [(square-matrix? m)
           (define n (square-matrix-size m))
           (for/and : Boolean ([i (in-range n)])
             (for/and : Boolean ([j (in-range i)])
               (= (matrix-ref m i j)
                  (matrix-ref m j i))))]
          [else #f]))

  ;; ----------------------------------------

  (: array-sqrt/nan : (Array Elem) -> (Array Elem))
  (define (array-sqrt/nan a)
    (array-map sqrt/nan a))

  (: array-sqrt/err : (Array Elem) -> (Array Elem))
  (define (array-sqrt/err a)
    (array-map sqrt/err a))

  (: sqrt/nan : Elem -> Elem)
  (define (sqrt/nan x)
    (if (negative? x) +nan.0 (sqrt x)))

  (: sqrt/err : Elem -> Elem)
  (define (sqrt/err x)
    (if (negative? x)
        (error 'array-sqrt/err "element is negative: ~e" x)
        (sqrt x)))

  ;; ----------------------------------------

  (: matrix-cholesky : (Matrix Elem) -> (Matrix Elem))
  (define (matrix-cholesky A)
    (unless (matrix-symmetric? A)
      (error 'matrix-cholesky "expected symmetric matrix\n  given: ~e" A))
    ;; FIXME: quick check: diagonal?
    (define n (square-matrix-size A))
    (define L ((inst array->mutable-array Elem) (make-matrix n n 0.0)))
    (define (real-sqrt [x : Elem])
      (define r (sqrt x))
      (if (real? r)
          (real->elem r)
          (error 'matrix-cholesky "expected positive-definite matrix\n  given: ~e" A)))
    (for ([j (in-range n)])
      (for ([i (in-range j n)])
        (define Aij (matrix-ref A i j))
        (matrix-set!
         L i j
         (cond [(= i j)
                (real-sqrt (- Aij (for/sum : Real ([k (in-range j)])
                                    (sqr (matrix-ref L j k)))))]
               [else
                (/ (- Aij (for/sum : Real ([k (in-range j)])
                            (* (matrix-ref L i k) (matrix-ref L j k))))
                   (matrix-ref L j j))]))))
    L)

  (: matrix-ldl : (Matrix Elem) -> (Values (Matrix Elem) (Vectorof Elem)))
  (define (matrix-ldl A)
    (define n (square-matrix-size A))
    (define L ((inst array->mutable-array Elem) (make-matrix n n 0.0)))
    (define D ((inst make-vector Elem) n 0.0))
    (for ([j (in-range n)])
      (vector-set!
       D j
       (- (matrix-ref A j j)
          (real->elem
           (for/sum : Real ([k (in-range j)])
             (* (sqr (matrix-ref L j k)) (vector-ref D k))))))
      (for ([i (in-range (add1 j) n)])
        (matrix-set!
         L i j
         (/ (- (matrix-ref A i j)
               (real->elem
                (for/sum : Real ([k (in-range j)])
                  (* (matrix-ref L i k) (matrix-ref L j k) (vector-ref D k)))))
            (vector-ref D j)))))
    (values L D))

  (begin))

;; ------------------------------------------------------------

(module syntax racket/base
  (require (for-syntax racket/base syntax/parse)
           (prefix-in t: math/array)
           (prefix-in t: math/matrix)
           (only-in typed/racket/base :)
           (submod ".." base))
  (provide array
           mutable-array
           matrix
           row-matrix
           col-matrix
           for/matrix
           for*/matrix)

  (define-syntax-rule (array elts)
    (ImmArray (t:array elts : Elem)))
  (define-syntax-rule (mutable-array elts)
    (MutArray (t:mutable-array elts : Elem)))
  (define-syntax-rule (matrix elts)
    (ImmArray (t:matrix elts : Elem)))
  (define-syntax-rule (row-matrix elts)
    (ImmArray (t:row-matrix elts : Elem)))
  (define-syntax-rule (col-matrix elts)
    (ImmArray (t:col-matrix elts : Elem)))

  ;; ----------------------------------------

  (begin-for-syntax
    (define-splicing-syntax-class maybe-fill
      #:attributes (fill)
      (pattern (~optional (~seq #:fill fill:expr)))))

  (begin-for-syntax
    (define (do-for/matrix who for/vector-id stx)
      (syntax-parse stx
        [(_ me:expr ne:expr :maybe-fill (clause ...) . body)
         (with-syntax ([who who] [for/vector for/vector-id])
           (syntax/loc stx
             (let* ([m me] [n ne])
               (ImmArray
                (t:vector->matrix
                 m n
                 (for/vector #:length (* m n) #:fill (real->elem (?? fill 0)) (clause ...)
                             (let ([e (let () . body)])
                               (unless (real? e)
                                 (error 'who (string-append
                                              "expected real value as result of body expression"
                                              "\n  got: ~e")
                                        e))
                               (real->elem e))))))))])))

  (define-syntax (for/matrix stx) (do-for/matrix 'for/matrix #'for/vector stx))
  (define-syntax (for*/matrix stx) (do-for/matrix 'for*/matrix #'for*/vector stx))

  (begin))

;; ============================================================

;; Instantiates Array and Matrix types from math/array and math/matrix
;; at Elem. Two benefits:
;; - better performance from untyped Racket (?)
;; - no polymorphic function instantiation problems from untyped Racket

(module matrix typed/racket/base
  (require (for-syntax racket/base racket/syntax syntax/parse racket/list)
           (only-in racket/vector vector-map)
           (only-in racket/sequence sequence-map)
           racket/math
           (prefix-in t: math/array)
           (prefix-in t: math/matrix)
           (prefix-in t: (submod ".." util))
           (submod ".." base)
           (submod ".." syntax))
  (provide (all-from-out (submod ".." base))
           (all-from-out (submod ".." syntax)))

  ;; ============================================================
  ;; math/array

  (begin-for-syntax
    (define-syntax-class arraytype
      #:literals (Array Matrix ImmArray ImmMatrix MutArray MutMatrix
                        U Listof Values)
      (pattern (~or Array Matrix)
               #:with unpack #'Array-contents
               #:with repack #'wrap-ImmArray)
      (pattern (~or ImmArray ImmMatrix)
               #:with unpack #'ImmArray-contents
               #:with repack #'wrap-ImmArray)
      (pattern (~or MutArray MutMatrix)
               #:with unpack #'MutArray-contents
               #:with repack #'MutArray)
      (pattern (Listof t:arraytype)
               #:with unpack #'(lambda (l) (map t.unpack l))
               #:with repack #'(lambda (l) (map t.repack l)))
      (pattern (U t:type (~or Array Matrix))
               #:with unpack #'(lambda (v) (if (Array? v) (Array-contents v) (t.unpack v)))
               #:with repack #'(lambda (v) (if (t:array? v) (wrap-ImmArray v) (t.repack v)))))
    (define-syntax-class type
      #:attributes (unpack repack)
      (pattern :arraytype)
      (pattern (~and _:expr (~not (~datum :)) (~not (~datum ->)) (~not (~datum ...)))
               #:with unpack #'values
               #:with repack #'begin)))

  (define-syntax (Wrap* stx)
    (define-syntax-class (typeclause tfun)
      #:attributes (arity code)
      #:literals (-> Values)
      (pattern [argtype:type ... -> restype:type]
               #:attr arity (length (syntax->list #'(argtype ...)))
               #:with (arg ...) (generate-temporaries #'(argtype ...))
               #:with tfun tfun
               #:with code
               #'[([arg : argtype] ...)
                  (restype.repack (tfun (argtype.unpack arg) ...))])
      (pattern [argtype:type ... -> (Values restype:type ...)]
               #:attr arity (length (syntax->list #'(argtype ...)))
               #:with (arg ...) (generate-temporaries #'(argtype ...))
               #:with (res ...) (generate-temporaries #'(restype ...))
               #:with tfun tfun
               #:with code
               #'[([arg : argtype] ...)
                  (let-values ([(res ...) (tfun (argtype.unpack arg) ...)])
                    (values (restype.repack res) ...))])
      (pattern [argtype:type ... #:rest restargtype:type -> restype:type]
               #:attr arity (length (syntax->list #'(argtype ...))) ;; FIXME: approx
               #:with (arg ...) (generate-temporaries #'(argtype ...))
               #:with tfun tfun
               #:with code
               #'[([arg : argtype] ... . [rest : restargtype *])
                  (restype.repack
                   (apply tfun (argtype.unpack arg) ... (map restargtype.unpack rest)))]))
    (syntax-parse stx
      #:datum-literals (: ->)
      [(Wrap* fun:id : c ...)
       #:declare c (typeclause (format-id #'fun "t:~a" #'fun))
       (let ([code+arity-list (map cons (syntax->list #'(c ...)) (attribute c.arity))])
         (cond [(check-duplicates code+arity-list #:key cdr)
                => (lambda (code+arity)
                     (raise-syntax-error #f "multiple cases with same arity" stx (car code+arity)))]))
       (syntax/loc stx
         (begin
           (define fun (case-lambda c.code ...))
           (provide fun)))]))

  (define-syntax (Wrap stx)
    (syntax-parse stx
      #:datum-literals (: ->)
      [(Wrap fun:id : argtype:type ... -> restype:type)
       (with-syntax ([(arg ...) (generate-temporaries #'(argtype ...))]
                     [t:fun (format-id #'fun "t:~a" #'fun)])
         (syntax/loc stx
           (begin
             (: fun : argtype ... -> restype)
             (define (fun arg ...)
               (restype.repack (t:fun (argtype.unpack arg) ...)))
             (provide fun))))]))

  (: wrap-ImmArray : (t:Array Elem) -> ImmArray)
  (define (wrap-ImmArray a)
    (cond [(t:settable-array? (values a))
           (ImmArray (t:array-map (inst values Elem) a))]
          [else
           (ImmArray a)]))

  ;; ------------------------------------------------------------

  (define-type In-Indexes t:Indexes)
  ;; else gives contract error: or/c case overlap

  ;; ------------------------------------------------------------

  ;; == Section 6.6

  (provide array?
           settable-array?
           mutable-array?)

  (define (array? x)
    (Array? x))
  (define (settable-array? x)
    (and (MutArray? x) (t:settable-array? x)))
  (define (mutable-array? x)
    (and (MutArray? x) (t:mutable-array? x)))

  (Wrap array-shape : Array -> t:Indexes)
  (Wrap array-size  : Array -> Index)
  (Wrap array-dims  : Array -> Index)
  ;; mutable-array-data

  ;; == Section 6.7

  ;;(Wrap make-array : In-Indexes Elem -> ImmArray)
  ;;(Wrap build-array : In-Indexes (t:Indexes -> Elem) -> ImmArray)

  (provide make-array
           build-array)

  (: make-array : In-Indexes Real -> ImmArray)
  (define (make-array indexes elt)
    (ImmArray (t:make-array indexes (real->elem elt))))

  (: build-array : In-Indexes (t:Indexes -> Real) -> ImmArray)
  (define (build-array indexes f)
    (: f* : t:Indexes -> Elem)
    (define (f* indexes) (real->elem (f indexes)))
    (ImmArray (t:build-array indexes f*)))

  (Wrap array->mutable-array : Array -> MutArray)
  (Wrap mutable-array-copy : MutArray -> MutArray)
  ;; indexes-array
  ;; index-array
  ;; axis-index-array
  (Wrap diagonal-array : Integer Integer Elem Elem -> ImmArray)

  ;; == Section 6.8 Conversion

  ;; (Wrap* list->array :
  ;;        [(Listof Elem) -> ImmArray]
  ;;        [In-Indexes (Listof Elem) -> ImmArray])
  ;; (Wrap vector->array : In-Indexes (Vectorof Elem) -> MutArray)

  (: list->array : (case->
                    [(Listof Real) -> ImmArray]
                    [In-Indexes (Listof Real) -> ImmArray]))
  (define list->array
    (case-lambda
      [(elts) (ImmArray (t:list->array (list-reals->elems elts)))]
      [(indexes elts) (ImmArray (t:list->array indexes (list-reals->elems elts)))]))

  (: vector->array : In-Indexes (Vectorof Real) -> MutArray)
  (define (vector->array indexes elts)
    (MutArray (t:vector->array indexes (vector-map real->elem elts))))

  (Wrap array->list : Array -> (Listof Elem))
  (Wrap array->vector : Array -> (Vectorof Elem))

  (provide list*->array
           vector*->array)

  (: list*->array : (t:Listof* Real) -> ImmArray)
  (define (list*->array elts)
    (define real-arr (t:list*->array elts real?))
    (ImmArray (t:array-map real->elem real-arr)))
  (: vector*->array : (t:Vectorof* Real) -> ImmArray)
  (define (vector*->array elts)
    (define real-arr (t:vector*->array elts real?))
    (ImmArray (t:array-map real->elem real-arr)))

  (Wrap array->list* : Array -> (t:Listof* Elem))
  (Wrap array->vector* : Array -> (t:Vectorof* Elem))

  (Wrap array-list->array : (Listof Array) Index -> Array)
  (Wrap array->array-list : Array Index -> (Listof Array))

  ;; == Section 6.9 Comprehensions and Sequences

  (Wrap in-array : Array -> (Sequenceof Elem))
  (Wrap in-array-indexes : In-Indexes -> (Sequenceof t:Indexes))

  (provide in-array-axis)

  (: in-array-axis : (->* [Array] [Integer] (Sequenceof Array)))
  (define (in-array-axis a [axis 0])
    (sequence-map ImmArray (t:in-array-axis (Array-contents a) axis)))

  ;; == Section 6.10 Pointwise Operations

  ;; FIXME: dots ???
  (Wrap* array-map :
         [(-> Elem) -> Array]
         [(Elem -> Elem) Array -> Array]
         [(Elem Elem -> Elem) Array Array -> Array]
         [(Elem Elem Elem -> Elem) Array Array Array -> Array])

  ;; unfold cases for now, since underlying is macro
  ;; - zero-arg cases not allowed if Elem=Flonum, since they return exact 0, 1
  (Wrap* array+ : #;[-> Array] [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array* : #;[-> Array] [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array- : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array/ : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array-min : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array-max : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])

  (Wrap array-scale : Array Elem -> Array)

  (Wrap array-abs : Array -> Array)
  (Wrap array-sqr : Array -> Array)
  ;; (Wrap array-sqrt : Array -> Array) ;; --- May be complex!
  ;; array-conjugate
  ;; array-real-part
  ;; array-imag-part
  ;; array-make-rectangular
  ;; array-magnitude
  ;; array-angle
  ;; array-make-polar


  #|
  ;; FIXME: (Array Boolean)
  (Wrap* array< :  [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array<= :  [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array= :  [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array> :  [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array>= :  [Array Array -> Array] [Array Array Array -> Array])
  |#

  ;; SKIP: broadcasting meta

  (Wrap array-broadcast : Array t:Indexes -> Array)

  ;; == Section 6.11 Indexing and Slicing

  (define-type Slice-Spec
    (U (Listof Integer) (Vectorof Integer) ;; not (Sequenceof Integer), overlaps Integer
       t:Slice
       t:Slice-Dots
       Integer
       t:Slice-New-Axis))

  (Wrap array-ref : Array In-Indexes -> Elem)
  (Wrap array-set! : MutArray In-Indexes Elem -> Void)
  ;; array-indexes-ref
  ;; array-indexes-set!

  (Wrap array-slice-ref : Array (Listof Slice-Spec) -> Array)
  (Wrap array-slice-set! : MutArray (Listof Slice-Spec) Array -> Void)

  (provide (rename-out [t::: ::]
                       #;[t:slice? slice?]
                       #;[t:slice-start slice-start]
                       #;[t:slice-end slice-end]
                       #;[t:slice-step slice-step]
                       [t:::... ::...]
                       #;[t:slice-dots? slice-dots?]
                       [t:::new ::new]
                       #;[t:slice-new-axis? slice-new-axis?]
                       #;[t:slice-new-axis-length slice-new-axis-length]))

  ;; == Section 6.12 Transformations

  (Wrap array-transform : Array In-Indexes (t:Indexes -> In-Indexes) -> Array)
  (Wrap* array-append* : [(Listof Array) -> Array] [(Listof Array) Integer -> Array])

  (Wrap* array-axis-insert :
         [Array Integer -> Array]
         [Array Integer Integer -> Array])
  (Wrap array-axis-ref : Array Integer Integer -> Array)
  (Wrap array-axis-swap : Array Integer Integer -> Array)
  (Wrap array-axis-permute : Array (Listof Integer) -> Array)
  (Wrap array-reshape : Array In-Indexes -> Array)
  (Wrap array-flatten : Array -> Array)

  ;; == Section 6.13 Folds, Reductions, and Expansions

  (Wrap* array-axis-fold :
         [Array Integer (Elem Elem -> Elem) -> Array]
         [Array Integer (Elem Elem -> Elem) Elem -> Array])

  (Wrap* array-axis-sum : [Array Integer -> Array] [Array Integer Elem -> Array])
  (Wrap* array-axis-prod : [Array Integer -> Array] [Array Integer Elem -> Array])
  (Wrap* array-axis-min : [Array Integer -> Array] [Array Integer Elem -> Array])
  (Wrap* array-axis-max : [Array Integer -> Array] [Array Integer Elem -> Array])
  ;; array-axis-count
  ;; array-axis-and
  ;; array-axis-or

  (provide array-fold
           array-all-sum
           array-all-prod)

  (: array-fold : Array (Array Index -> Array) -> Array)
  (define (array-fold a f)
    (: f* : (t:Array Elem) Index -> (t:Array Elem))
    (define (f* a k) (Array-contents (f (ImmArray a) k)))
    (ImmArray (t:array-fold (Array-contents a) f*)))

  (: array-all-sum : (->* [Array] [Elem] Elem))
  (define (array-all-sum a [init (real->elem 0)])
    (t:array-all-sum (Array-contents a) init))

  (: array-all-prod : (->* [Array] [Elem] Elem))
  (define (array-all-prod a [init (real->elem 1)])
    (t:array-all-prod (Array-contents a) init))

  (: array-all-min : (->* [Array] [Elem] Elem))
  (define (array-all-min a [init +inf.0])
    (t:array-all-min (Array-contents a) init))

  (: array-all-max : (->* [Array] [Elem] Elem))
  (define (array-all-max a [init -inf.0])
    (t:array-all-max (Array-contents a) init))

  (Wrap* array-all-fold :
         [Array (Elem Elem -> Elem) -> Elem]
         [Array (Elem Elem -> Elem) Elem -> Elem])
  (Wrap array-all-and : Array -> (U Elem Boolean))
  (Wrap array-all-or : Array -> (U Elem #f))

  ;; FIXME
  (Wrap* array-count : [(Elem -> Any) Array -> Integer] [(Elem Elem -> Any) Array Array -> Integer])
  (Wrap* array-andmap : [(Elem -> Any) Array -> Any] [(Elem Elem -> Any) Array Array -> Any])
  (Wrap* array-ormap : [(Elem -> Any) Array -> Any] [(Elem Elem -> Any) Array Array -> Any])

  (Wrap array-axis-reduce : Array Integer (Index (Integer -> Elem) -> Elem) -> Array)
  (Wrap array-axis-expand : Array Integer Integer (Elem Index -> Elem) -> Array)

  ;; array->list-array
  ;; list-array->array

  ;; == Section 6.14 Other Array Operations

  ;; SKIPPED (complex)

  ;; == Section 6.15 Subtypes (Flonums, etc)

  ;; SKIPPED

  ;; == Section 6.16 Strictness

  ;; SKIPPED

  ;; ============================================================
  ;; math/matrix

  ;; == Section 7.2 Types, Predicates, and Accessors

  (provide matrix?
           col-matrix?
           row-matrix?
           square-matrix?)

  (define (matrix? x)
    (and (Array? x) (t:matrix? (Array-contents x))))
  (define (col-matrix? x)
    (and (Array? x) (t:col-matrix? (Array-contents x))))
  (define (row-matrix? x)
    (and (Array? x) (t:row-matrix? (Array-contents x))))
  (define (square-matrix? x)
    (and (Array? x) (t:square-matrix? (Array-contents x))))

  (Wrap matrix-shape : Matrix -> (Values Integer Integer))
  (Wrap matrix-num-rows : Matrix -> Index)
  (Wrap matrix-num-cols : Matrix -> Index)
  (Wrap square-matrix-size : Matrix -> Index)

  ;; == Section 7.3 Construction

  (provide identity-matrix
           make-matrix
           build-matrix
           diagonal-matrix
           block-diagonal-matrix)

  (: identity-matrix : (->* [Integer] [Real Real] Matrix))
  (define (identity-matrix n [one 1] [zero 0])
    (ImmArray (t:identity-matrix n (real->elem one) (real->elem zero))))

  (: make-matrix : Integer Integer Real -> Matrix)
  (define (make-matrix m n elt)
    (ImmArray (t:make-matrix m n (real->elem elt))))

  (: build-matrix : Integer Integer (Index Index -> Real) -> Matrix)
  (define (build-matrix m n f)
    (: f* : Index Index -> Elem)
    (define (f* i j) (real->elem (f i j)))
    (ImmArray (t:build-matrix m n f*)))

  (: diagonal-matrix : (->* [(Listof Real)] [Real] Matrix))
  (define (diagonal-matrix elts [zero 0])
    (ImmArray (t:diagonal-matrix (list-reals->elems elts) (real->elem zero))))

  (: block-diagonal-matrix : (->* [(Listof Array)] [Real] Matrix))
  (define (block-diagonal-matrix as [zero 0])
    (ImmArray (t:block-diagonal-matrix (map Array-contents as) (real->elem zero))))

  ;; vandermonde-matrix

  ;; == Section 7.4 Conversion

  (provide list->matrix
           vector->matrix
           list*->matrix
           vector*->matrix
           ->row-matrix
           ->col-matrix)

  (: list->matrix : Integer Integer (Listof Real) -> Matrix)
  (define (list->matrix m n elts)
    (ImmArray (t:list->matrix m n (list-reals->elems elts))))

  (: vector->matrix : Integer Integer (Vectorof Real) -> Matrix)
  (define (vector->matrix m n elts)
    (ImmArray (t:vector->matrix m n (vector-reals->elems elts))))

  (: list*->matrix : (Listof (Listof Real)) -> Matrix)
  (define (list*->matrix eltss)
    (ImmArray (t:list*->matrix (map list-reals->elems eltss))))

  (: vector*->matrix : (Vectorof (Vectorof Real)) -> Matrix)
  (define (vector*->matrix eltss)
    (ImmArray (t:vector*->matrix (vector-map vector-reals->elems eltss))))

  (: ->row-matrix : (U Array (Listof Real) (Vectorof Real)) -> Matrix)
  (define (->row-matrix elts)
    (cond [(Array? elts) (ImmArray (t:->row-matrix (Array-contents elts)))]
          [(vector? elts) (ImmArray (t:->row-matrix (vector-reals->elems elts)))]
          [(list? elts) (ImmArray (t:->row-matrix (list-reals->elems elts)))]))

  (: ->col-matrix : (U Array (Listof Real) (Vectorof Real)) -> Matrix)
  (define (->col-matrix elts)
    (cond [(Array? elts) (ImmArray (t:->col-matrix (Array-contents elts)))]
          [(vector? elts) (ImmArray (t:->col-matrix (vector-reals->elems elts)))]
          [(list? elts) (ImmArray (t:->col-matrix (list-reals->elems elts)))]))

  (Wrap matrix->list : Matrix -> (Listof Elem))
  (Wrap matrix->vector : Matrix -> (Vectorof Elem))
  (Wrap matrix->list* : Matrix -> (Listof (Listof Elem)))
  (Wrap matrix->vector* : Matrix -> (Vectorof (Vectorof Elem)))

  ;; == Section 7.5 Entrywise Operations and Arithmetic

  (Wrap* matrix+ : [Matrix -> Matrix] [Matrix Matrix -> Matrix] [Matrix Matrix Matrix -> Matrix])
  (Wrap* matrix- : [Matrix -> Matrix] [Matrix Matrix -> Matrix] [Matrix Matrix Matrix -> Matrix])
  (Wrap* matrix* : [Matrix -> Matrix] [Matrix Matrix -> Matrix] [Matrix Matrix Matrix -> Matrix])

  (Wrap matrix-expt : Matrix Integer -> Matrix)

  (Wrap matrix-scale : Matrix Elem -> Matrix)

  (Wrap* matrix-kronecker :
         [Matrix -> Matrix]
         [Matrix Matrix -> Matrix]
         [Matrix Matrix Matrix -> Matrix])

  (Wrap* matrix-map :
         [(Elem -> Elem) Matrix -> Matrix]
         [(Elem Elem -> Elem) Matrix Matrix -> Matrix]
         [(Elem Elem Elem -> Elem) Matrix Matrix Matrix -> Matrix])

  (Wrap matrix-sum : (Listof Matrix) -> Matrix)

  (Wrap matrix= : Matrix Matrix -> Boolean)

  ;; == Section 7.6 Polymorphic Operations

  (Wrap matrix-ref : Matrix Integer Integer -> Elem)
  (Wrap matrix-row : Matrix Integer -> Matrix)
  (Wrap matrix-col : Matrix Integer -> Matrix)
  (Wrap* submatrix : (Matrix
                      (U t:Slice (Listof Integer) (Vectorof Integer))
                      (U t:Slice (Listof Integer) (Vectorof Integer))
                      -> Array))

  (Wrap matrix-diagonal : Matrix -> Array)

  (provide matrix-upper-triangle
           matrix-lower-triangle
           matrix-map-rows
           matrix-map-cols)

  (: matrix-upper-triangle : (->* [Matrix] [Real] Matrix))
  (define (matrix-upper-triangle m [zero 0])
    (ImmArray (t:matrix-upper-triangle (Array-contents m) (real->elem zero))))

  (: matrix-lower-triangle : (->* [Matrix] [Real] Matrix))
  (define (matrix-lower-triangle m [zero 0])
    (ImmArray (t:matrix-lower-triangle (Array-contents m) (real->elem zero))))

  (Wrap matrix-rows : Matrix -> (Listof Matrix))
  (Wrap matrix-cols : Matrix -> (Listof Matrix))
  (Wrap matrix-augment : (Listof Matrix) -> Matrix)
  (Wrap matrix-stack : (Listof Matrix) -> Matrix)

  (Wrap matrix-set-row : Matrix Integer Matrix -> Matrix)
  (Wrap matrix-set-col : Matrix Integer Matrix -> Matrix)

  (: matrix-map-rows : (Matrix -> Matrix) Matrix -> Matrix)
  (define (matrix-map-rows f m)
    (: f* : (t:Matrix Elem) -> (t:Matrix Elem))
    (define (f* m) (Array-contents (f (ImmArray m))))
    (ImmArray (t:matrix-map-rows f* (Array-contents m))))

  (: matrix-map-cols : (Matrix -> Matrix) Matrix -> Matrix)
  (define (matrix-map-cols f m)
    (: f* : (t:Matrix Elem) -> (t:Matrix Elem))
    (define (f* m) (Array-contents (f (ImmArray m))))
    (ImmArray (t:matrix-map-cols f* (Array-contents m))))

  ;; == Section 7.7 Basic Operations

  ;; matrix-conjugate
  ;; matrix-hermitian

  (Wrap matrix-transpose : Matrix -> Matrix)
  (Wrap matrix-trace : Matrix -> Elem)

  ;; == Section 7.8 Inner Product Space Operations

  (Wrap matrix-1norm : Matrix -> Elem)
  (Wrap matrix-2norm : Matrix -> Elem)
  (Wrap matrix-inf-norm : Matrix -> Elem)
  (Wrap* matrix-norm : [Matrix -> Elem] [Matrix Real -> Elem])

  (Wrap* matrix-dot : [Matrix -> Elem] [Matrix Matrix -> Elem])

  (Wrap matrix-cos-angle : Matrix Matrix -> Elem)
  (Wrap matrix-angle : Matrix Matrix -> Elem)

  (Wrap* matrix-normalize : [Matrix -> Matrix] [Matrix Elem -> Matrix])
  (Wrap* matrix-normalize-rows : [Matrix -> Matrix] [Matrix Elem -> Matrix])
  (Wrap* matrix-normalize-cols : [Matrix -> Matrix] [Matrix Elem -> Matrix])

  (Wrap* matrix-rows-orthogonal? : [Matrix -> Boolean] [Matrix Elem -> Boolean])
  (Wrap* matrix-cols-orthogonal? : [Matrix -> Boolean] [Matrix Elem -> Boolean])

  ;; == Section 7.9 Solving Systems of Equations

  (Wrap matrix-solve : Matrix Matrix -> Matrix)
  (Wrap matrix-inverse : Matrix -> Matrix)
  (Wrap matrix-invertible? : Matrix -> Boolean)
  (Wrap matrix-determinant : Matrix -> Elem)

  ;; == Section 7.10 Row-based algorithms

  (provide matrix-gauss-elim
           matrix-row-echelon)

  (: matrix-gauss-elim : (->* [Matrix] [Boolean Boolean (U 'first 'partial)]
                              (Values Matrix (Listof Index))))
  (define (matrix-gauss-elim m [jordan? #f] [unitize-pivot? #f] [pivoting 'partial])
    (define-values (sm indexes)
      (t:matrix-gauss-elim (Array-contents m) jordan? unitize-pivot? pivoting))
    (values (ImmArray sm) indexes))

  (: matrix-row-echelon : (->* [Matrix] [Boolean Boolean (U 'first 'partial)] Matrix))
  (define (matrix-row-echelon m [jordan? #f] [unitize-pivot? #f] [pivoting 'partial])
    (ImmArray (t:matrix-row-echelon (Array-contents m) jordan? unitize-pivot? pivoting)))

  (Wrap* matrix-lu : [Matrix -> (Values Matrix Matrix)])

  ;; == Section 7.11 Orthogonal algorithms

  (provide matrix-gram-schmidt
           matrix-qr)

  (: matrix-gram-schmidt : (->* [Matrix] [Boolean Integer] Matrix))
  (define (matrix-gram-schmidt m [normalize? #f] [start-col 0])
    (ImmArray (t:matrix-gram-schmidt (Array-contents m) normalize? start-col)))

  (Wrap matrix-basis-extension : Matrix -> Matrix)

  (: matrix-qr : (->* [Matrix] [Boolean] (Values Matrix Matrix)))
  (define (matrix-qr m [full? #t])
    (define-values (Q R) (t:matrix-qr (Array-contents m) full?))
    (values (ImmArray Q) (ImmArray R)))

  ;; == Section 7.12 Operator norms and comparing matrices

  (Wrap matrix-op-1norm : Matrix -> Elem)
  (Wrap matrix-op-2norm : Matrix -> Elem)
  (Wrap matrix-op-inf-norm : Matrix -> Elem)

  ;;(Wrap matrix-absolute-error : Matrix Matrix -> Elem)
  ;;(Wrap matrix-relative-error : Matrix Matrix -> Elem)

  (Wrap* matrix-zero? : [Matrix -> Boolean] [Matrix Elem -> Boolean])
  (Wrap* matrix-identity? : [Matrix -> Boolean] [Matrix Elem -> Boolean])
  (Wrap* matrix-orthonormal? : [Matrix -> Boolean] [Matrix Elem -> Boolean])

  ;; ============================================================

  (Wrap array->immutable-array : Array -> ImmArray)
  (Wrap matrix11->value : Matrix -> Elem)
  (Wrap matrix-set! : MutMatrix Integer Integer Elem -> Void)
  (Wrap matrix-symmetric? : Matrix -> Boolean)
  (Wrap matrix-cholesky : Matrix -> Matrix)
  (Wrap make-mutable-matrix : Index Index Elem -> MutMatrix)
  (Wrap array-sqrt/nan : Array -> Array)
  (Wrap array-sqrt/err : Array -> Array)

  (begin))

;; ============================================================

(require (submod "." base)
         (submod "." syntax)
         (submod "." matrix))
(provide (all-from-out (submod "." base))
         (all-from-out (submod "." syntax))
         (all-from-out (submod "." matrix)))
