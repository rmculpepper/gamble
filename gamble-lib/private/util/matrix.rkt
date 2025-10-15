;; Copyright 2014-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang racket/base

;; ------------------------------------------------------------

(module env typed-racket/base-env/extra-env-lang

  ;; Type environment extension needed by matrix-base.rkt

  ;; Types for racket/serialize:

  (require racket/serialize
           (for-syntax (only-in typed-racket/rep/type-rep make-Name make-Opaque)))

  (begin-for-syntax
    (define -serialize-info (make-Opaque #'serialize-info?))
    (define -deserialize-info (make-Opaque #'deserialize-info?)))

  (define (make-deserialize-info* make)
    (make-deserialize-info
     (lambda args (make (list->vector args)))
     (lambda () (error 'deserialize "cycle not permitted"))))

  (type-environment
   [prop:serializable
    -Struct-Type-Property]
   ;; HACK: Specialize vector to one element, for ease of typing.
   ;; Maybe can use polydots to do better?
   [make-serialize-info
    (-poly (a b)
           (-> (-> a b)
               (Un (-Syntax -Symbol)
                   (-pair -Symbol -Module-Path-Index))
               -Boolean
               -Pathlike
               -serialize-info))]
   [make-deserialize-info*
    (-poly (a)
           (-> (-> Univ a)
               -deserialize-info))])

  (begin))

;; ------------------------------------------------------------

(module base typed/racket/base
  (require racket/match
           (for-syntax racket/base)
           (prefix-in t: math/array)
           (prefix-in t: math/matrix)
           (submod ".." env))
  (provide (struct-out ImmArray)
           (struct-out MutArray)
           Array
           Array?
           Array-contents
           Matrix
           ImmMatrix
           MutMatrix)

  ;; ----------------------------------------
  ;; Serialization

  (provide t:array-deserialize-info-v0)
  (define t:array-deserialize-info-v0
    ((inst make-deserialize-info* Array)
     (lambda (v)
       (match (cast v (Vector Boolean t:Indexes (Vectorof Real)))
         [(vector mutable? indexes contents)
          (let ([marr (t:vector->array indexes contents)])
            (if mutable?
                (MutArray marr)
                (ImmArray (t:array-map (inst values Real) marr))))]))))

  (define array-serialize-info-v0
    ((inst make-serialize-info Array (Vector Boolean t:Indexes (Vectorof Real)))
     (lambda (a)
       (define arr (Array-contents a))
       (vector (t:mutable-array? arr)
               (t:array-shape arr)
               (t:array->vector arr)))
     ;; HACK: see comments in matrix-syntax.rkt
     (cons 'array-deserialize-info-v0
           (module-path-index-join '(lib "gamble/private/matrix-syntax.rkt") #f))
     #f
     ;; FIXME:
     (current-directory)))

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

  (: print-imm : Any Output-Port (U #t #f 0 1) -> Any)
  (define print-imm
    (lambda (imm out mode)
      (print-recur (ImmArray-contents (cast imm ImmArray)) out mode)))

  (: print-mut : Any Output-Port (U #t #f 0 1) -> Any)
  (define print-mut
    (lambda (mut out mode)
      (print-recur (MutArray-contents (cast mut MutArray)) out mode)))

  (: print-recur : Any Output-Port (U #t #f 0 1) -> Any)
  (define (print-recur v out mode)
    (cond [(not mode) (display v out)]
          [(integer? mode) (print v out mode)]
          [else (write v out)]))

  ;; ----------------------------------------

  (struct: ImmArray ([contents : (t:Array Real)])
    #:transparent
    #:property prop:serializable array-serialize-info-v0
    ;; #:property prop:custom-write print-imm
    #:property prop:custom-print-quotable 'never)
  (struct: MutArray ([contents : (t:Mutable-Array Real)])
    #:transparent
    #:property prop:serializable array-serialize-info-v0
    ;; #:property prop:custom-write print-mut
    #:property prop:custom-print-quotable 'never)
  (define-type Array (U ImmArray MutArray))

  (define (Array? x)
    (or (ImmArray? x) (MutArray? x)))

  (: Array-contents : (U ImmArray MutArray) -> (t:Array Real))
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
           racket/math)
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
    (unless (and (square-matrix? m)
                 (= 1 (square-matrix-size m)))
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

  (: array-sqrt/nan : (Array Real) -> (Array Real))
  (define (array-sqrt/nan a)
    (array-map sqrt/nan a))

  (: array-sqrt/err : (Array Real) -> (Array Real))
  (define (array-sqrt/err a)
    (array-map sqrt/err a))

  (: sqrt/nan : Real -> Real)
  (define (sqrt/nan x)
    (if (negative? x) +nan.0 (sqrt x)))

  (: sqrt/err : Real -> Real)
  (define (sqrt/err x)
    (if (negative? x)
        (error 'array-sqrt/err "got negative number: ~e" x)
        (sqrt x)))

  ;; ----------------------------------------

  (: matrix-cholesky : (Matrix Real) -> (Matrix Real))
  (define (matrix-cholesky A)
    (unless (matrix-symmetric? A)
      (error 'matrix-cholesky "expected symmetric matrix\n  given: ~e" A))
    ;; check square, symmetric
    ;; FIXME: quick check: diagonal?
    (define n (square-matrix-size A))
    (define L ((inst array->mutable-array Real) (make-matrix n n 0.0)))
    (define (real-sqrt [x : Real])
      (define r (sqrt x))
      (if (real? r)
          r
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

  (: matrix-ldl : (Matrix Real) -> (Values (Matrix Real) (Vectorof Real)))
  (define (matrix-ldl A)
    (define n (square-matrix-size A))
    (define L ((inst array->mutable-array Real) (make-matrix n n 0.0)))
    (define D ((inst make-vector Real) n 0.0))
    (for ([j (in-range n)])
      (vector-set!
       D j
       (- (matrix-ref A j j)
          (for/sum : Real ([k (in-range j)])
            (* (sqr (matrix-ref L j k)) (vector-ref D k)))))
      (for ([i (in-range (add1 j) n)])
        (matrix-set!
         L i j
         (/ (- (matrix-ref A i j)
               (for/sum : Real ([k (in-range j)])
                 (* (matrix-ref L i k) (matrix-ref L j k) (vector-ref D k))))
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
    (ImmArray (t:array elts : Real)))
  (define-syntax-rule (mutable-array elts)
    (MutArray (t:mutable-array elts : Real)))
  (define-syntax-rule (matrix elts)
    (ImmArray (t:matrix elts : Real)))
  (define-syntax-rule (row-matrix elts)
    (ImmArray (t:row-matrix elts : Real)))
  (define-syntax-rule (col-matrix elts)
    (ImmArray (t:col-matrix elts : Real)))

  ;; ----------------------------------------

  (begin-for-syntax
    (define-splicing-syntax-class maybe-fill
      #:attributes (fill)
      (pattern (~optional (~seq #:fill fill:expr)))))

  #|
  ;; For some reason, for/matrix and for*/matrix trigger "Macro from
  ;; typed module used in untyped code" error when wrapped the obvious
  ;; way:
  (define-syntax (for/matrix stx)
  (syntax-parse stx
  [(_ m:expr n:expr :maybe-fill (clause ...) . body)
  (template/loc stx
  (ImmArray
  (t:for/matrix: m n (?? (?@ #:fill fill)) (clause ...) : Real . body)))]))
  |#

  (begin-for-syntax
    (define (do-for/matrix who for/vector-id stx)
      (syntax-parse stx
        [(_ me:expr ne:expr :maybe-fill (clause ...) . body)
         (with-syntax ([who who] [for/vector for/vector-id])
           (syntax/loc stx
             (let* ([m me] [n ne])
               (ImmArray
                (t:vector->matrix m n
                                  (for/vector #:length (* m n) #:fill (?? fill 0) (clause ...)
                                              (let ([e (let () . body)])
                                                (unless (real? e)
                                                  (error 'who "expected real value as result of body expression\n  got: ~e" e))
                                                e)))))))])))

  (define-syntax (for/matrix stx) (do-for/matrix 'for/matrix #'for/vector stx))
  (define-syntax (for*/matrix stx) (do-for/matrix 'for*/matrix #'for*/vector stx))

  ;; ============================================================

  ;; Deserialization Info

  ;; The reason for this peculiar hack is to avoid the overhead of
  ;; deserialize dynamic-requiring a "variable" from a typed module
  ;; (matrix-base.rkt), which TR actually turns into an indirection
  ;; macro, necessitating an eval rather than a simple env lookup.

  ;; Without this hack, EACH deserialization takes ~150ms (on my laptop).
  ;; With this change, each deserialization takes ~1ms.

  (provide array-deserialize-info-v0)
  (define array-deserialize-info-v0 t:array-deserialize-info-v0)

  (begin))

;; ============================================================

;; Instantiates Array and Matrix types from math/array and math/matrix
;; at Real. Two benefits:
;; - better performance from untyped Racket (?)
;; - no polymorphic function instantiation problems from untyped Racket

(module matrix typed/racket/base
  (require (for-syntax racket/base racket/syntax syntax/parse racket/list)
           racket/math
           (prefix-in t: math/array)
           (prefix-in t: math/matrix)
           (prefix-in t: (submod ".." util))
           (submod ".." base)
           (submod ".." syntax))
  (provide (all-from-out (submod ".." base))
           (all-from-out (submod ".." syntax)))

  ;; FIXME/TODO:
  ;; - use FLArray?
  ;; - (Vectorof _) types introduce chaperones, may => slow

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

  (: wrap-ImmArray : (t:Array Real) -> ImmArray)
  (define (wrap-ImmArray a)
    #|
    ;; Don't want to unnecessarily copy every array
    (ImmArray (t:array-map (inst values Real) a))
    |#
    #|
    ;; Occurrence typing screws up here
    (cond [(t:settable-array? (values a))
    (ImmArray (t:array-map (inst values Real) a))]
    [else
    (ImmArray a)])
    |#
    ;; Workaround:
    (: identity : (t:Array Real) -> (t:Array Real))
    (define (identity x) x)
    (let ([b (identity a)])
      (cond [(t:settable-array? b)
             (ImmArray (t:array-map (inst values Real) a))]
            [else (ImmArray a)])))

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

  (Wrap make-array : In-Indexes Real -> ImmArray)
  (Wrap build-array : In-Indexes (t:Indexes -> Real) -> ImmArray)
  (Wrap array->mutable-array : Array -> MutArray)
  (Wrap mutable-array-copy : MutArray -> MutArray)
  ;; indexes-array
  ;; index-array
  ;; axis-index-array
  (Wrap diagonal-array : Integer Integer Real Real -> ImmArray)

  ;; == Section 6.8 Conversion

  (Wrap* list->array :
         [(Listof Real) -> ImmArray]
         [In-Indexes (Listof Real) -> ImmArray])
  (Wrap array->list : Array -> (Listof Real))
  (Wrap vector->array : In-Indexes (Vectorof Real) -> MutArray)
  (Wrap array->vector : Array -> (Vectorof Real))

  (provide list*->array
           vector*->array)

  (: list*->array : (t:Listof* Real) -> ImmArray)
  (define (list*->array elts)
    (ImmArray (t:list*->array elts real?)))
  (: vector*->array : (t:Vectorof* Real) -> MutArray)
  (define (vector*->array elts)
    (MutArray (t:vector*->array elts real?)))
  (Wrap array->list* : Array -> (t:Listof* Real))
  (Wrap array->vector* : Array -> (t:Vectorof* Real))

  (Wrap array-list->array : (Listof Array) Index -> Array)
  (Wrap array->array-list : Array Index -> (Listof Array))

  ;; == Section 6.9 Comprehensions and Sequences

  ;; FIXME?

  ;; == Section 6.10 Pointwise Operations

  ;; FIXME: dots ???
  (Wrap* array-map :
         [(-> Real) -> Array]
         [(Real -> Real) Array -> Array]
         [(Real Real -> Real) Array Array -> Array]
         [(Real Real Real -> Real) Array Array Array -> Array])

  ;; FIXME: unfold cases for now, since underlying is macro
  (Wrap* array+ : [-> Array] [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array* : [-> Array] [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array- : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array/ : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array-min : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])
  (Wrap* array-max : [Array -> Array] [Array Array -> Array] [Array Array Array -> Array])

  (Wrap array-scale : Array Real -> Array)

  (Wrap array-abs : Array -> Array)
  (Wrap array-sqr : Array -> Array)
  ;; (Wrap array-sqrt : Array -> Array) ;; --- May be complex!
  (Wrap array-conjugate : Array -> Array)

  ;; SKIP: ops for complex arrays <=> real arrays

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

  (Wrap array-ref : Array In-Indexes -> Real)
  (Wrap array-set! : MutArray In-Indexes Real -> Void)
  ;; array-indexes-ref, array-indexes-set!

  (Wrap array-slice-ref : Array (Listof Slice-Spec) -> Array)
  (Wrap array-slice-set! : MutArray (Listof Slice-Spec) Array -> Void)

  (provide (rename-out [t::: ::]
                       [t:slice? slice?]
                       [t:slice-start slice-start]
                       [t:slice-end slice-end]
                       [t:slice-step slice-step]
                       [t:::... ::...]
                       [t:slice-dots? slice-dots?]
                       [t:::new ::new]
                       [t:slice-new-axis? slice-new-axis?]
                       [t:slice-new-axis-length slice-new-axis-length]))

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
         [Array Integer (Real Real -> Real) -> Array]
         [Array Integer (Real Real -> Real) Real -> Array])

  (Wrap* array-axis-sum : [Array Integer -> Array] [Array Integer Real -> Array])
  (Wrap* array-axis-prod : [Array Integer -> Array] [Array Integer Real -> Array])
  (Wrap* array-axis-min : [Array Integer -> Array] [Array Integer Real -> Array])
  (Wrap* array-axis-max : [Array Integer -> Array] [Array Integer Real -> Array])

  (Wrap array-axis-count : Array Integer (Real -> Any) -> Array)
  ;; (Wrap array-fold : Array (Array Integer -> Array) -> Array) ;; FIXME: Array in ->
  (Wrap* array-all-fold :
         [Array (Real Real -> Real) -> Real]
         [Array (Real Real -> Real) Real -> Real])
  (Wrap* array-all-sum : [Array -> Real] [Array Real -> Real])
  (Wrap* array-all-prod : [Array -> Real] [Array Real -> Real])

  ;; FIXME
  (Wrap* array-count : [(Real -> Any) Array -> Integer] [(Real Real -> Any) Array Array -> Integer])
  (Wrap* array-andmap : [(Real -> Any) Array -> Any] [(Real Real -> Any) Array Array -> Any])
  (Wrap* array-ormap : [(Real -> Any) Array -> Any] [(Real Real -> Any) Array Array -> Any])

  (Wrap array-axis-reduce : Array Integer (Index (Integer -> Real) -> Real) -> Array)
  (Wrap array-axis-expand : Array Integer Integer (Real Index -> Real) -> Array)

  ;; SKIPPED list-array ops

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

  (Wrap* identity-matrix :
         [Integer -> Matrix]
         [Integer Real -> Matrix]
         [Integer Real Real -> Matrix])
  (Wrap make-matrix : Integer Integer Real -> Matrix)
  (Wrap build-matrix : Integer Integer (Index Index -> Real) -> Matrix)
  (Wrap diagonal-matrix : (Listof Real) -> Matrix)
  ;; block-diagonal-matrix
  (Wrap vandermonde-matrix : (Listof Real) Integer -> Matrix)

  ;; == Section 7.4 Conversion

  (Wrap list->matrix : Integer Integer (Listof Real) -> Matrix)
  (Wrap matrix->list : Matrix -> (Listof Real))

  (Wrap vector->matrix : Integer Integer (Vectorof Real) -> Matrix)
  (Wrap matrix->vector : Matrix -> (Vectorof Real))

  (Wrap ->row-matrix : (U (U (Listof Real) (Vectorof Real)) Array) -> Matrix)
  (Wrap ->col-matrix : (U (U (Listof Real) (Vectorof Real)) Array) -> Matrix)

  (Wrap list*->matrix : (Listof (Listof Real)) -> Matrix)
  (Wrap matrix->list* : Matrix -> (Listof (Listof Real)))

  (Wrap vector*->matrix : (Vectorof (Vectorof Real)) -> Matrix)
  (Wrap matrix->vector* : Matrix -> (Vectorof (Vectorof Real)))

  ;; == Section 7.5 Entrywise Operations and Arithmetic

  (Wrap* matrix+ : [Matrix -> Matrix] [Matrix Matrix -> Matrix] [Matrix Matrix Matrix -> Matrix])
  (Wrap* matrix- : [Matrix -> Matrix] [Matrix Matrix -> Matrix] [Matrix Matrix Matrix -> Matrix])
  (Wrap* matrix* : [Matrix -> Matrix] [Matrix Matrix -> Matrix] [Matrix Matrix Matrix -> Matrix])

  (Wrap matrix-expt : Matrix Integer -> Matrix)

  (Wrap matrix-scale : Matrix Real -> Matrix)

  (Wrap* matrix-map :
         [(Real -> Real) Matrix -> Matrix]
         [(Real Real -> Real) Matrix Matrix -> Matrix]
         [(Real Real Real -> Real) Matrix Matrix Matrix -> Matrix])

  (Wrap matrix-sum : (Listof Matrix) -> Matrix)

  (Wrap matrix= : Matrix Matrix -> Boolean)

  ;; == Section 7.6 Polymorphic Operations

  (Wrap matrix-ref : Matrix Integer Integer -> Real)
  (Wrap matrix-row : Matrix Integer -> Matrix)
  (Wrap matrix-col : Matrix Integer -> Matrix)
  (Wrap submatrix : Matrix (U t:Slice (Sequenceof Integer)) (U t:Slice (Sequenceof Integer)) -> Array)
  (Wrap matrix-diagonal : Matrix -> Array)
  (Wrap matrix-upper-triangle : Matrix -> Matrix)
  (Wrap matrix-lower-triangle : Matrix -> Matrix)

  (Wrap matrix-rows : Matrix -> (Listof Matrix))
  (Wrap matrix-cols : Matrix -> (Listof Matrix))
  (Wrap matrix-augment : (Listof Matrix) -> Matrix)
  (Wrap matrix-stack : (Listof Matrix) -> Matrix)

  ;; matrix-map-rows, matrix-map-cols

  ;; == Section 7.7 Basic Operations

  (Wrap matrix-conjugate : Matrix -> Matrix)
  (Wrap matrix-transpose : Matrix -> Matrix)
  (Wrap matrix-hermitian : Matrix -> Matrix)
  (Wrap matrix-trace : Matrix -> Real)

  ;; == Section 7.8 Inner Product Space Operations

  (Wrap matrix-1norm : Matrix -> Real)
  (Wrap matrix-2norm : Matrix -> Real)
  (Wrap matrix-inf-norm : Matrix -> Real)
  (Wrap* matrix-norm : [Matrix -> Real] [Matrix Real -> Real])

  (Wrap* matrix-dot : [Matrix -> Real] [Matrix Matrix -> Real])

  (Wrap matrix-cos-angle : Matrix Matrix -> Real)
  (Wrap matrix-angle : Matrix Matrix -> Real)

  (Wrap* matrix-normalize : [Matrix -> Matrix] [Matrix Real -> Matrix])
  (Wrap* matrix-normalize-rows : [Matrix -> Matrix] [Matrix Real -> Matrix])
  (Wrap* matrix-normalize-cols : [Matrix -> Matrix] [Matrix Real -> Matrix])

  (Wrap* matrix-rows-orthogonal? : [Matrix -> Boolean] [Matrix Real -> Boolean])
  (Wrap* matrix-cols-orthogonal? : [Matrix -> Boolean] [Matrix Real -> Boolean])

  ;; == Section 7.9 Solving Systems of Equations

  (Wrap matrix-solve : Matrix Matrix -> Matrix)
  (Wrap matrix-inverse : Matrix -> Matrix)
  (Wrap matrix-invertible? : Matrix -> Boolean)
  (Wrap matrix-determinant : Matrix -> Real)

  ;; == Section 7.10 Row-based algorithms

  (Wrap* matrix-gauss-elim :
         [Matrix -> (Values Matrix (Listof Index))]
         [Matrix Any -> (Values Matrix (Listof Index))]
         [Matrix Any Any -> (Values Matrix (Listof Index))]
         [Matrix Any Any (U 'first 'partial) -> (Values Matrix (Listof Index))])

  (Wrap* matrix-row-echelon :
         [Matrix -> Matrix]
         [Matrix Any -> Matrix]
         [Matrix Any Any -> Matrix]
         [Matrix Any Any (U 'first 'partial) -> Matrix])

  (Wrap* matrix-lu :
         [Matrix -> (Values Matrix Matrix)])

  ;; == Section 7.11 Orthogonal algorithms

  (Wrap* matrix-gram-schmidt :
         [Matrix -> Matrix]
         [Matrix Any -> Matrix]
         [Matrix Any Integer -> Matrix])

  (Wrap matrix-basis-extension : Matrix -> Matrix)

  (Wrap* matrix-qr :
         [Matrix -> (Values Matrix Matrix)]
         [Matrix Any -> (Values Matrix Matrix)])

  ;; == Section 7.12 Operator norms and comparing matrices

  (Wrap matrix-op-1norm : Matrix -> Real)
  (Wrap matrix-op-2norm : Matrix -> Real)
  (Wrap matrix-op-inf-norm : Matrix -> Real)

  (Wrap matrix-absolute-error : Matrix Matrix -> Real)
  (Wrap matrix-relative-error : Matrix Matrix -> Real)

  (Wrap* matrix-zero? : [Matrix -> Boolean] [Matrix Real -> Boolean])
  (Wrap* matrix-identity? : [Matrix -> Boolean] [Matrix Real -> Boolean])
  (Wrap* matrix-orthonormal? : [Matrix -> Boolean] [Matrix Real -> Boolean])

  ;; ============================================================

  (Wrap array->immutable-array : Array -> ImmArray)
  (Wrap matrix11->value : Matrix -> Real)
  (Wrap matrix-set! : MutMatrix Integer Integer Real -> Void)
  (Wrap matrix-symmetric? : Matrix -> Boolean)
  (Wrap matrix-cholesky : Matrix -> Matrix)
  (Wrap make-mutable-matrix : Index Index Real -> MutMatrix)
  (Wrap array-sqrt/nan : Array -> Array)
  (Wrap array-sqrt/err : Array -> Array)

  ;; ============================================================

  ;; TODO:
  ;; - matrix->vector, etc
  (begin))

;; ============================================================
