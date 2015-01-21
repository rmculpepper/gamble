;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Instantiates Array and Matrix types from math/array and math/matrix
;; at Real. Two benefits:
;; - better performance from untyped Racket (?)
;; - no polymorphic function instantiation problems from untyped Racket

#lang typed/racket/base
(require (for-syntax racket/base racket/syntax syntax/parse unstable/list)
         racket/math
         (prefix-in t: math/array)
         (prefix-in t: math/matrix)
         (prefix-in t: "private/matrix-util.rkt")
         "private/instrument-data.rkt"
         "private/matrix-base.rkt"
         "private/matrix-syntax.rkt")
(provide (all-from-out "private/matrix-base.rkt")
         (all-from-out "private/matrix-syntax.rkt"))

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
       (cond [(check-duplicate code+arity-list #:key cdr)
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
  (let ([b a])
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

(Wrap array-ref : Array In-Indexes -> Real)
(Wrap array-set! : MutArray In-Indexes Real -> Void)
;; array-indexes-ref, array-indexes-set!

(Wrap array-slice-ref : Array (Listof t:Slice-Spec) -> Array)
(Wrap array-slice-set! : MutArray (Listof t:Slice-Spec) Array -> Void)

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

;; Note: implicit broadcast means inverse is not unique, really.
(declare-observation-propagator (array+ a ... _)
  array?
  (lambda (y) (array- y a ...))
  (lambda (x) 1))

;; array* not supported --- cannot scale
;; FIXME: well... why not? Just need to figure out how to distribute scale
;; when splitting later on.

(declare-observation-propagator (matrix* a _)
  (lambda (y) (and (square-matrix? a) (matrix? y)
              (= (matrix-num-rows a) (matrix-num-rows y))))
  (lambda (y) (matrix-solve a y))
  (lambda (x) (/ (abs (matrix-determinant a)))))

;; TODO:
;; - matrix->vector, etc
