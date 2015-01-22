;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Defines wrapped (monomorphic) types used by gamble/matrix.

#lang typed/racket/base
(require racket/match
         (prefix-in t: math/array)
         (prefix-in t: math/matrix)
         "matrix-type-env.rkt")
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
