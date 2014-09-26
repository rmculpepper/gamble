;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Defines wrapped (monomorphic) types used by prob/matrix.

#lang typed/racket/base
(require (prefix-in t: math/array)
         (prefix-in t: math/matrix))
(provide (struct-out ImmArray)
         (struct-out MutArray)
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
         ;; #:property prop:custom-write print-imm
         #:property prop:custom-print-quotable 'never)
(struct: MutArray ([contents : (t:Mutable-Array Real)])
         #:transparent
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
