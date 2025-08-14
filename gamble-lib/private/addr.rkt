#lang racket/base
(require racket/fixnum)
(provide (all-defined-out))

(define ADDR-mark (string->uninterned-symbol "ADDR"))

(define-syntax-rule (with-ADDR addr body ...)
  (with-continuation-mark ADDR-mark addr (let () body ...)))

(define-syntax-rule (with-let-ADDR x body ...)
  (call-with-immediate-continuation-mark ADDR-mark (lambda (x) body ...)))

(define-syntax-rule (addr-extend addr n)
  (and addr (addr-update addr n)))

;; TODO: differentiate
;;   1. ADDR lost because of unknown context frames
;;   2. no ADDR because called out of context

;; ----------------------------------------
;; Addr as list of call-site indexes

(module cons-addr racket/base
  (provide init-addr
           addr-update
           addr-final)
  (define init-addr null)
  (define (addr-update base n) (cons n base))
  (define (addr-final addr) addr))

;; ----------------------------------------
;; Addr as hash-code

(module hash-addr racket/base
  (require racket/fixnum)
  (provide init-addr
           addr-update
           addr-final)

  ;; Addr = NonnegativeFixnum

  (define 64-bit? (fixnum? (expt 2 33)))

  (define fixnum-mask  ;; greatest positive fixnum with bit pattern "0*1+"
    (let loop ([fuel 64] [acc 1])
      (define next (bitwise-ior 1 (arithmetic-shift acc 1)))
      (if (and (positive? fuel) (fixnum? next))
          (loop (sub1 fuel) next)
          acc)))

  (define (fx v) (if (fixnum? v) v (bitwise-and v fixnum-mask)))

  (define fh-a (fx #x2127599bf4325c37))
  (define fh-m (fx #x880355f21e6d1965))

  (define (fh-mix h)
    (define (fh-mix64 h)
      (let* ([h (fxxor h (fxrshift h 23))]
             [h (fx*/wraparound h fh-a)]
             [h (fxxor h (fxrshift h 47))])
        h))
    (define (fh-mix32 h)
      (let* ([h (fxxor h (fxrshift h 11))]
             [h (fx*/wraparound h fh-a)]
             [h (fxxor h (fxrshift h 23))])
        h))
    (if 64-bit? (fh-mix64 h) (fh-mix32 h)))

  ;; ----------------------------------------

  ;; init-addr : Addr
  (define init-addr (fx 987654321))

  ;; addr-update : Addr Integer -> Addr
  (define (addr-update h v)
    (let* ([h (fxxor h (fh-mix (fx v)))]
           [h (fx*/wraparound h fh-m)])
      h))

  ;; addr-final : Addr -> Addr
  (define (addr-final h)
    (fh-mix h)))
(require (submod "." hash-addr))
