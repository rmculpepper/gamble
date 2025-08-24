#lang racket/base
(provide with-put-ADDR
         with-get-ADDR
         context-has-ADDR?
         addr-add-call
         addr-add-mem
         init-hash-addr
         init-full-addr
         (struct-out auto-label)
         current-init-addr)

;; Note: Label vs Addr
;; - Label is any non-false value, includes (auto-label Addr)
;; - Addr refers to addresses managed by instrumenter

(struct auto-label (addr) #:prefab)

;; Addr is one of
;; - HashAddr  -- compact, may introduce collisions
;; - FullAddr  -- addrs grow, no artificial collisions

;; HashAddr is one of
;; - Fixnum
;; - (cons (cons (list 'mem Any ...) HashAddr) Fixnum)

;; FullAddr is one of
;; - (cons Fixnum FullAddr/null)
;; - (cons (list 'mem Any ...) FullAddr)

;; addr-add-call : Addr/#f Fixnum -> Addr/#f
(define (addr-add-call addr cs)
  (cond [(eq? addr #f) #f]
        ;; HashAddr cases:
        [(fixnum? addr) (addr-fxupdate addr cs)]
        [(and #;(pair? addr) (fixnum? (cdr addr)))
         (cons (car addr) (addr-fxupdate (cdr addr) cs))]
        ;; FullAddr cases:
        [else (cons cs addr)]))

;; addr-add-mem : Addr/#f List -> Addr/#f
(define (addr-add-mem addr args)
  (cond [(eq? addr #f) #f]
        ;; HashAddr cases:
        [(fixnum? addr) (cons (cons (cons 'mem args) addr) init-fxaddr)]
        [(and #;(pair? addr) (fixnum? (cdr addr)))
         (cons (cons (cons 'mem args) addr) init-fxaddr)]
        ;; FullAddr cases:
        [else (cons (cons 'mem args) addr)]))

(define init-full-addr '(0))
(define init-hash-addr init-fxaddr)

(define current-init-addr (make-parameter init-hash-addr))

;; Addresses are passed between procedures using the "dynamic addr protocol":
;; The caller puts the address in a continuation mark frame, and the callee
;; receives the address from the top frame. Thus intermediate uninstrumented
;; frames lead to address loss.

(define ADDR-mark (string->uninterned-symbol "ADDR"))

(define-syntax-rule (with-put-ADDR addr body ...)
  (with-continuation-mark ADDR-mark addr (let () body ...)))

(define-syntax-rule (with-get-ADDR x body ...)
  (call-with-immediate-continuation-mark ADDR-mark (lambda (x) body ...)))

(define (context-has-ADDR? ctag)
  (and (ormap values (continuation-mark-set->list #f ADDR-mark ctag)) #t))

;; TODO: differentiate
;;   1. ADDR lost because of unknown context frames
;;   2. no ADDR because called out of context

;; ============================================================

(module hash-addr racket/base
  (require racket/fixnum)
  (provide init-fxaddr
           addr-fxupdate
           #;addr-fxfinal)

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

  ;; init-fxaddr : Addr
  (define init-fxaddr (fx 987654321))

  ;; addr-fxupdate : Addr Integer -> Addr
  (define (addr-fxupdate h v)
    (let* ([h (fxxor h (fh-mix (fx v)))]
           [h (fx*/wraparound h fh-m)])
      h))

  #;
  ;; addr-fxfinal : Addr -> Addr
  (define (addr-fxfinal h)
    (fh-mix h)))
(require (submod "." hash-addr))
