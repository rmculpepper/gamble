(module base '#%kernel
  (#%require (all-except typed/racket/base #%module-begin #%top-interaction))
  (#%provide (all-from typed/racket/base))

  (#%require (rename "typed-racket.rkt" module-begin #%module-begin)
             (rename "typed-racket.rkt" top-interaction #%top-interaction))
  (#%provide (rename module-begin #%module-begin)
             (rename top-interaction #%top-interaction)))
