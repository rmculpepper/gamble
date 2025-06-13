#lang info

;; pkg info

(define collection "gamble")
(define deps
  '("base"
    "gamble-lib"
    "rackunit-lib"
    "typed-racket-lib"
    "scribble-lib"))
(define build-deps '("racket-doc"
                     "math-doc"
                     "plot-doc"))
(define pkg-authors '(ryanc))

;; collection info

(define name "gamble")
(define scribblings '(("scribblings/gamble.scrbl" (multi-page))))
