;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang setup/infotab

(define collection 'multi)

(define deps '(["base" #:version "6.3"]
               "data-lib"
               "math-lib"
               "gui-lib"
               "plot-lib"
               "plot-gui-lib"
               "pict-lib"
               "rackunit-lib"
               "markparam-lib"
               "compatibility-lib"
               "slideshow-lib"
               "typed-racket-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "pict-doc"
                     "plot-doc"
                     "typed-racket-doc"
                     "math-doc"))
