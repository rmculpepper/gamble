;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

;; Language with call-site instrumentation
;; Suitable for writing libraries that need call-site instrumentation, like
;; the ho-functions.rkt library.

#lang racket/base
(require "instrument.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin #%top-interaction)
         (rename-out [instrumenting-module-begin #%module-begin]
                     [instrumenting-top-interaction #%top-interaction]))
