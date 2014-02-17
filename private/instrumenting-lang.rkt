;; Language with call-site instrumentation
;; Suitable for writing libraries that need call-site instrumentation, like
;; the ho-functions.rkt library.

#lang racket/base
(require "instrument.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin #%top-interaction)
         (rename-out [instrumenting-module-begin #%module-begin]
                     [instrumenting-top-interaction #%top-interaction]))
