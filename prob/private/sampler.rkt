;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class)
(provide sampler<%>)

;; Sampler interface:

;; A samplers is an applicable object taking zero arguments. When
;; applied, it produces a single sample.
(define sampler<%>
  (interface* ()
              ([prop:procedure (lambda (this) (send this sample))])
    sample))

;; TODO: interface for weighted samplers

;; TODO: conversion from weighted samplers to ordinary sampler, using
;; uniform rejection test (need scale to be known, though)
