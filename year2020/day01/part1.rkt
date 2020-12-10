#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/runtime-path
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define numbers
  (transduce (file->lines input) (mapping string->number) (sorting) #:into into-list))

(transduce (in-combinations numbers 2)
           (filtering (match-lambda [(list a b) (equal? (+ a b) 2020)]))
           (mapping (match-lambda [(list a b) (* a b)]))
           #:into into-first)
