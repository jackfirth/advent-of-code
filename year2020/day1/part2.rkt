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

(transduce (in-combinations numbers 3)
           (filtering (match-lambda [(list a b c) (equal? (+ a b c) 2020)]))
           (mapping (match-lambda [(list a b c) (* a b c)]))
           #:into into-first)
