#lang racket/base

(require advent-of-code/private/regexp-parsing
         fancy-app
         racket/file
         racket/function
         racket/list
         racket/match
         racket/runtime-path
         racket/sequence
         racket/set
         racket/string
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multidict
         rebellion/collection/set
         rebellion/collection/vector
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/singleton
         rebellion/type/wrapper)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define slice-size 25)

(define (valid-slice? number-slice)
  (define target-number (vector-ref number-slice slice-size))
  (for/or ([combination (in-combinations (take (sequence->list number-slice) slice-size) 2)])
    (match-define (list x y) combination)
    (equal? (+ x y) target-number)))

(transduce (file->lines input)
           (mapping string->number)
           (windowing (add1 slice-size) #:into (into-vector #:size (add1 slice-size)))
           (filtering (negate valid-slice?))
           (mapping (vector-ref _ slice-size))
           #:into into-first)
