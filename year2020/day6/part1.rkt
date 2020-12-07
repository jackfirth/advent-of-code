#lang racket/base

(require advent-of-code/private/regexp-parsing
         racket/file
         racket/match
         racket/runtime-path
         racket/set
         racket/string
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/set
         rebellion/collection/vector
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define batching-group-answers
  (batching
   (into-transduced
    (taking-while non-empty-string?)
    (append-mapping string->list)
    (filtering char-alphabetic?)
    (deduplicating)
    #:into into-count)))

(transduce (file->lines input) batching-group-answers #:into into-sum)
