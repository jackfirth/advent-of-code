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

(define alphabet (list->set (string->list "qwertyuiopasdfghjklzxcvbnm")))

(define batching-group-answers
  (batching
   (into-transduced
    (taking-while non-empty-string?)
    (mapping string->list)
    (mapping list->set)
    #:into (reducer-map (make-fold-reducer set-intersect alphabet) #:range set-count))))

(transduce (file->lines input) batching-group-answers #:into into-sum)
