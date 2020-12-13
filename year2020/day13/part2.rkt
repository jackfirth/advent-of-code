#lang racket/base

(require (submod advent-of-code/private/grid-set no-contracts)
         (submod advent-of-code/private/grid-space no-contracts)
         advent-of-code/private/congruence
         advent-of-code/private/regexp-parsing
         fancy-app
         math/number-theory
         racket/file
         racket/function
         racket/match
         racket/runtime-path
         racket/sequence
         racket/set
         racket/stream
         racket/string
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multidict
         rebellion/collection/set
         rebellion/collection/vector
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/singleton
         rebellion/type/tuple
         rebellion/type/wrapper)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(match-define (list _ bus-ids-line) (file->lines input))

(define (entry->congruence e)
  (match-define (entry id offset) e)
  (congruence (- id offset) id))

(define (solve [bus-ids-line bus-ids-line])
  (transduce (immutable-string-split (string->immutable-string bus-ids-line) ",")
             enumerating
             (bisecting enumerated-element enumerated-position)
             (filtering-keys (negate (equal? _ "x")))
             (mapping-keys string->number)
             (mapping entry->congruence)
             #:into congruences-into-solution))

(module+ test
  (test-case (name-string solve)
    (check-equal? (solve "7,13,x,x,59,x,31,19") 1068781)))

(module+ main
  (solve))
