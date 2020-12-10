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
         racket/stream
         racket/string
         rebellion/base/comparator
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
         rebellion/type/singleton
         rebellion/type/wrapper)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define adaptor-joltages
  (transduce (file->lines input) (mapping string->number) (sorting) #:into into-list))

(define device-joltage (+ (present-value (transduce adaptor-joltages #:into (into-max))) 3))

(define all-joltages (append (list 0) adaptor-joltages (list device-joltage)))

(define (ways-to-remove-n-one-volt-adaptors num-one-volt-adaptors-between-two-three-volt-adaptors)
  ;; no chains of more than 4 one-volt adaptors are present in the puzzle input I got.
  (match num-one-volt-adaptors-between-two-three-volt-adaptors
    [(== 0) 1] ; -3 0 3 - only valid configuration
    [(== 1) 1] ; -3 0 1 4 - only valid configuration
    [(== 2) 2] ; -3 0 1 2 5 - two valid configurations, 1 can be deleted or kept
    [(== 3) 4] ; -3 0 1 2 3 6 - 1, 2, both, or neither can be deleted
    [(== 4) 7])) ; -3 0 1 2 3 4 7 - 1, 2, 3, (1,2), (2,3), (1, 3), or nothing can be deleted

(define differences
  (for/list ([lower (in-list all-joltages)]
             [higher (in-list (rest all-joltages))])
    (- higher lower)))

(define batching-into-one-volt-adaptor-subchain-lengths
  (batching (into-transduced (taking-while (equal? _ 1)) #:into into-count)))

(define one-volt-adaptor-subchain-lengths
  (transduce differences
             batching-into-one-volt-adaptor-subchain-lengths
             (mapping ways-to-remove-n-one-volt-adaptors)
             #:into into-product))
