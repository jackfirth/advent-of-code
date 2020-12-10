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

(define input-numbers
  (transduce (file->lines input) (mapping string->number) (sorting) #:into into-list))

(define device-joltage (+ (present-value (transduce input-numbers #:into (into-max))) 3))

(define (possible-joltages latest-joltage remaining-adaptors)
  (transduce remaining-adaptors
             (filtering (<= latest-joltage _))
             (taking-while (<= _ (+ latest-joltage 3)))
             #:into into-list))

(define/guard (build-joltage-chain [chain-so-far empty-list]
                                   [latest-joltage 0]
                                   [remaining-adaptors input-numbers])
  (guard (empty-list? remaining-adaptors) then (list-reverse chain-so-far))
  (define choices (possible-joltages latest-joltage remaining-adaptors))
  (for/or ([choice (in-list choices)])
    (build-joltage-chain (cons choice chain-so-far) choice (remove choice remaining-adaptors))))

(define chain (build-joltage-chain))

(module+ test
  (check-equal? (length chain) (length input-numbers)))

(define chain-including-endpoints (append (list 0) chain (list device-joltage)))

(define differences
  (for/list ([lower (in-list chain-including-endpoints)]
             [higher (in-list (rest chain-including-endpoints))])
    (- higher lower)))

(define one-counts (transduce differences (filtering (equal? _ 1)) #:into into-count))
(define three-counts (transduce differences (filtering (equal? _ 3)) #:into into-count))
(* one-counts three-counts)
