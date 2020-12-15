#lang racket/base

(require (submod advent-of-code/private/grid-set no-contracts)
         (submod advent-of-code/private/grid-space no-contracts)
         advent-of-code/private/congruence
         advent-of-code/private/regexp-parsing
         fancy-app
         racket/file
         racket/function
         racket/list
         racket/match
         racket/pretty
         racket/runtime-path
         racket/sequence
         racket/set
         racket/stream
         racket/string
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/binary/bitstring
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
         rebellion/type/record
         rebellion/type/singleton
         rebellion/type/tuple
         rebellion/type/wrapper)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define-record-type game-state
  (current-turn
   unspoken-starting-numbers
   latest-appearances
   last-spoken-number
   last-spoken-number-previous-appearance))

(define (read-starting-numbers)
  (transduce (string-split (file->string input) ",") (mapping string->number) #:into (into-vector)))

(define (initial-game-state starting-numbers)
  (game-state
   #:current-turn 1
   #:unspoken-starting-numbers (sequence->list starting-numbers)
   #:latest-appearances (make-hash)
   #:last-spoken-number #false
   #:last-spoken-number-previous-appearance #false))

(define/guard (next-game-state state)
  (match-define
    (game-state
     #:current-turn turn
     #:unspoken-starting-numbers starting-numbers
     #:latest-appearances appearances
     #:last-spoken-number last-number
     #:last-spoken-number-previous-appearance previous-appearance)
    state)
  (guard-match (cons next-number remaining-starting-numbers) starting-numbers then
    (hash-set! appearances next-number turn)
    (game-state
     #:current-turn (add1 turn)
     #:unspoken-starting-numbers remaining-starting-numbers
     #:latest-appearances appearances
     #:last-spoken-number next-number
     #:last-spoken-number-previous-appearance #false))
  (guard previous-appearance else
    (define previous-appearance (hash-ref appearances 0))
    (hash-set! appearances 0 turn)
    (game-state
     #:current-turn (add1 turn)
     #:unspoken-starting-numbers empty-list
     #:latest-appearances appearances
     #:last-spoken-number 0
     #:last-spoken-number-previous-appearance previous-appearance))
  (define num-turns-since-latest-appearance
    (- turn previous-appearance 1))
  (define num-since-latest-previous-appearance
    (hash-ref appearances num-turns-since-latest-appearance #false))
  (hash-set! appearances num-turns-since-latest-appearance turn)
  (game-state
   #:current-turn (add1 turn)
   #:unspoken-starting-numbers empty-list
   #:latest-appearances appearances
   #:last-spoken-number num-turns-since-latest-appearance
   #:last-spoken-number-previous-appearance num-since-latest-previous-appearance))

(define (nth-game-state starting-numbers turn-count)
  (for/fold ([state (initial-game-state starting-numbers)]) ([_ (in-range 0 turn-count)])
    (next-game-state state)))

(module+ test
  (test-case "example"
    (define state (nth-game-state (list 0 3 6) 10))
    (check-equal? (game-state-last-spoken-number state) 0)))

(module+ main
  (game-state-last-spoken-number (nth-game-state (read-starting-numbers) 30000000)))
