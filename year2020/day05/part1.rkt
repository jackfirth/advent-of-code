#lang racket/base

(require advent-of-code/private/regexp-parsing
         racket/file
         racket/match
         racket/runtime-path
         racket/set
         racket/string
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define (row-id row-code)
  (for/fold ([row 0]) ([code-char (in-string row-code)])
      (match code-char [#\F (* row 2)] [#\B (add1 (* row 2))])))

(define (column-id column-code)
  (for/fold ([column 0]) ([code-char (in-string column-code)])
      (match code-char [#\L (* column 2)] [#\R (add1 (* column 2))])))

(define (seat-id boarding-pass)
  (regexp-define (row-code column-code) #px"([FB]{7})([LR]{3})" boarding-pass)
  (define row (row-id row-code))
  (define column (column-id column-code))
  (+ (* row 8) column))

(module+ test
  (test-case (name-string row-id)
    (check-equal? (row-id "FBFBBFF") 44)
    (check-equal? (row-id "BFFFBBF") 70)
    (check-equal? (row-id "FFFBBBF") 14)
    (check-equal? (row-id "BBFFBBF") 102))

  (test-case (name-string column-id)
    (check-equal? (column-id "RLR") 5)
    (check-equal? (column-id "RRR") 7)
    (check-equal? (column-id "RLL") 4))

  (test-case (name-string seat-id)
    (check-equal? (seat-id "FBFBBFFRLR") 357)
    (check-equal? (seat-id "BFFFBBFRRR") 567)
    (check-equal? (seat-id "FFFBBBFRRR") 119)
    (check-equal? (seat-id "BBFFBBFRLL") 820)))

(transduce (file->lines input)
           (mapping seat-id)
           #:into (into-max))
