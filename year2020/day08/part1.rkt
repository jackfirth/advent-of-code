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

(define-singleton-type nop)
(define-wrapper-type jmp)
(define-wrapper-type acc)

(define (parse-instruction instruction-text)
  (regexp-define (instruction operand) #px"(nop|acc|jmp) ([+\\-]\\d+)" instruction-text)
  (match instruction
    ["nop" nop]
    ["acc" (acc (string->number operand))]
    ["jmp" (jmp (string->number operand))]))

(module+ test
  (test-case (name-string parse-instruction)
    (check-equal? (parse-instruction "nop +0") nop)
    (check-equal? (parse-instruction "jmp +7") (jmp 7))
    (check-equal? (parse-instruction "acc -3") (acc -3))))

(define instructions
  (transduce (file->lines input) (mapping parse-instruction) #:into (into-vector)))

(define/guard (execute [visited empty-set] [accumulator 0] [code-pointer 0])
  (guard (set-member? visited code-pointer) then accumulator)
  (define updated-visited (set-add visited code-pointer))
  (match (vector-ref instructions code-pointer)
    [(== nop) (execute updated-visited accumulator (add1 code-pointer))]
    [(jmp offset) (execute updated-visited accumulator (+ code-pointer offset))]
    [(acc amount) (execute updated-visited (+ accumulator amount) (add1 code-pointer))]))

(execute)
