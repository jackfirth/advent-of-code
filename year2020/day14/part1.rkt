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

(define word-size 36)
(define empty-mask (make-string word-size #\X))

(define-tuple-type execution-state (mask memory))

(define initial-execution-state (execution-state empty-mask empty-hash))

(define-tuple-type mask-instruction (new-mask))
(define-tuple-type write-instruction (address value))

(define/guard (parse-instruction line)
  (regexp-define (mask) #px"mask = ([X10]+)" line then (mask-instruction mask))
  (regexp-define (address value) #px"mem\\[(\\d+)\\] = (\\d+)" line)
  (write-instruction (string->number address) (string->number value)))

(define (pad-bit-list bits min-length)
  (define num-padding-zeros (- min-length (list-size bits)))
  (append (make-list num-padding-zeros 0) bits))

(define (unsigned-integer->bitstring n #:min-length [min-length 1])
  (define/guard (loop [bit-list empty-list] [n n])
    (guard (zero? n) then (apply bitstring (pad-bit-list bit-list min-length)))
    (define-values (q r) (quotient/remainder n 2))
    (loop (list-insert bit-list r) q))
  (loop))

(define (bitstring->unsigned-integer bits)
  (for/fold ([n 0]) ([b (in-bitstring bits)])
    (+ (* n 2) b)))

(define (apply-mask-to-bitstring mask bits)
  (define masked-bits
    (for/list ([b (in-bitstring bits)] [m (in-string mask)])
      (match m
        [#\X b]
        [#\1 1]
        [#\0 0])))
  (apply bitstring masked-bits))

(define (apply-mask-to-unsigned-integer mask n)
  (bitstring->unsigned-integer
   (apply-mask-to-bitstring mask (unsigned-integer->bitstring n #:min-length word-size))))

(module+ test
  (test-case (name-string apply-mask-to-unsigned-integer)
    (for ([n (in-range 0 1000)])
      (check-equal? (apply-mask-to-unsigned-integer empty-mask n) n)))

  (test-case "example1"
    (define input-bits
      (bitstring 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1))
    (define expected-input-value 11)
    (define mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    (define expected-bits
      (bitstring 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 1))
    (define expected-result 73)
    (check-equal? (apply-mask-to-bitstring mask input-bits) expected-bits)
    (check-equal? (bitstring->unsigned-integer input-bits) expected-input-value)
    (check-equal?
     (unsigned-integer->bitstring expected-input-value #:min-length word-size) input-bits)
    (check-equal? (bitstring->unsigned-integer expected-bits) expected-result)
    (check-equal? (unsigned-integer->bitstring expected-result #:min-length word-size) expected-bits)
    (check-equal? (apply-mask-to-unsigned-integer mask expected-input-value) expected-result)))

(define (execute-instruction state instruction)
  (match-define (execution-state mask memory) state)
  (match instruction
    [(mask-instruction new-mask) (execution-state new-mask memory)]
    [(write-instruction address value)
     (define masked-value (apply-mask-to-unsigned-integer mask value))
     (define updated-memory (hash-set memory address masked-value))
     (execution-state mask updated-memory)]))

(define into-program-execution (make-fold-reducer execute-instruction initial-execution-state))
(define (execute-program program) (transduce program #:into into-program-execution))

(module+ test
  (test-case (name-string execute-program)
    (define program
      (list
       (mask-instruction "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
       (write-instruction 8 11)
       (write-instruction 7 101)
       (write-instruction 8 0)))
    (define expected-memory (hash 7 101 8 64))
    (define expected-state (execution-state "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" expected-memory))
    (check-equal? (execute-program program) expected-state)))

(module+ main
  (define final-state
    (transduce (file->lines input) (mapping parse-instruction) #:into into-program-execution))
  (transduce (in-hash-values (execution-state-memory final-state)) #:into into-sum))
