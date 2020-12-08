#lang racket/base

(require advent-of-code/private/regexp-parsing
         fancy-app
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

(define-wrapper-type nop)
(define-wrapper-type jmp)
(define-wrapper-type acc)

(define (parse-instruction instruction-text)
  (regexp-define (instruction operand) #px"(nop|acc|jmp) ([+\\-]\\d+)" instruction-text)
  (match instruction
    ["nop" (nop (string->number operand))]
    ["acc" (acc (string->number operand))]
    ["jmp" (jmp (string->number operand))]))

(module+ test
  (test-case (name-string parse-instruction)
    (check-equal? (parse-instruction "nop +0") (nop 0))
    (check-equal? (parse-instruction "jmp +7") (jmp 7))
    (check-equal? (parse-instruction "acc -3") (acc -3))))

(define program
  (transduce (file->lines input) (mapping parse-instruction) #:into (into-vector)))

(define jmpnop-instructions
  (transduce program
             enumerating
             (filtering (λ (e) (or (jmp? (enumerated-element e)) (nop? (enumerated-element e)))))
             (mapping enumerated-position)
             #:into (into-vector)))

(define (vector-set vec index v)
  (define copy (make-vector (vector-length vec)))
  (vector-copy! copy 0 vec)
  (vector-set! copy index v)
  (vector->immutable-vector copy))

(define (toggle-jmpnop program index)
  (match (vector-ref program index)
    [(nop operand) (vector-set program index (jmp operand))]
    [(jmp operand) (vector-set program index (nop operand))]))

(define modified-programs
  (transduce jmpnop-instructions (mapping (toggle-jmpnop program _)) #:into (into-vector)))

(define/guard (execute program [visited empty-set] [accumulator 0] [code-pointer 0])
  (guard (set-member? visited code-pointer) then #false)
  (guard (equal? code-pointer (vector-length program)) then accumulator)
  (define updated-visited (set-add visited code-pointer))
  (match (vector-ref program code-pointer)
    [(nop _) (execute program updated-visited accumulator (add1 code-pointer))]
    [(jmp offset) (execute program updated-visited accumulator (+ code-pointer offset))]
    [(acc amount) (execute program updated-visited (+ accumulator amount) (add1 code-pointer))]))

(for/or ([modified (in-vector modified-programs)]) (execute modified))
