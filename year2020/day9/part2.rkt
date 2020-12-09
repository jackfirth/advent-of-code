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

(define slice-size 25)

(define (valid-slice? number-slice)
  (define target-number (vector-ref number-slice slice-size))
  (for/or ([combination (in-combinations (take (sequence->list number-slice) slice-size) 2)])
    (match-define (list x y) combination)
    (equal? (+ x y) target-number)))

(define all-numbers (transduce (file->lines input) (mapping string->number) #:into (into-vector)))

(define invalid-number
  (present-value
   (transduce all-numbers
              (windowing (add1 slice-size) #:into (into-vector #:size (add1 slice-size)))
              (filtering (negate valid-slice?))
              (mapping (vector-ref _ slice-size))
              #:into into-first)))

(define (subvector vec start end)
  (define subsize (- end start))
  (define subvec (make-vector subsize))
  (vector-copy! subvec 0 vec start end)
  (vector->immutable-vector subvec))

(module+ test
  (test-case (name-string subvector)
    (check-equal? (subvector (vector 1 2 3 4 5) 1 3) (vector 2 3))))

(define (in-contiguous-ranges)
  (for*/stream ([i (in-range 0 (vector-length all-numbers))]
                [j (in-range i (vector-length all-numbers))]
                #:unless (equal? i j))
    (subvector all-numbers i j)))

(define (sequence-sum seq) (for/sum ([x seq]) x))
(define (sequence-max seq) (transduce seq #:into (into-max)))
(define (sequence-min seq) (transduce seq #:into (into-min)))

(define weak-range
  (present-value
   (transduce (in-contiguous-ranges)
              (filtering (λ (range) (equal? (sequence-sum range) invalid-number)))
              #:into into-first)))

(define weak-max (present-value (sequence-max weak-range)))
(define weak-min (present-value (sequence-min weak-range)))
(+ weak-max weak-min)
