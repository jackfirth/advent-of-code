#lang racket


(require advent-of-code/private/regexp-parsing
         racket/runtime-path
         rebellion/binary/bitstring
         rebellion/collection/multiset
         rebellion/collection/vector
         rebellion/streaming/transducer
         rebellion/type/tuple)


(module+ test
  (require rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(define-runtime-path input "input.txt")


(define (bitstring->unsigned-integer bits)
  (for/fold ([i 0])
            ([b (in-bitstring bits)])
    (+ b (* i 2))))


(define (oxygen-rating bitstrings [i 0])
  (define bits
    (for/multiset ([bitstring (in-vector bitstrings)])
      (bitstring-ref bitstring i)))
  (define most-common
    (if (>= (multiset-frequency bits 1) (multiset-frequency bits 0)) 1 0))
  (define filtered
    (for/vector ([bitstring (in-vector bitstrings)]
                 #:when (equal? (bitstring-ref bitstring i) most-common))
      bitstring))
  (if (equal? (vector-length filtered) 1)
      (bitstring->unsigned-integer (vector-ref filtered 0))
      (oxygen-rating filtered (add1 i))))


(define (co2-rating bitstrings [i 0])
  (define bits
    (for/multiset ([bitstring (in-vector bitstrings)])
      (bitstring-ref bitstring i)))
  (define least-common
    (if (>= (multiset-frequency bits 1) (multiset-frequency bits 0)) 0 1))
  (define filtered
    (for/vector ([bitstring (in-vector bitstrings)]
                 #:when (equal? (bitstring-ref bitstring i) least-common))
      bitstring))
  (if (equal? (vector-length filtered) 1)
      (bitstring->unsigned-integer (vector-ref filtered 0))
      (co2-rating filtered (add1 i))))
      


(define (string->bitstring str)
  (transduce str (mapping (λ (ch) (if (equal? ch #\0) 0 1))) #:into into-bitstring))


(module+ test

  (define test-input
    (vector (bitstring 0 0 1 0 0)
            (bitstring 1 1 1 1 0)
            (bitstring 1 0 1 1 0)
            (bitstring 1 0 1 1 1)
            (bitstring 1 0 1 0 1)
            (bitstring 0 1 1 1 1)
            (bitstring 0 0 1 1 1)
            (bitstring 1 1 1 0 0)
            (bitstring 1 0 0 0 0)
            (bitstring 1 1 0 0 1)
            (bitstring 0 0 0 1 0)
            (bitstring 0 1 0 1 0)))

  (test-case (name-string oxygen-rating)
    (check-equal? (oxygen-rating test-input) 23))

  (test-case (name-string co2-rating)
    (check-equal? (co2-rating test-input) 10)))


(module+ main
  (define bitstrings
    (transduce (file->lines input)
               (mapping string->bitstring)
               #:into (into-vector)))
  (* (oxygen-rating bitstrings) (co2-rating bitstrings)))
