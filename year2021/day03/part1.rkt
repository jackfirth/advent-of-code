#lang racket


(require advent-of-code/private/regexp-parsing
         racket/runtime-path
         rebellion/binary/bitstring
         rebellion/collection/multiset
         rebellion/collection/vector
         rebellion/streaming/transducer
         rebellion/type/tuple)


;@----------------------------------------------------------------------------------------------------


(define-runtime-path input "input.txt")


(define (bitstring->unsigned-integer bits)
  (for/fold ([i 0])
            ([b (in-bitstring bits)])
    (+ b (* i 2))))


(define (solve bitstrings)
  (define bit-width (bitstring-size (vector-ref bitstrings 0)))
  (define bits-by-position
    (for/vector ([i (in-range 0 bit-width)])
      (for/multiset ([bitstring (in-vector bitstrings)])
        (bitstring-ref bitstring i))))
  (define gamma
    (bitstring->unsigned-integer
     (for/bitstring ([bits (in-vector bits-by-position)])
       (if (> (multiset-frequency bits 1) (multiset-frequency bits 0)) 1 0))))
  (define epsilon
    (bitstring->unsigned-integer
     (for/bitstring ([bits (in-vector bits-by-position)])
       (if (< (multiset-frequency bits 1) (multiset-frequency bits 0)) 1 0))))
  (* gamma epsilon))


(define (string->bitstring str)
  (transduce str (mapping (λ (ch) (if (equal? ch #\0) 0 1))) #:into into-bitstring))


(module+ main
  (define bitstrings
    (transduce (file->lines input)
               (mapping string->bitstring)
               #:into (into-vector)))
  (solve bitstrings))
