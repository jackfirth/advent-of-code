#lang racket/base

(require (submod advent-of-code/private/grid-set no-contracts)
         (submod advent-of-code/private/grid-space no-contracts)
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
         rebellion/type/singleton
         rebellion/type/tuple
         rebellion/type/wrapper)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(match-define (list min-time-line bus-ids-line) (file->lines input))

(define min-time (string->number min-time-line))

(define-tuple-type departure (bus-id time))

(define (earliest-departure-time bus-id #:min-time [min-time min-time])
  (define-values (q r) (quotient/remainder min-time bus-id))
  (cond
    [(zero? r) min-time]
    [else (* (add1 q) bus-id)]))

(define-tuple-type bus-constraint (id remainder))

(define (entry->bus-constraint e)
  (match-define (entry id offset) e)
  (bus-constraint id (modulo (- id offset) id)))

(define constraints
  (transduce (immutable-string-split (string->immutable-string bus-ids-line) ",")
             enumerating
             (bisecting enumerated-element enumerated-position)
             (filtering-keys (negate (equal? _ "x")))
             (mapping-keys string->number)
             (mapping entry->bus-constraint)
             (sorting #:key bus-constraint-id #:descending? #true)
             #:into into-list))

(define (solve-constraints [constraints constraints])
  (define id-product
    (transduce constraints (mapping bus-constraint-id) #:into into-product))

  (match-define
    (list (bus-constraint first-id first-remainder)
          (bus-constraint next-id next-remainder)
          remaining-constraints ...)
    constraints)

  (define (sieve [solution first-remainder]
                 [current-step-size first-id]
                 [next-id next-id]
                 [next-remainder next-remainder]
                 [remaining-constraints remaining-constraints])
    (printf "solution: ~a current-step-size ~a next-id ~a next-offset ~a\n"
            solution current-step-size next-id next-remainder)
    (cond
      [(equal? (modulo solution next-id) next-remainder)
       (match remaining-constraints
         [(list) solution]
         [(list (bus-constraint new-next-id new-next-remainder) new-remaining-constraints ...)
          (sieve solution
                 (* current-step-size next-id)
                 new-next-id
                 new-next-remainder
                 new-remaining-constraints)])]
      [else
       (sieve (+ solution current-step-size)
              current-step-size
              next-id
              next-remainder
              remaining-constraints)]))

  (sieve))

(module+ test
  (test-case (name-string solve-constraints)
    (test-begin
     (define cs
       (list
        (bus-constraint 5 4)
        (bus-constraint 4 3)
        (bus-constraint 3 0)))
     (check-equal? (solve-constraints cs) 39))

    (test-begin
     (define cs
       (list
        (bus-constraint 59 55)
        (bus-constraint 31 25)
        (bus-constraint 19 12)
        (bus-constraint 13 12)
        (bus-constraint 7 0)))
     (check-equal? (solve-constraints cs) 1068781))))
