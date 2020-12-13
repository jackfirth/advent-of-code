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

(define (earliest-departure bus-id)
  (departure bus-id (earliest-departure-time bus-id)))

(module+ test
  (test-case (name-string earliest-departure-time)
    (check-equal? (earliest-departure-time 3 #:min-time 100) 102)
    (check-equal? (earliest-departure-time 3 #:min-time 101) 102)
    (check-equal? (earliest-departure-time 3 #:min-time 102) 102)
    (check-equal? (earliest-departure-time 3 #:min-time 103) 105)))

(define fastest-bus
  (present-value
   (transduce (immutable-string-split (string->immutable-string bus-ids-line) ",")
              (filtering (negate (equal? _ "x")))
              (mapping string->number)
              (mapping earliest-departure)
              #:into (into-min #:key departure-time))))

(* (departure-bus-id fastest-bus)
   (- (departure-time fastest-bus) min-time))
