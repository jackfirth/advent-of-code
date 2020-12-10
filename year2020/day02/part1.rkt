#lang racket/base

(require advent-of-code/private/regexp-parsing
         fancy-app
         racket/file
         racket/match
         racket/runtime-path
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record)

(module+ test
  (require rackunit))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define-record-type input-row (policy password))
(define-record-type password-policy (required-character min-occurrences max-occurrences))

(define (parse-input-row row-string)
  (regexp-define (min max char password) #px"(\\d*)\\-(\\d*) (\\w): (\\w+)" row-string)
  (input-row
   #:policy
   (password-policy
    #:required-character (string->char char)
    #:min-occurrences (string->number min)
    #:max-occurrences (string->number max))
   #:password password))

(module+ test
  (test-case (name-string parse-input-row)
    (define actual (parse-input-row "2-15 x: akxxcjlsafjalskxfklsax"))
    (define expected
      (input-row
       #:policy (password-policy #:required-character #\x #:min-occurrences 2 #:max-occurrences 15)
       #:password "akxxcjlsafjalskxfklsax"))
    (check-equal? actual expected)))

(define (password-satisfies-policy? row)
  (match-define (input-row #:policy policy #:password password) row)
  (match-define
    (password-policy #:required-character char #:min-occurrences min #:max-occurrences max)
    policy)
  (define occurrences (transduce password (filtering (equal? _ char)) #:into into-count))
  (<= min occurrences max))

(transduce (file->lines input)
           (mapping parse-input-row)
           (filtering password-satisfies-policy?)
           #:into into-count)
