#lang racket/base

(require advent-of-code/private/regexp-parsing
         fancy-app
         racket/bool
         racket/file
         racket/match
         racket/runtime-path
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/tuple)

(module+ test
  (require rackunit))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define-tuple-type input-row (first-position second-position required-character password))

(define (parse-input-row row-string)
  (regexp-define (first second char password) #px"(\\d*)\\-(\\d*) (\\w): (\\w+)" row-string)
  (input-row (string->number first) (string->number second) (string->char char) password))

(module+ test
  (test-case (name-string parse-input-row)
    (define actual (parse-input-row "2-15 x: akxxcjlsafjalskxfklsax"))
    (define expected (input-row 2 15 #\x "akxxcjlsafjalskxfklsax"))
    (check-equal? actual expected)))

(define (password-satisfies-policy? row)
  (match-define (input-row first second char password) row)
  (define first-matches? (equal? (string-ref password (sub1 first)) char))
  (define second-matches? (equal? (string-ref password (sub1 second)) char))
  (xor first-matches? second-matches?))

(module+ test
  (test-case (name-string password-satisfies-policy?)
    (check-true (password-satisfies-policy? (parse-input-row "1-3 a: abcde")))
    (check-false (password-satisfies-policy? (parse-input-row "1-3 b: cdefg")))
    (check-false (password-satisfies-policy? (parse-input-row "2-9 c: ccccccccc")))))

(transduce (file->lines input)
           (mapping parse-input-row)
           (filtering password-satisfies-policy?)
           #:into into-count)
