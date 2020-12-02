#lang racket/base

(require fancy-app
         racket/file
         racket/match
         racket/runtime-path
         rebellion/collection/list
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         syntax/parse/define)

(module+ test
  (require rackunit))

;@--------------------------------------------------------------------------------------------------

(define-simple-macro (regexp-define (id:id ...) regex str)
  (match-define (list _ id ...) (regexp-match regex str)))

(module+ test
  (test-case (name-string regexp-define)
    (regexp-define (as bs cs) #rx"(a*)(b*)(c*)" "aabbbbc")
    (check-equal? as "aa")
    (check-equal? bs "bbbb")
    (check-equal? cs "c")))

(define (string->char str)
  (unless (equal? (string-length str) 1)
    (raise-arguments-error
     (name string->char)
     "expected single character string"
     "string" str))
  (list-first (string->list str)))

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
