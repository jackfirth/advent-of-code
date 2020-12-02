#lang racket/base

(require racket/contract/base)

(provide
 regexp-define
 (contract-out
  [string->char (-> string? char?)]))

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
  (require (submod "..")
           rackunit))

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
