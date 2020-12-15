#lang racket/base

(require racket/contract/base)

(provide
 guard-match
 regexp-define
 (contract-out
  [string->char (-> string? char?)]))

(require (for-syntax racket/base)
         fancy-app
         racket/file
         racket/match
         racket/runtime-path
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rebellion/base/option
           rackunit))

;@--------------------------------------------------------------------------------------------------

(define-syntax-parser regexp-define
  #:literals (then else)
  [(_ (id:id ...) regex str)
   #'(match-define (list _ id ...) (regexp-match regex str))]
  [(_ (id:id ...) regex str then early-exit-body ...+)
   #'(guard-match (list _ id ...) (regexp-match regex str) then early-exit-body ...)]
  [(_ (id:id ...) regex str else early-exit-body ...+)
   #'(guard-match (list _ id ...) (regexp-match regex str) else early-exit-body ...)])

(define-syntax-parser guard-match
  #:literals (then else)
  [(_ pattern subject:expr then success-body ...+)
   #'(begin
       (define subject-matched? (match subject [pattern #true] [_ #false]))
       (guard subject-matched? then
         (match-define pattern subject)
         success-body ...))]
  [(_ pattern subject:expr else failure-body ...+)
   #'(begin
       (define subject-matched? (match subject [pattern #true] [_ #false]))
       (guard subject-matched? else failure-body ...)
       (match-define pattern subject))])

(module+ test
  (test-case (name-string regexp-define)
    (regexp-define (as bs cs) #rx"(a*)(b*)(c*)" "aabbbbc")
    (check-equal? as "aa")
    (check-equal? bs "bbbb")
    (check-equal? cs "c"))

  (test-case (name-string guard-match)
    (test-case "then branch"
      (define/guard (f opt)
        (guard-match (present x) opt then (format "x = ~a" x))
        "failed")
      (check-equal? (f absent) "failed")
      (check-equal? (f (present 5)) "x = 5"))

    (test-case "else branch"
      (define/guard (f opt)
        (guard-match (present x) opt else "failed")
        (format "x = ~a" x))
      (check-equal? (f absent) "failed")
      (check-equal? (f (present 5)) "x = 5"))))

(define (string->char str)
  (unless (equal? (string-length str) 1)
    (raise-arguments-error
     (name string->char)
     "expected single character string"
     "string" str))
  (list-first (string->list str)))
