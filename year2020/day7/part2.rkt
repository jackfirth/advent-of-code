#lang racket/base

(require advent-of-code/private/regexp-parsing
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/multiset
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/tuple)

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define-tuple-type bag-rule (type contents))

(define/guard (parse-bag-rule rule-text)
  (regexp-define (bag-type contained-types-string) #px"(\\w+ \\w+) bags contain (.*)\\." rule-text)
  (guard (equal? contained-types-string "no other bags") then
    (bag-rule bag-type empty-multiset))
  (define contained-types
    (for/reducer (into-transduced (append-mapping values) #:into into-multiset)
                 ([type-string (in-list (string-split contained-types-string ", "))])
      (regexp-define (amount bag-type _) #px"(\\d+) (\\w+ \\w+) (bag|bags)" type-string)
      (make-list (string->number amount) bag-type)))
  (bag-rule bag-type contained-types))

(define bag-hash
  (transduce (file->lines input)
             (mapping parse-bag-rule)
             (bisecting bag-rule-type bag-rule-contents)
             #:into into-hash))

(define (transitive-bag-count bag-hash bag)
  (define direct-bags (hash-ref bag-hash bag))
  (+ (multiset-size direct-bags)
     (for/sum ([bag (in-multiset direct-bags)])
       (transitive-bag-count bag-hash bag))))

(transitive-bag-count bag-hash "shiny gold")
