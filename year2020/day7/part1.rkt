#lang racket/base

(require advent-of-code/private/regexp-parsing
         racket/file
         racket/match
         racket/runtime-path
         racket/set
         racket/string
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multidict
         rebellion/collection/set
         rebellion/collection/vector
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/tuple)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define-tuple-type bag-rule (type contents))

(define/guard (parse-bag-rule rule-text)
  (regexp-define (bag-type contained-types-string) #px"(\\w+ \\w+) bags contain (.*)\\." rule-text)
  (guard (equal? contained-types-string "no other bags") then
    (bag-rule bag-type empty-set))
  (define contained-types
    (for/set ([type-string (in-list (string-split contained-types-string ", "))])
      (regexp-define (amount bag-type _) #px"(\\d+) (\\w+ \\w+) (bag|bags)" type-string)
      bag-type))
  (bag-rule bag-type contained-types))

(define allowed-bag-contents
  (transduce (file->lines input)
             (mapping parse-bag-rule)
             (bisecting bag-rule-contents bag-rule-type)
             (append-mapping-keys values)
             #:into into-multidict))

(define (multidict-transitive-closure dict key)
  (define/guard (loop [unanalyzed-keys (set key)]
                [analyzed-keys (set)]
                [known-closure-elements (set)])
    (guard (set-empty? unanalyzed-keys) then known-closure-elements)
    (define next (set-first unanalyzed-keys))
    (define rest (set-rest unanalyzed-keys))
    (define new-direct-deps
      (set-subtract (multidict-ref dict next)
                    analyzed-keys
                    known-closure-elements))
    (loop (set-union rest new-direct-deps)
          (set-add analyzed-keys next)
          (set-union known-closure-elements new-direct-deps)))
  (loop))

(module+ test
  (test-case (name-string multidict-transitive-closure)
    (define dict
      (multidict
       'a 'b
       'a 'c
       'b 'd
       'b 'e
       'c 'f))
    (check-equal? (multidict-transitive-closure dict 'a) (set 'b 'c 'd 'e 'f))
    (check-equal? (multidict-transitive-closure dict 'b) (set 'd 'e))
    (check-equal? (multidict-transitive-closure dict 'c) (set 'f))
    (check-equal? (multidict-transitive-closure dict 'd) empty-set)
    (check-equal? (multidict-transitive-closure dict 'e) empty-set)
    (check-equal? (multidict-transitive-closure dict 'f) empty-set)
    (check-equal? (multidict-transitive-closure dict 'g) empty-set)))

(set-count (multidict-transitive-closure allowed-bag-contents "shiny gold"))
