#lang racket/base

(require racket/file
         racket/match
         racket/runtime-path
         racket/set
         racket/string
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

(module+ test
  (require))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define input-lines (file->lines input))

(define batching-passports
  (batching
   (into-transduced
    (taking-while non-empty-string?)
    (append-mapping string-split)
    (mapping
     (λ (field)
       (match-define (list key value) (string-split field ":"))
       (entry key value)))
    #:into into-hash)))

(define (passport-fields passport) (hash-key-set passport))

(define (valid-passport-fields? fields)
  (subset? (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid") fields))

(transduce input-lines
           batching-passports
           (mapping passport-fields)
           (filtering valid-passport-fields?)
           #:into into-count)
