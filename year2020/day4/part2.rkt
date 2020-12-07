#lang racket/base

(require advent-of-code/private/regexp-parsing
         racket/file
         racket/match
         racket/runtime-path
         racket/set
         racket/string
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

(module+ test
  (require rackunit
           rebellion/private/static-name))

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

(define/guard (valid-passport? passport)
  (guard (subset? (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid") (hash-key-set passport)) else
    #false)
  (define byr (string->number (hash-ref passport "byr")))
  (guard (<= 1920 byr 2002) else #false)
  (define iyr (string->number (hash-ref passport "iyr")))
  (guard (<= 2010 iyr 2020) else #false)
  (define eyr (string->number (hash-ref passport "eyr")))
  (guard (<= 2020 eyr 2030) else #false)
  (guard (valid-height? (hash-ref passport "hgt")) else #false)
  (guard (valid-hair-color? (hash-ref passport "hcl")) else #false)
  (define ecl (hash-ref passport "ecl"))
  (guard (set-member? (set "amb" "blu" "brn" "gry" "grn" "hzl" "oth") ecl) else #false)
  (valid-pid? (hash-ref passport "pid")))

(define (valid-height? height-string)
  (regexp-define (hgt unit) #px"(\\d+)(\\w+)" height-string)
  (cond
    [(equal? unit "cm") (<= 150 (string->number hgt) 193)]
    [(equal? unit "in") (<= 59 (string->number hgt) 76)]
    [else #false]))

(define (valid-hair-color? hcl)
  (regexp-match-exact? #px"#[0-9a-f]{6}" hcl))

(define (valid-pid? pid-string)
  (regexp-match-exact? #px"\\d{9}" pid-string))

(module+ test
  (test-case (name-string valid-height?)
    (check-true (valid-height? "60in"))
    (check-true (valid-height? "190cm"))
    (check-false (valid-height? "190in"))
    (check-false (valid-height? "190")))
  
  (test-case (name-string valid-hair-color?)
    (check-true (valid-hair-color? "#123abc"))
    (check-false (valid-hair-color? "#123abz"))
    (check-false (valid-hair-color? "123abc")))
  
  (test-case (name-string valid-passport?)
    (check-true
     (valid-passport?
      (hash "pid" "087499704"
            "hgt" "74in"
            "ecl" "grn"
            "iyr" "2012"
            "eyr" "2030"
            "byr" "1980"
            "hcl" "#623a2f")))
    (check-true
     (valid-passport?
      (hash "eyr" "2029"
            "ecl" "blu"
            "cid" "129"
            "byr" "1989"
            "iyr" "2014"
            "pid" "896056539"
            "hcl" "#a97842"
            "hgt" "165cm")))))

(transduce input-lines
           batching-passports
           (filtering valid-passport?)
           #:into into-count)
