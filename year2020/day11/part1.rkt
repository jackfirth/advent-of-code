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
         rebellion/type/singleton
         rebellion/type/wrapper)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define lines (file->lines input))
(define width (string-length (first lines)))
(define height (length lines))

(define seat-grid
  (for/reducer (into-grid-set #:width width #:height height)
               ([row-line (in-list lines)]
                [row (in-naturals)]
                #:when #true
                [column-char (in-string row-line)]
                [column (in-naturals)]
                #:when (equal? column-char #\L))
    (grid-space #:row row #:column column)))

(define occupation-grid (grid-set #:width width #:height height))

(define (space-neighbors space occupation-grid)
  (define row (grid-space-row space))
  (define column (grid-space-column space))
  (for*/sum ([r (in-range (max 0 (sub1 row)) (min height (+ row 2)))]
             [c (in-range (max 0 (sub1 column)) (min width (+ column 2)))]
             #:unless (and (equal? r row) (equal? c column))
             #:when (grid-set-contains? occupation-grid #:row r #:column c))
    1))

(define/guard (loop [occupation-grid occupation-grid] [generation 0])
  (define-values (grid-after-occupants-leave any-seats-became-empty?)
    (for*/fold ([builder (make-grid-set-builder #:width width #:height height)]
                [any-seats-became-empty? #false]
                #:result (values (build-grid-set builder) any-seats-became-empty?))
               ([space (in-grid-set occupation-grid)])
      (define num-occupied-neighbor-seats (space-neighbors space occupation-grid))
      (if (>= num-occupied-neighbor-seats 4)
          (values builder #true)
          (values (grid-set-builder-add-space builder space) any-seats-became-empty?))))
  (define-values (new-arrivals-grid any-seats-became-full?)
    (for*/fold ([builder (make-grid-set-builder #:width width #:height height)]
                [any-seats-became-full? #false]
                #:result (values (build-grid-set builder) any-seats-became-full?))
               ([space (in-grid-set seat-grid)]
                #:unless (grid-set-contains-space? occupation-grid space))
      (define num-occupied-neighbor-seats (space-neighbors space occupation-grid))
      (if (equal? num-occupied-neighbor-seats 0)
          (values (grid-set-builder-add-space builder space) #true)
          (values builder any-seats-became-full?))))
  (define any-changes? (or any-seats-became-empty? any-seats-became-full?))
  (define new-occupation-grid
    (for/reducer (into-grid-set #:width width #:height height)
                 ([seat (sequence-append grid-after-occupants-leave new-arrivals-grid)])
      seat))
  (printf "generation ~a, occupants: ~a, incoming arrivals: ~a, pending departures: ~a\n"
          generation
          (grid-set-size occupation-grid)
          (grid-set-size new-arrivals-grid)
          (- (grid-set-size occupation-grid) (grid-set-size grid-after-occupants-leave)))
  (guard any-changes? then (loop new-occupation-grid (add1 generation)))
  new-occupation-grid)

(define stable-occupation-grid (loop))

(grid-set-size stable-occupation-grid)
