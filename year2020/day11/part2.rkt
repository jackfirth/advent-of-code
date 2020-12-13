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



(define (first-visible-seat space #:row-delta dr #:column-delta dc)
  (define/guard (loop [row (+ (grid-space-row space) dr)]
                      [column (+ (grid-space-column space) dc)])
    (guard (<= 0 row (sub1 height)) else #false)
    (guard (<= 0 column (sub1 width)) else #false)
    (guard (grid-set-contains? seat-grid #:row row #:column column) then
      (grid-space #:row row #:column column))
    (loop (+ row dr) (+ column dc)))
  (loop))

(define dr-list (list 0 -1 -1 -1 0 1 1 1))
(define dc-list (list 1 1 0 -1 -1 -1 0 1))

(define first-visible-seat-table
  (for/hash ([dr (in-list dr-list)]
             [dc (in-list dc-list)]
             #:when #true
             [seat (in-grid-set seat-grid)]
             #:when #true
             [first-visible (in-value (first-visible-seat seat #:row-delta dr #:column-delta dc))]
             #:when first-visible)
    (values (list dr dc seat) first-visible)))

(define (space-neighbors space occupation-grid)
  (define row (grid-space-row space))
  (define column (grid-space-column space))
  (for/sum ([dr (in-list dr-list)]
            [dc (in-list dc-list)]
            #:when (hash-has-key? first-visible-seat-table (list dr dc space))
            #:when
            (grid-set-contains-space?
             occupation-grid
             (hash-ref first-visible-seat-table (list dr dc space))))
    1))

(define/guard (loop [occupation-grid occupation-grid] [generation 0])
  (define-values (grid-after-occupants-leave any-seats-became-empty?)
    (for*/fold ([builder (make-grid-set-builder #:width width #:height height)]
                [any-seats-became-empty? #false]
                #:result (values (build-grid-set builder) any-seats-became-empty?))
               ([space (in-grid-set occupation-grid)])
      (define num-occupied-neighbor-seats (space-neighbors space occupation-grid))
      (if (>= num-occupied-neighbor-seats 5)
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

(module+ main
  (define stable-occupation-grid (loop))

  (grid-set-size stable-occupation-grid))
