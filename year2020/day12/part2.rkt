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
         rebellion/type/enum
         rebellion/type/singleton
         rebellion/type/tuple
         rebellion/type/wrapper)

(module+ test
  (require rackunit
           rebellion/private/static-name))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define-enum-type direction (north south east west left right forward))

(define (char->direction ch)
  (match ch [#\N north] [#\S south] [#\E east] [#\W west] [#\L left] [#\R right] [#\F forward]))

(define-tuple-type navigation-instruction (direction amount))

(define (parse-navigation-instruction str)
  (regexp-define (direction amount) #px"([NSEWLRF])(\\d+)" str)
  (navigation-instruction (char->direction (string-ref direction 0)) (string->number amount)))

(module+ test
  (test-case (name-string parse-navigation-instruction)
    (check-equal? (parse-navigation-instruction "R90") (navigation-instruction right 90))))

(define-tuple-type ship-position (waypoint-x waypoint-y x y))

(define initial-position (ship-position 10 1 0 0))

(define (next-position position instruction)
  (match-define (ship-position wx wy x y) position)
  (match-define (navigation-instruction direction amount) instruction)
  (match direction
    [(== north) (ship-position wx (+ wy amount) x y)]
    [(== south) (ship-position wx (- wy amount) x y)]
    [(== east) (ship-position (+ wx amount) wy x y)]
    [(== west) (ship-position (- wx amount) wy x y)]
    [(== left) (turn-left position amount)]
    [(== right) (turn-right position amount)]
    [(== forward)
     (define x* (+ x (* amount wx)))
     (define y* (+ y (* amount wy)))
     (ship-position wx wy x* y*)]))

(define (turn-left position amount)
  (match-define (ship-position wx wy x y) position)
  (match amount
    [90 (ship-position (- wy) wx x y)]
    [180 (ship-position (- wx) (- wy) x y)]
    [270 (ship-position wy (- wx) x y)]))

(define (turn-right position amount)
  (match-define (ship-position wx wy x y) position)
  (match amount
    [90 (ship-position wy (- wx) x y)]
    [180 (ship-position (- wx) (- wy) x y)]
    [270 (ship-position (- wy) wx x y)]))

(define into-current-ship-position
  (make-fold-reducer next-position initial-position))

(module+ test
  (test-case "ship position updating"
    (define position1 (next-position initial-position (parse-navigation-instruction "F10")))
    (check-equal? position1 (ship-position 10 1 100 10))

    (define position2 (next-position position1 (parse-navigation-instruction "N3")))
    (check-equal? position2 (ship-position 10 4 100 10))

    (define position3 (next-position position2 (parse-navigation-instruction "F7")))
    (check-equal? position3 (ship-position 10 4 170 38))

    (define position4 (next-position position3 (parse-navigation-instruction "R90")))
    (check-equal? position4 (ship-position 4 -10 170 38))

    (define position5 (next-position position4 (parse-navigation-instruction "F11")))
    (check-equal? position5 (ship-position 4 -10 214 -72))))

(match-define (ship-position _ _ final-x final-y)
  (transduce (file->lines input)
             (mapping parse-navigation-instruction)
             #:into into-current-ship-position))

(+ (abs final-x) (abs final-y))
