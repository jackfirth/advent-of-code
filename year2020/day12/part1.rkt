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

(define-tuple-type ship-position (direction x y))

(define initial-position (ship-position east 0 0))

(define (next-position position instruction)
  (match-define (ship-position ship-direction x y) position)
  (match-define (navigation-instruction direction amount) instruction)
  (match direction
    [(== north) (ship-position ship-direction x (+ y amount))]
    [(== south) (ship-position ship-direction x (- y amount))]
    [(== east) (ship-position ship-direction (+ x amount) y)]
    [(== west) (ship-position ship-direction (- x amount) y)]
    [(== left) (ship-position (turn-left ship-direction amount) x y)]
    [(== right) (ship-position (turn-right ship-direction amount) x y)]
    [(== forward)
     (match ship-direction
       [(== north) (ship-position ship-direction x (+ y amount))]
       [(== south) (ship-position ship-direction x (- y amount))]
       [(== east) (ship-position ship-direction (+ x amount) y)]
       [(== west) (ship-position ship-direction (- x amount) y)])]))

(define (turn-left direction amount)
  (match* (direction amount)
    [((== north) 90) west]
    [((== north) 180) south]
    [((== north) 270) east]
    [((== south) 90) east]
    [((== south) 180) north]
    [((== south) 270) west]
    [((== east) 90) north]
    [((== east) 180) west]
    [((== east) 270) south]
    [((== west) 90) south]
    [((== west) 180) east]
    [((== west) 270) north]))

(define (turn-right direction amount)
  (match* (direction amount)
    [((== north) 90) east]
    [((== north) 180) south]
    [((== north) 270) west]
    [((== south) 90) west]
    [((== south) 180) north]
    [((== south) 270) east]
    [((== east) 90) south]
    [((== east) 180) west]
    [((== east) 270) north]
    [((== west) 90) north]
    [((== west) 180) east]
    [((== west) 270) south]))

(define into-current-ship-position
  (make-fold-reducer next-position initial-position))

(module+ test
  (test-case "ship position updating"
    (define final-position
      (transduce (list "F10" "N3" "F7" "R90" "F11")
                 (mapping parse-navigation-instruction)
                 #:into into-current-ship-position))
    (check-equal? final-position (ship-position south 17 -8))))

(match-define (ship-position final-direction final-x final-y)
  (transduce (file->lines input)
             (mapping parse-navigation-instruction)
             #:into into-current-ship-position))

(+ (abs final-x) (abs final-y))
