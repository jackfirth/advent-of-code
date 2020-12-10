#lang racket/base

(require advent-of-code/private/grid-set
         advent-of-code/private/grid-space
         fancy-app
         racket/file
         racket/list
         racket/match
         racket/runtime-path
         racket/stream
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record)

(module+ test
  (require rackunit))

;@--------------------------------------------------------------------------------------------------

(define-runtime-path input "input.txt")

(define input-lines (file->lines input))
(define width (string-length (first input-lines)))
(define height (length input-lines))

(define tree-grid
  (for*/reducer (into-grid-set #:width width #:height height)
                ([(row-string row) (in-indexed (in-list input-lines))]
                 [(column-char column) (in-indexed (in-string row-string))]
                 #:when (equal? column-char #\#))
    (grid-space #:row row #:column column)))

(define (in-slope-spaces #:right rightward-delta #:down downward-delta)
  (for/stream ([row (in-range 0 height downward-delta)]
               [column (in-range 0 +inf.0 rightward-delta)])
    (grid-space #:row row #:column (modulo column width))))

(define (slope-tree-count #:right rightward-delta #:down downward-delta)
  (transduce (in-slope-spaces #:right rightward-delta #:down downward-delta)
             (filtering (grid-set-contains-space? tree-grid _))
             #:into into-count))

(* (slope-tree-count #:right 1 #:down 1)
   (slope-tree-count #:right 3 #:down 1)
   (slope-tree-count #:right 5 #:down 1)
   (slope-tree-count #:right 7 #:down 1)
   (slope-tree-count #:right 1 #:down 2))
