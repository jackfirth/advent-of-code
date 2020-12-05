#lang racket/base

(require advent-of-code/private/grid-set
         advent-of-code/private/grid-space
         fancy-app
         racket/file
         racket/list
         racket/match
         racket/runtime-path
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

(for/sum ([row (in-range 0 height)]
          [column (in-range 0 +inf.0 3)])
  (define adjusted-column (modulo column width))
  (if (grid-set-contains? tree-grid #:row row #:column adjusted-column) 1 0))
