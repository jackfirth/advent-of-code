#lang racket/base

;; A grid space is a pair of natural numbers representing a row number and a column number.

(require racket/contract/base)

(provide
 (contract-out
  [grid-space? predicate/c]
  [grid-space (-> #:row natural? #:column natural? grid-space?)]
  [grid-space-row (-> grid-space? natural?)]
  [grid-space-column (-> grid-space? natural?)]))

(require racket/math
         rebellion/type/record)

;@--------------------------------------------------------------------------------------------------

;; We use a record type instead of a tuple type because when written as an ordered pair, people
;; assume the X coordinate comes first, but when described with words, people call them a "row and
;; column" which puts the row index (and therefore the Y coordinate) first. Accidentally using a
;; column number where a row number is expected or vice-versa creates bugs that are unlikely to be
;; caught by type systems and runtime checks, so we explicitly require keyword arguments to prevent
;; this confusion.
(define-record-type grid-space (row column))
