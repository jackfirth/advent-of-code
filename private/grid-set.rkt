#lang racket/base

;; A grid set is a collection of spaces within a 2d grid of finite width and height. The width and
;; height of a grid set are significant: an empty 2x2 grid is distinct from an empty 10x10 grid.
;; Note that grid dimensions are typically written as width x height, and that the "size" of a grid
;; set refers to the number of spaces it contains and not its dimensions.

(require racket/contract/base)

(provide
 (contract-out
  #:unprotected-submodule no-contracts
  [grid-dimension/c flat-contract?]
  [grid-set? predicate/c]
  [grid-set (-> #:width grid-dimension/c #:height grid-dimension/c grid-space? ... grid-set?)]
  [empty-grid-set? predicate/c]
  [nonempty-grid-set? predicate/c]
  [in-grid-set (-> grid-set? grid-set-sequence/c)]
  [into-grid-set
   (-> #:width grid-dimension/c #:height grid-dimension/c (reducer/c grid-space? grid-set?))]
  [sequence->grid-set
   (-> grid-set-sequence/c #:width grid-dimension/c #:height grid-dimension/c grid-set?)]
  [grid-set-width (-> grid-set? natural?)]
  [grid-set-height (-> grid-set? natural?)]
  [grid-set-size (-> grid-set? natural?)]
  [grid-set-contains? (-> grid-set? #:row natural? #:column natural? boolean?)]
  [grid-set-contains-space? (-> grid-set? grid-space? boolean?)]
  [grid-set-contains-all? (-> grid-set? grid-set-sequence/c boolean?)]
  [grid-set-contains-any? (-> grid-set? grid-set-sequence/c boolean?)]
  [grid-set-contains-none? (-> grid-set? grid-set-sequence/c boolean?)]
  [grid-set-sequence/c contract?]
  [grid-set-builder? predicate/c]
  [make-grid-set-builder
   (-> #:width grid-dimension/c #:height grid-dimension/c grid-set-builder?)]
  [grid-set-builder-add
   (-> unused-grid-set-builder/c #:row natural? #:column natural? unused-grid-set-builder/c)]
  [grid-set-builder-add-space
   (-> unused-grid-set-builder/c grid-space? ... unused-grid-set-builder/c)]
  [grid-set-builder-add-all
   (-> unused-grid-set-builder/c grid-set-sequence/c unused-grid-set-builder/c)]
  [build-grid-set (-> unused-grid-set-builder/c grid-set?)]))

(require (submod advent-of-code/private/grid-space no-contracts)
         fancy-app
         racket/contract/combinator
         racket/math
         racket/sequence
         racket/stream
         rebellion/base/generative-token
         rebellion/binary/bitstring
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

(module+ test
  (require (submod "..")
           rackunit))

;@--------------------------------------------------------------------------------------------------

(struct grid-set (occupancy-bitstring size width height)

  #:property prop:sequence
  (λ (this)
    (define width (grid-set-width this))
    (define height (grid-set-height this))
    (define occupancy-bitstring (grid-set-occupancy-bitstring this))
    (for*/stream ([row (in-range 0 height)]
                  [column (in-range 0 width)]
                  [i (in-value (+ (* row width) column))]
                  #:unless (zero? (bitstring-ref occupancy-bitstring i)))
      (grid-space #:row row #:column column)))

  #:methods gen:equal+hash
  [(define (equal-proc this other recur)
     (and (= (grid-set-size this) (grid-set-size other))
          (= (grid-set-width this) (grid-set-width other))
          (= (grid-set-height this) (grid-set-height other))
          (= (grid-set-occupancy-bitstring this) (grid-set-occupancy-bitstring other))))
   (define hash-mixing-token1 (make-generative-token))
   (define hash-mixing-token2 (make-generative-token))
   (define (hash-proc this recur)
     (recur (list hash-mixing-token1 (grid-set-occupancy-bitstring this))))
   (define (hash2-proc this recur)
     (recur (list hash-mixing-token2 (grid-set-occupancy-bitstring this))))]
  
  #:omit-define-syntaxes

  #:constructor-name constructor:grid-set)

(define (empty-grid-set? v) (and (grid-set? v) (zero? (grid-set-size v))))
(define (nonempty-grid-set? v) (and (grid-set? v) (positive? (grid-set-size v))))
(define grid-set-sequence/c (or/c grid-set? (sequence/c grid-space?)))
(define grid-dimension/c natural?)

(define (grid-set #:width width #:height height . spaces)
  (sequence->grid-set spaces #:width width #:height height))

(define (check-grid-set-row-index checker set row)
  (void))

(define (check-grid-set-column-index checker set column)
  (void))

(define (check-grid-set-space checker set space)
  (check-grid-set-row-index checker set (grid-space-row space))
  (check-grid-set-column-index checker set (grid-space-column space)))

(define (grid-set-occupancy-index set #:row row #:column column)
  (+ (* row (grid-set-width set)) column))

(define/name (grid-set-contains? set #:row row #:column column)
  (check-grid-set-row-index enclosing-function-name set row)
  (check-grid-set-column-index enclosing-function-name set column)
  (define occupants (grid-set-occupancy-bitstring set))
  (equal? (bitstring-ref occupants (grid-set-occupancy-index set #:row row #:column column)) 1))

(define/name (grid-set-contains-space? set space)
  (check-grid-set-space enclosing-function-name set space)
  (define row (grid-space-row space))
  (define column (grid-space-column space))
  (define occupants (grid-set-occupancy-bitstring set))
  (equal? (bitstring-ref occupants (grid-set-occupancy-index set #:row row #:column column)) 1))

(define (grid-set-contains-all? set spaces)
  (transduce spaces #:into (into-all-match? (grid-set-contains-space? set _))))

(define (grid-set-contains-any? set spaces)
  (transduce spaces #:into (into-any-match? (grid-set-contains-space? set _))))

(define (grid-set-contains-none? set spaces)
  (transduce spaces #:into (into-none-match? (grid-set-contains-space? set _))))

(define (in-grid-set set) set)

(define/name (into-grid-set #:width width #:height height)
  (make-effectful-fold-reducer
   grid-set-builder-add-space
   (λ () (make-grid-set-builder #:width width #:height height))
   build-grid-set
   #:name enclosing-function-name))

(define (sequence->grid-set sequence #:width width #:height height)
  (transduce sequence #:into (into-grid-set #:width width #:height height)))

(struct grid-set-builder
  ([uses-remaining #:mutable]
   next-total-uses
   contents
   [size #:mutable]
   width
   height)
  #:constructor-name constructor:grid-set-builder)

(define/name unused-grid-set-builder/c
  (flat-contract-with-explanation
   (λ (v)
     (guarded-block
       (guard (grid-set-builder? v) else
         (define template '(expected: "grid-set-builder?" given: "~e"))
         (λ (blame) (raise-blame-error blame v template v)))
       (guard (positive? (grid-set-builder-uses-remaining v)) else
         (define template '("this builder has already been used\n  builder: ~e"))
         (λ (blame) (raise-blame-error blame v template v)))
       #true))
   #:name enclosing-variable-name))

(define (make-grid-set-builder #:width width #:height height)
  (define contents (make-vector (* width height) 0))
  (constructor:grid-set-builder 1 2 contents 0 width height))

(define/guard (grid-set-builder-mark-used-once builder)
  (define new-uses (sub1 (grid-set-builder-uses-remaining builder)))
  (set-grid-set-builder-uses-remaining! builder new-uses)
  (guard (zero? new-uses) else builder)
  (define next-total (grid-set-builder-next-total-uses builder))
  (define contents (grid-set-builder-contents builder))
  (define size (grid-set-builder-size builder))
  (define width (grid-set-builder-width builder))
  (define height (grid-set-builder-height builder))
  (constructor:grid-set-builder next-total (* next-total 2) contents size width height))

(define (grid-set-builder-add builder #:row row #:column column)
  (define width (grid-set-builder-width builder))
  (define contents (grid-set-builder-contents builder))
  (vector-set! contents (+ (* row width) column) 1)
  (set-grid-set-builder-size! builder (add1 (grid-set-builder-size builder)))
  (grid-set-builder-mark-used-once builder))

(define (grid-set-builder-add-space builder . spaces)
  (grid-set-builder-add-all builder spaces))

(define (grid-set-builder-add-all builder spaces)
  (define width (grid-set-builder-width builder))
  (define contents (grid-set-builder-contents builder))
  (for ([space spaces])
    (define row (grid-space-row space))
    (define column (grid-space-column space))
    (vector-set! contents (+ (* row width) column) 1)
    (set-grid-set-builder-size! builder (add1 (grid-set-builder-size builder))))
  (grid-set-builder-mark-used-once builder))

(define (build-grid-set builder)
  (set-grid-set-builder-uses-remaining! builder 0)
  (define contents (grid-set-builder-contents builder))
  (define size (grid-set-builder-size builder))
  (define width (grid-set-builder-width builder))
  (define height (grid-set-builder-height builder))
  (constructor:grid-set (apply bitstring (vector->list contents)) size width height))

(module+ test
  (test-case (name-string grid-set)
    (define set (grid-set (grid-space #:row 2 #:column 3) #:width 5 #:height 10))
    (check-equal? (grid-set-size set) 1)
    (check-equal? (grid-set-width set) 5)
    (check-equal? (grid-set-height set) 10)
    (check-equal? (sequence->list set) (list (grid-space #:row 2 #:column 3))))

  (test-case "grid-set containment queries"
    (define space1 (grid-space #:row 2 #:column 2))
    (define space2 (grid-space #:row 0 #:column 3))
    (define space3 (grid-space #:row 3 #:column 8))
    (define set (grid-set #:width 5 #:height 10 space1 space2))

    (test-case (name-string grid-set-contains?)
      (check-true (grid-set-contains? set #:row 2 #:column 2))
      (check-true (grid-set-contains? set #:row 0 #:column 3))
      (check-false (grid-set-contains? set #:row 3 #:column 8)))

    (test-case (name-string grid-set-contains-space?)
      (check-true (grid-set-contains-space? set space1))
      (check-true (grid-set-contains-space? set space2))
      (check-false (grid-set-contains-space? set space3)))

    (test-case (name-string grid-set-contains-all?)
      (check-true (grid-set-contains-all? set (list space1 space2)))
      (check-false (grid-set-contains-all? set (list space1 space3)))
      (check-false (grid-set-contains-all? set (list space2 space3)))
      (check-false (grid-set-contains-all? set (list space3)))
      (check-true (grid-set-contains-all? set empty-list))
      (check-false (grid-set-contains-all? set (list space1 space2 space3))))

    (test-case (name-string grid-set-contains-none?)
      (check-false (grid-set-contains-none? set (list space1 space2)))
      (check-false (grid-set-contains-none? set (list space1 space3)))
      (check-false (grid-set-contains-none? set (list space2 space3)))
      (check-true (grid-set-contains-none? set (list space3)))
      (check-true (grid-set-contains-none? set empty-list))
      (check-false (grid-set-contains-none? set (list space1 space2 space3))))

    (test-case (name-string grid-set-contains-any?)
      (check-true (grid-set-contains-any? set (list space1 space2)))
      (check-true (grid-set-contains-any? set (list space1 space3)))
      (check-true (grid-set-contains-any? set (list space2 space3)))
      (check-false (grid-set-contains-any? set (list space3)))
      (check-false (grid-set-contains-any? set empty-list))
      (check-true (grid-set-contains-any? set (list space1 space2 space3))))))
