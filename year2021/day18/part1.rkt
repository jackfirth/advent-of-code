#lang racket/base


(require advent-of-code/private/regexp-parsing
         fancy-app
         racket/file
         racket/list
         racket/match
         racket/math
         racket/port
         racket/runtime-path
         rebellion/base/option
         rebellion/base/pair
         rebellion/binary/bitstring
         rebellion/collection/multiset
         rebellion/collection/vector
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/tuple)


(module+ test
  (require rackunit))


;@----------------------------------------------------------------------------------------------------


(define-runtime-path input "input.txt")


(struct snailfish (left right) #:transparent
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (parameterize ([current-output-port out])
       (define (recur v)
         (match mode
           [#true (write v)]
           [#false (display v)]
           [_ (print v (current-output-port) mode)]))
       (write-string "[")
       (recur (snailfish-left this))
       (write-string ",")
       (recur (snailfish-right this))
       (write-string "]")
       (void)))])


(define left 'left)
(define right 'right)


(define (parse-snailfish str)
  (define (read-next)
    (match (peek-char)
      [#\[
       (read-char)
       (define l (read-next))
       (read-char)
       (define r (read-next))
       (read-char)
       (snailfish l r)]
      [(? char-numeric?)
       (read-num)]))
  (define (read-num)
    (let loop ([x (read-digit)])
      (match (peek-char)
        [(? char-numeric?)
         (loop (+ (* x 10) (read-digit)))]
        [_ x])))
  (define (read-digit)
    (match (read-char)
      [#\0 0]
      [#\1 1]
      [#\2 2]
      [#\3 3]
      [#\4 4]
      [#\5 5]
      [#\6 6]
      [#\7 7]
      [#\8 8]
      [#\9 9]))
  (with-input-from-string str
    (λ () (read-next))))


(define (snailfish-get x path)
  (match path
    [(cons (== left) rest-path) (snailfish-get (snailfish-left x) rest-path)]
    [(cons (== right) rest-path) (snailfish-get (snailfish-right x) rest-path)]
    ['() x]))


(define (snailfish-set x path y)
  (match path
    [(cons (== left) rest-path)
     (snailfish (snailfish-set (snailfish-left x) rest-path y) (snailfish-right x))]
    [(cons (== right) rest-path)
     (snailfish (snailfish-left x) (snailfish-set (snailfish-right x) rest-path y))]
    ['() y]))


(define (snailfish-modify x path updater)
  (snailfish-set x path (updater (snailfish-get x path))))


(define (snailfish-clear x path)
  (snailfish-set x path 0))


(module+ test

  (test-case "basic snailfish data structure operations"

    (define x (parse-snailfish "[[1,2],[[3,4],5]]"))

    (test-case (name-string snailfish-get)
      (check-equal? (snailfish-get x (list)) x)
      (check-equal? (snailfish-get x (list left)) (parse-snailfish "[1,2]"))
      (check-equal? (snailfish-get x (list right)) (parse-snailfish "[[3,4],5]"))
      (check-equal? (snailfish-get x (list left left)) 1)
      (check-equal? (snailfish-get x (list left right)) 2)
      (check-equal? (snailfish-get x (list right left left)) 3)
      (check-equal? (snailfish-get x (list right left right)) 4)
      (check-equal? (snailfish-get x (list right right)) 5))

    (test-case (name-string snailfish-set)
      (define x (parse-snailfish "[[1,2],[[3,4],5]]"))
      (check-equal? (snailfish-set x (list) 42) 42)
      (check-equal? (snailfish-set x (list left) 42) (parse-snailfish "[42,[[3,4],5]]"))
      (check-equal? (snailfish-set x (list right) 42) (parse-snailfish "[[1,2],42]"))
      (check-equal? (snailfish-set x (list left left) 42) (parse-snailfish "[[42,2],[[3,4],5]]"))
      (check-equal? (snailfish-set x (list left right) 42) (parse-snailfish "[[1,42],[[3,4],5]]"))
      (check-equal?
       (snailfish-set x (list right left left) 42) (parse-snailfish "[[1,2],[[42,4],5]]"))
      (check-equal?
       (snailfish-set x (list right left right) 42) (parse-snailfish "[[1,2],[[3,42],5]]"))
      (check-equal? (snailfish-set x (list right right) 42) (parse-snailfish "[[1,2],[[3,4],42]]")))))


(define/guard (snailfish-reduce x)
  (guard-match (present reduced) (option-or (snailfish-try-explode x) (snailfish-try-split x)) then
    (snailfish-reduce reduced))
  x)


(define/guard (snailfish-try-explode x)
  (guard-match (present path) (snailfish-find-explode-path x) else absent)
  (present (snailfish-clear (snailfish-explode-right (snailfish-explode-left x path) path) path)))


(define (snailfish-find-explode-path x)
  (let loop ([x x] [path '()] [depth 0])
    (match x
      [(? number?) absent]
      [(snailfish (? number?) (? number?))
       (if (< depth 4)
           absent
           (present (reverse path)))]
      [(snailfish a b)
       (option-or-call
        (loop a (cons left path) (add1 depth))
        (λ () (loop b (cons right path) (add1 depth))))])))


(define/guard (snailfish-explode-right x path)
  (define exploding (snailfish-get x path))
  (unless (snailfish? exploding)
    (raise-arguments-error (name snailfish-explode-right)
                           "expected a path to a pair, not a single number"
                           "snailfish number" x
                           "path" path
                           "snailfish number at path" exploding))
  (guard-match (present right-path) (snailfish-right-neighbor-path x path) else x)
  (define leaf (snailfish-right exploding))
  (snailfish-modify x right-path (+ _ leaf)))


(define/guard (snailfish-explode-left x path)
  (define exploding (snailfish-get x path))
  (unless (snailfish? exploding)
    (raise-arguments-error (name snailfish-explode-left)
                           "expected a path to a pair, not a single number"
                           "snailfish number" x
                           "path" path
                           "snailfish number at path" exploding))
  (guard-match (present left-path) (snailfish-left-neighbor-path x path) else x)
  (define leaf (snailfish-left exploding))
  (snailfish-modify x left-path (+ _ leaf)))


(define/guard (snailfish-right-neighbor-path x path)
  (define last-left-turn-index
    (for/last ([turn (in-list path)]
               [i (in-naturals)]
               #:when (equal? turn left))
      i))
  (guard last-left-turn-index
    else absent)
  (define path-with-right-turn (append (take path last-left-turn-index) (list right)))
  (define turnpoint (snailfish-get x path-with-right-turn))
  (present (append path-with-right-turn (snailfish-leftmost-path turnpoint))))


(define/guard (snailfish-left-neighbor-path x path)
  (define last-right-turn-index
    (for/last ([turn (in-list path)]
               [i (in-naturals)]
               #:when (equal? turn right))
      i))
  (guard last-right-turn-index
    else absent)
  (define path-with-left-turn (append (take path last-right-turn-index) (list left)))
  (define turnpoint (snailfish-get x path-with-left-turn))
  (present (append path-with-left-turn (snailfish-rightmost-path turnpoint))))


(define (snailfish-leftmost-path x)
  (match x
    [(snailfish l _)
     (cons left (snailfish-leftmost-path l))]
    [_ '()]))


(define (snailfish-rightmost-path x)
  (match x
    [(snailfish _ r)
     (cons right (snailfish-rightmost-path r))]
    [_ '()]))


(module+ test
  (test-case "snailfish explosion"

    (define (test-snailfish-explosion input
                                      #:expected-path expected-path
                                      #:expected-left-neighbor-path expected-left-neighbor-path
                                      #:expected-right-neighbor-path expected-right-neighbor-path
                                      #:expected-output expected-output)
      (let ([input (parse-snailfish input)]
            [expected-output (parse-snailfish expected-output)])
        (check-equal? (snailfish-find-explode-path input) (present expected-path))
        (check-equal? (snailfish-left-neighbor-path input expected-path) expected-left-neighbor-path)
        (check-equal?
         (snailfish-right-neighbor-path input expected-path) expected-right-neighbor-path)
        (check-equal? (snailfish-try-explode input) (present expected-output))))
    
    (test-snailfish-explosion "[[[[[9,8],1],2],3],4]"
                              #:expected-path (list left left left left)
                              #:expected-left-neighbor-path absent
                              #:expected-right-neighbor-path (present (list left left left right))
                              #:expected-output "[[[[0,9],2],3],4]")

    (test-snailfish-explosion "[7,[6,[5,[4,[3,2]]]]]"
                              #:expected-path (list right right right right)
                              #:expected-left-neighbor-path (present (list right right right left))
                              #:expected-right-neighbor-path absent
                              #:expected-output "[7,[6,[5,[7,0]]]]")

    (test-snailfish-explosion "[[6,[5,[4,[3,2]]]],1]"
                              #:expected-path (list left right right right)
                              #:expected-left-neighbor-path (present (list left right right left))
                              #:expected-right-neighbor-path (present (list right))
                              #:expected-output "[[6,[5,[7,0]]],3]")

    (test-snailfish-explosion "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
                              #:expected-path (list left right right right)
                              #:expected-left-neighbor-path (present (list left right right left))
                              #:expected-right-neighbor-path (present (list right left))
                              #:expected-output "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")

    (test-snailfish-explosion "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
                              #:expected-path (list right right right right)
                              #:expected-left-neighbor-path (present (list right right right left))
                              #:expected-right-neighbor-path absent
                              #:expected-output "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")))


(define/guard (snailfish-try-split x)
  (guard-match (present path) (snailfish-find-split-path x) else absent)
  (present (snailfish-modify x path snailfish-split-number)))


(define (snailfish-find-split-path x)
  (let loop ([x x] [path '()])
    (match x
      [(? number?) (if (>= x 10) (present (reverse path)) absent)]
      [(snailfish a b)
       (option-or-call
        (loop a (cons left path))
        (λ () (loop b (cons right path))))])))


(define (snailfish-split-number n)
   (snailfish (exact-floor (/ n 2)) (exact-ceiling (/ n 2))))


(module+ test
  (test-case "snailfish splitting"
    (define input (parse-snailfish "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"))
    (define expected (parse-snailfish "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"))
    (check-equal? (snailfish-try-split input) (present expected))))


(define (snailfish-add x y)
  (snailfish-reduce (snailfish x y)))


(module+ test
  (test-case "snailfish addition"
    (define left-input (parse-snailfish "[[[[4,3],4],4],[7,[[8,4],9]]]"))
    (define right-input (parse-snailfish "[1,1]"))
    (define expected (parse-snailfish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))
    (check-equal? (snailfish-add left-input right-input) expected)))


(define (snailfish-magnitude x)
  (match x
    [(snailfish l r)
     (+ (* (snailfish-magnitude l) 3) (* (snailfish-magnitude r) 2))]
    [(? number?) x]))


(module+ test
  (test-case "snailfish magnitude"
    (check-equal? (snailfish-magnitude (parse-snailfish "[9,1]")) 29)
    (check-equal? (snailfish-magnitude (parse-snailfish "[1,9]")) 21)
    (check-equal? (snailfish-magnitude (parse-snailfish "[[9,1],[1,9]]")) 129)
    (check-equal? (snailfish-magnitude (parse-snailfish "[[1,2],[[3,4],5]]")) 143)
    (check-equal? (snailfish-magnitude (parse-snailfish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")) 1384)))


(module+ main
  (define nums
    (for/list ([line (file->lines input)])
      (parse-snailfish line)))
  (for/fold ([sum (first nums)]
             #:result (snailfish-magnitude sum))
            ([num (in-list (rest nums))])
    (snailfish-add sum num)))
