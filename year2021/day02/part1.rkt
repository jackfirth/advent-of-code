#lang racket


(require advent-of-code/private/regexp-parsing
         racket/runtime-path
         rebellion/type/tuple)


;@----------------------------------------------------------------------------------------------------


(define-runtime-path input "input.txt")


(define-tuple-type submarine-position (distance depth))


(module+ main
  (for/fold ([distance 0]
             [depth 0]
             #:result (* distance depth))
            ([line (file->lines input)])
    (regexp-define (direction amount-str) #px"(forward|down|up) (\\d+)" line)
    (define amount (string->number amount-str))
    (match direction
      ["forward" (values (+ distance amount) depth)]
      ["up" (values distance (- depth amount))]
      ["down" (values distance (+ depth amount))])))
