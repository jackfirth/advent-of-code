#lang info

(define collection "advent-of-code")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "advent-of-code")))

(define deps
  (list "rebellion"
        "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
