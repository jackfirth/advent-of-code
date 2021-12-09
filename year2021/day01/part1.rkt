#lang racket


(require racket/runtime-path)


;@----------------------------------------------------------------------------------------------------


(define-runtime-path input "input.txt")


(module+ main
  (for/fold ([previous #false]
             [count 0]
             #:result count)
            ([line (file->lines input)])
    (define depth (string->number line))
    (values depth (if (and previous (> depth previous)) (add1 count) count))))
