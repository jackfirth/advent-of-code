#lang racket


(require racket/runtime-path
         rebellion/collection/vector
         rebellion/streaming/reducer
         rebellion/streaming/transducer)


;@----------------------------------------------------------------------------------------------------


(define-runtime-path input "input.txt")

(define windows
  (transduce (file->lines input)
             (mapping string->number)
             (windowing 3 #:into into-sum)
             #:into (into-vector)))


(module+ main
  (for/fold ([previous #false]
             [count 0]
             #:result count)
            ([depth (in-vector windows)])
    (values depth (if (and previous (> depth previous)) (add1 count) count))))
