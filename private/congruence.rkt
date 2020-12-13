#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [congruence (-> exact-integer? exact-positive-integer? congruence?)]
  [congruence? predicate/c]
  [congruence-remainder (-> congruence? exact-integer?)]
  [congruence-modulus (-> congruence? exact-positive-integer?)]
  [solve-congruences (-> (sequence/c congruence?) exact-nonnegative-integer?)]
  [congruences-into-solution (reducer/c congruence? exact-nonnegative-integer?)]))

(require math/number-theory
         racket/contract/base
         racket/sequence
         rebellion/collection/list
         rebellion/private/static-name
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           racket/set
           rackunit))

;@--------------------------------------------------------------------------------------------------

(struct congruence (remainder modulus)
  #:guard (λ (remainder modulus _) (values (modulo remainder modulus) modulus))
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (define remainder (congruence-remainder this))
     (define modulus (congruence-modulus this))
     (write-string (format "#<~a: x ≡ ~a (mod ~a)>" (name congruence) remainder modulus) out))])

(module+ test
  (test-case (name-string congruence)
    (check-equal? (congruence 3 5) (congruence 13 5))
    (check-equal? (congruence 3 5) (congruence -2 5))))

(define (solve-congruences congruences)
  (define congruence-list (sequence->list congruences))
  (define remainders (map congruence-remainder congruence-list))
  (define moduli (map congruence-modulus congruence-list))
  (solve-chinese remainders moduli))

(define congruences-into-solution
  (reducer-map into-list #:range solve-congruences))

(module+ test
  (test-case (name-string solve-congruences)
    (check-equal? (solve-congruences (set (congruence 2 3) (congruence 3 5) (congruence 2 7))) 23)))
