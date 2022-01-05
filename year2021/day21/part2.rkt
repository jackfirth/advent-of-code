#lang racket


(require rebellion/collection/multiset
         rebellion/private/guarded-block)


(module+ test
  (require rackunit))


;@----------------------------------------------------------------------------------------------------


;; A multiset representing the possible outcomes of a Dirac die.
(define die-outcomes
  (for*/multiset ([a (in-range 1 4)]
                  [b (in-range 1 4)]
                  [c (in-range 1 4)])
    (+ a b c)))


(struct player-state (position score) #:transparent)


(define possible-player-states
  (vector->immutable-vector
   (for*/vector ([position (in-range 0 10)]
                 [score (in-range 0 21)])
     (player-state position score))))


(struct game-state (player1 player2) #:transparent)


(define possible-game-states
  (vector->immutable-vector
   (for*/vector ([player1 (in-vector possible-player-states)]
                 [player2 (in-vector possible-player-states)])
     (game-state player1 player2))))


(define (game-over? state)
  (or (player-won? (game-state-player1 state)) (player-won? (game-state-player2 state))))


(define (player-won? player)
  (>= (player-state-score player) 21))


(struct game-outcome (player1-score player2-score) #:transparent)


;; A hash mapping player states to multisets representing the possible next player states
(define possible-next-player-states
  (for*/hash ([current-position (in-range 0 10)]
              [current-score (in-range 0 21)])
    (define state (player-state current-position current-score))
    (define possibilities
      (for/fold ([possibilities empty-multiset])
                ([(die-outcome count) (in-hash (multiset-frequencies die-outcomes))])
        (define next-position (modulo (+ current-position die-outcome) 10))
        (define next-score (+ current-score next-position 1))
        (multiset-add possibilities (player-state next-position next-score) #:copies count)))
    (values state possibilities)))


;; A hash mapping game states to multisets representing the possible next game states after player 1
;; moves.
(define possible-next-game-states/player1
  (for*/hash ([state (in-vector possible-game-states)])
    (define possibilities
      (for/fold ([possibilities empty-multiset])
                 ([(next-player1 player1-count)
                   (in-hash
                    (multiset-frequencies
                     (hash-ref possible-next-player-states (game-state-player1 state))))])
        (define next (game-state next-player1 (game-state-player2 state)))
        (multiset-add possibilities next #:copies player1-count)))
    (values state possibilities)))


;; A hash mapping game states to multisets representing the possible next game states after player 2
;; moves.
(define possible-next-game-states/player2
  (for*/hash ([state (in-vector possible-game-states)])
    (define possibilities
      (for*/fold ([possibilities empty-multiset])
                 ([(next-player2 player2-count)
                   (in-hash
                    (multiset-frequencies
                     (hash-ref possible-next-player-states (game-state-player2 state))))])
        (define next (game-state (game-state-player1 state) next-player2))
        (multiset-add possibilities next #:copies player2-count)))
    (values state possibilities)))


(define (possible-next-nonterminal-game-states player1?)
  (for*/hash ([state (in-vector possible-game-states)])
    (define possible-next-states
      (hash-ref
       (if player1? possible-next-game-states/player1 possible-next-game-states/player2) state))
    (define possibilities
      (for*/fold ([possibilities empty-multiset])
                 ([(next count) (in-hash (multiset-frequencies possible-next-states))]
                  #:unless (game-over? next))
        (multiset-add possibilities next #:copies count)))
    (values state possibilities)))


(define possible-next-nonterminal-game-states/player1 (possible-next-nonterminal-game-states #true))
(define possible-next-nonterminal-game-states/player2 (possible-next-nonterminal-game-states #false))


(define (possible-next-game-outcomes player1?)
  (for*/hash ([state (in-vector possible-game-states)])
    (define possible-next-states
      (hash-ref
       (if player1? possible-next-game-states/player1 possible-next-game-states/player2) state))
    (define possibilities
      (for*/fold ([possibilities empty-multiset])
                 ([(next count) (in-hash (multiset-frequencies possible-next-states))]
                  #:when (game-over? next))
        (match-define (game-state (player-state _ p1score) (player-state _ p2score)) next)
        (multiset-add possibilities (game-outcome p1score p2score) #:copies count)))
    (values state possibilities)))


(define possible-next-game-outcomes/player1 (possible-next-game-outcomes #true))
(define possible-next-game-outcomes/player2 (possible-next-game-outcomes #false))


(define (multiset-replicate set copies)
  (for/fold ([replicated empty-multiset])
            ([(element original-count) (in-hash (multiset-frequencies set))])
    (multiset-add replicated element #:copies (* original-count copies))))
    


(define (initial-game-state player1-start-position player2-start-position)
  (game-state
   (player-state (sub1 player1-start-position) 0) (player-state (sub1 player2-start-position) 0)))


(define (possible-game-outcomes game)
  (let loop
      ([possible-states (multiset game)]
       [possible-outcomes (multiset)])
    (guarded-block
     (guard (zero? (multiset-size possible-states)) then
       possible-outcomes)
     (define (compute-next player1? possible-states possible-outcomes)
       (for*/fold ([next-possible-outcomes possible-outcomes]
                   [next-possible-states empty-multiset])
                  ([(state count) (in-hash (multiset-frequencies possible-states))])
         (define outcomes
           (hash-ref
            (if player1? possible-next-game-outcomes/player1 possible-next-game-outcomes/player2)
            state))
         (define
           next-states
           (hash-ref
            (if player1?
                possible-next-nonterminal-game-states/player1
                possible-next-nonterminal-game-states/player2)
            state))
         (values (multiset-add-all next-possible-outcomes (multiset-replicate outcomes count))
                 (multiset-add-all next-possible-states (multiset-replicate next-states count)))))
     (define-values (player1-possible-outcomes player1-possible-states)
       (compute-next #true possible-states possible-outcomes))
     (guard (zero? (multiset-size player1-possible-states)) then
       player1-possible-outcomes)
     (define-values (next-possible-outcomes next-possible-states)
       (compute-next #false player1-possible-states player1-possible-outcomes))
     (loop next-possible-states next-possible-outcomes))))


(define (possible-game-winners game)
  (for/fold ([possibilities empty-multiset])
            ([(outcome count) (in-hash (multiset-frequencies (possible-game-outcomes game)))])
    (define winner (if (>= (game-outcome-player1-score outcome) 21) 'player1 'player2))
    (multiset-add possibilities winner #:copies count)))


(define (solve-puzzle player1-start-position player2-start-position)
  (define outcomes
    (possible-game-winners (initial-game-state player1-start-position player2-start-position)))
  (max (multiset-frequency outcomes 'player1) (multiset-frequency outcomes 'player2)))


(module+ test
  (define winners (possible-game-winners (initial-game-state 4 8)))
  (check-equal? (multiset-frequency winners 'player1) 444356092776315)
  (check-equal? (multiset-frequency winners 'player2) 341960390180808)
  (check-equal? (solve-puzzle 4 8) 444356092776315))


(module+ main
  (solve-puzzle 5 10))
