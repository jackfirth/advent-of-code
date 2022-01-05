#lang racket


(module+ test
  (require rackunit))


(struct game
  (player1-position player1-score player2-position player2-score player1-next? dice-roll-count)
  #:transparent)


(define (game-next-turn g)
  (match-define
    (game player1-position player1-score player2-position player2-score player1-next? dice-roll-count)
    g)
  (cond
    [player1-next?
     (define next (player-next-position player1-position dice-roll-count))
     (game next (+ player1-score next 1) player2-position player2-score #false (+ dice-roll-count 3))]
    [else
     (define next (player-next-position player2-position dice-roll-count))
     (game
      player1-position player1-score next (+ player2-score next 1) #true (+ dice-roll-count 3))]))


(define (player-next-position pos dice-roll-count)
  (modulo (+ pos (+ dice-roll-count 1) (+ dice-roll-count 2) (+ dice-roll-count 3)) 10))


(define (player1-won? g)
  (>= (game-player1-score g) 1000))


(define (player2-won? g)
  (>= (game-player2-score g) 1000))


(define (solve-puzzle player1-start player2-start)
  (let loop ([g (game (sub1 player1-start) 0 (sub1 player2-start) 0 #true 0)])
    (cond
      [(player1-won? g) (* (game-player2-score g) (game-dice-roll-count g))]
      [(player2-won? g) (* (game-player1-score g) (game-dice-roll-count g))]
      [else (loop (game-next-turn g))])))


(module+ test
  (check-equal? (solve-puzzle 4 8) 739785))


(module+ main
  (solve-puzzle 5 10))
