(define-module (player)
  #:use-module (chickadee math vector)
  #:use-module (util)

  #:export (player-position
            player-ship-upgrades
            score
            reset-player-position))

(define INITIAL-PLAYER-POSITION (vec2 (/ SCREEN-WIDTH 2) 20))

(define player-position INITIAL-PLAYER-POSITION)
(define score 0)
(define player-ship-upgrades '())

(define (reset-player-position)
  (set! player-position INITIAL-PLAYER-POSITION))
