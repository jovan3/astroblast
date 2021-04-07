(use-modules (chickadee)
             (system repl coop-server)
             (chickadee math vector)
             (chickadee graphics font)
             (chickadee graphics texture)
             (chickadee graphics sprite))

(define repl (spawn-coop-repl-server))

(define MOVE-STEP 5)

(define textures-atlas #f)
(define enemy-ship-sprite #f)
(define player-ship-sprite #f)
(define fireball #f)

(define player-position (vec2 300 0))

(define fireballs '())

(define keys (list (cons 'left #f)
                   (cons 'right #f)
                   (cons 'up #f)
                   (cons 'down #f)
                   (cons 'space #f)))

(define (key-press key modifiers repeat?)
  (assoc-set! keys key #t))

(define (key-release key modifiers)
  (assoc-set! keys key #f))

(define (load)
  (set! textures-atlas (load-tileset "enemy-ships.png" 16 16))
  (set! enemy-ship-sprite (texture-atlas-ref textures-atlas 0))
  (set! player-ship-sprite (load-image "ship.png"))
  (set! fireball (load-image "fire.png")))

(define (player-move-delta left? right? up? down?)
  (let ((x (cond (left? (- MOVE-STEP))
                 (right? MOVE-STEP)
                 (else 0)))
        (y (cond (up? MOVE-STEP)
                 (down? (- MOVE-STEP))
                 (else 0))))
    (vec2 x y)))

(define (put-fireball position)
  (set! fireballs (cons position fireballs)))

(define (draw-fireballs)
  (map (lambda (ball) (draw-sprite fireball ball)) fireballs))

(define (move-fireballs)
  (set! fireballs (map (lambda (ball) (vec2+ ball (vec2 0 MOVE-STEP))) fireballs)))

(define (draw alpha)
  (let ((text (cond ((assoc-ref keys 'left) "left")
                    ((assoc-ref keys 'right) "right")
                    ((assoc-ref keys 'space) "space")
                    (else "none")))
        (move-left? (assoc-ref keys 'left))
        (move-right? (assoc-ref keys 'right))
        (move-up? (assoc-ref keys 'up))
        (move-down? (assoc-ref keys 'down))
        (fire? (assoc-ref keys 'space)))
    (set! player-position (vec2+ player-position (player-move-delta move-left?
                                                                    move-right?
                                                                    move-up?
                                                                    move-down?)))
    (if fire? (put-fireball player-position))
    (draw-fireballs)
    (move-fireballs)
    (draw-text text (vec2 260.0 240.0))
    (draw-sprite enemy-ship-sprite (vec2 200 290))
    (draw-sprite player-ship-sprite player-position)))

(define (update dt)
  (poll-coop-repl-server repl))

(run-game
 #:load load
 #:update update
 #:window-width 400
 #:window-height 600
 #:key-press key-press
 #:key-release key-release
 #:draw draw)
