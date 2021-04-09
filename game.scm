(use-modules (chickadee)
             (system repl coop-server)
             (chickadee math rect)
             (chickadee math vector)
             (chickadee graphics font)
             (chickadee graphics texture)
             (chickadee graphics sprite))

(define repl (spawn-coop-repl-server))

(define MOVE-STEP 5)
(define SCREEN-WIDTH 400)
(define SCREEN-HEIGHT 600)

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
  (if (and (equal? key 'space) (not repeat?))
      (put-fireball player-position))
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

(define (move-upwards coords-vector)
  (vec2+ coords-vector (vec2 0 MOVE-STEP)))

(define (outside-view coords-vector)
  (let ((view-rect (rect 0 0 SCREEN-WIDTH SCREEN-HEIGHT)))
    (rect-contains-vec2? view-rect coords-vector)))

(define (move-fireballs)
  (set! fireballs (map move-upwards
                       (filter outside-view fireballs))))

(define (move-player!)
  (let ((move-left? (assoc-ref keys 'left))
        (move-right? (assoc-ref keys 'right))
        (move-up? (assoc-ref keys 'up))
        (move-down? (assoc-ref keys 'down)))
    (set! player-position (vec2+ player-position (player-move-delta move-left?
                                                                    move-right?
                                                                    move-up?
                                                                    move-down?)))))
(define (draw-debug)
  (let ((text (cond ((assoc-ref keys 'left) "left")
                    ((assoc-ref keys 'right) "right")
                    ((assoc-ref keys 'space) "space")
                    (else "none"))))

    (draw-text text (vec2 260.0 240.0))
    (draw-text (string-append "Fireballs: "
                              (number->string (length fireballs))) (vec2 280.0 290.0))))

(define (draw alpha)
  (move-player!)
  (draw-fireballs)
  (move-fireballs)
  (draw-debug)
  (draw-sprite enemy-ship-sprite (vec2 200 290))
  (draw-sprite player-ship-sprite player-position))

(define (update dt)
  (poll-coop-repl-server repl))

(run-game
 #:load load
 #:update update
 #:window-width SCREEN-WIDTH
 #:window-height SCREEN-HEIGHT
 #:key-press key-press
 #:key-release key-release
 #:draw draw)
