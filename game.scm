(use-modules (chickadee)
             (srfi srfi-9)
             (system repl coop-server)
             (chickadee math rect)
             (chickadee math vector)
             (chickadee math bezier)
             (chickadee math easings)
             (chickadee graphics font)
             (chickadee graphics texture)
             (chickadee graphics color)
             (chickadee graphics sprite)
             (chickadee graphics path)
             (chickadee scripting))

(set! *random-state* (seed->random-state (current-time)))

(define repl (spawn-coop-repl-server))

(define MOVE-STEP 5)
(define SCREEN-WIDTH 400)
(define SCREEN-HEIGHT 600)

(define textures-atlas #f)
(define enemy-ship-sprite #f)
(define player-ship-sprite #f)
(define fireball #f)
(define explosion-atlas #f)

(define agenda-dt 1)

(define game-over #f)
(define player-position (vec2 300 0))

(define enemy-ships '())
(define fireballs '())
(define explosions '())

(define keys (list (cons 'left #f)
                   (cons 'right #f)
                   (cons 'up #f)
                   (cons 'down #f)
                   (cons 'space #f)))

(define (key-press key modifiers repeat?)
  (if (and (equal? key 'space) (not repeat?))
      (put-fireball player-position))
      ;(spawn-script (explode (vec2 200 200))))
  (assoc-set! keys key #t))

(define (key-release key modifiers)
  (assoc-set! keys key #f))

(define (load)
  (set! textures-atlas (load-tileset "graphics/enemy-ships.png" 16 16))
  (set! enemy-ship-sprite (texture-atlas-ref textures-atlas 0))
  (set! player-ship-sprite (load-image "graphics/ship.png"))
  (set! fireball (load-image "graphics/fire.png"))

  (set! explosion-atlas (load-tileset "graphics/explosion.png" 128 128)))

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

(define (make-enemy-path)
  (let ((start-x (random SCREEN-WIDTH))
        (start-y SCREEN-HEIGHT)
        (p1-x (random SCREEN-WIDTH))
        (p1-y 100)
        (p2-x (random SCREEN-WIDTH))
        (p2-y 300)
        (p3-x (random SCREEN-WIDTH))
        (p3-y 0))

    (make-bezier-curve
     (vec2 start-x start-y)
     (vec2 p1-x p1-y)
     (vec2 p2-x p2-y)
     (vec2 p3-x p3-y))))          

(define enemy-path
  (let ((start-x (/ SCREEN-WIDTH 2))
        (start-y SCREEN-HEIGHT))
    (make-bezier-curve
     (vec2 start-x start-y)
     (vec2 (- start-x 300) (- start-y 100))
     (vec2 (+ start-x 300) (- start-y 300))
     (vec2 start-x (- start-y SCREEN-HEIGHT)))))

(define-record-type <enemy-ship>
  (make-enemy-ship bezier-path current-t)
  enemy-ship?
  (bezier-path enemy-ship-path)
  (current-t enemy-ship-path-t enemy-ship-path-t-set!))

(define (enemy-ship-position ship)
  (bezier-curve-point-at
   (enemy-ship-path ship) (enemy-ship-path-t ship)))

(define (spawn-enemy)
  (let ((ship (make-enemy-ship (make-enemy-path) 0)))
      (set! enemy-ships (cons ship enemy-ships))))

(define (spawn-enemies)
  (forever
   (spawn-enemy)
   (sleep (+ 1 (random 100)))))

(spawn-script spawn-enemies)

(define (draw-debug)
  (if (not (nil? enemy-ships))
      (let ((enemy-path (enemy-ship-path (car enemy-ships))))
        (draw-canvas
         (make-canvas
          (with-style ((stroke-color green)
                       (stroke-width 4.0)
                       (fill-color green))
                      (stroke
                       (path
                        (move-to (bezier-curve-p0 enemy-path))
                        (bezier-to
                         (bezier-curve-p1 enemy-path)
                         (bezier-curve-p2 enemy-path)
                         (bezier-curve-p3 enemy-path)))))))))
  
  (let ((text (cond ((assoc-ref keys 'left) "left")
                    ((assoc-ref keys 'right) "right")
                    ((assoc-ref keys 'space) "space")
                    (else "none"))))

    (draw-text (number->string (agenda-time)) (vec2 220 220))
    (draw-text text (vec2 260.0 240.0))
    (draw-text (string-append "Fireballs: "
                              (number->string (length fireballs))) (vec2 280.0 290.0))))

(define (draw-enemies)
  (for-each
   
   (lambda (ship)
     (let ((ship-position (enemy-ship-position ship)))
           (draw-sprite enemy-ship-sprite ship-position)))
     
     enemy-ships))

(define (move-enemies)
  (forever
   (for-each
    (lambda (enemy)
      (let ((t (enemy-ship-path-t enemy)))
        (enemy-ship-path-t-set! enemy (+ t 0.005)))) enemy-ships)
   (sleep 1)))

(spawn-script move-enemies)

(define (player-collides?)
  (let ((ship-collisions
         (filter
          (lambda (enemy)
            (let* ((enemy-position (enemy-ship-position enemy))
                   (enemy-rect (rect (vec2-x enemy-position)
                                     (vec2-y enemy-position)
                                     16 16))
                   (player-rect (rect (vec2-x player-position)
                                      (vec2-y player-position)
                                      32 24)))

              (rect-intersects? enemy-rect player-rect)))

          enemy-ships)))

    (not (nil? ship-collisions))))

(define (explode position)
  (lambda ()
    (let ((new-position (vec2- position (vec2 64 64))))
      (tween 100 0 63
             (lambda (index)
               (let ((sprite (inexact->exact (floor index))))
                 (draw-sprite
                  (texture-atlas-ref explosion-atlas sprite) new-position)))))))

(define (enemy-fireball-collision fireball)
  (filter
   (lambda (enemy)
     (let* ((enemy-position (enemy-ship-position enemy))
            (enemy-rect (rect (vec2-x enemy-position)
                              (vec2-y enemy-position)
                              16 16))
            (fireball-rect (rect (vec2-x fireball)
                                 (vec2-y fireball)
                                 4 3)))

       (rect-intersects? enemy-rect fireball-rect)))

   enemy-ships))

(define (clear-hit-enemies!)
  (for-each
   (lambda (fireball)
     (let ((hit-enemies (enemy-fireball-collision fireball)))

       (for-each
        (lambda (hit-enemy)
          (set! enemy-ships (delete hit-enemy enemy-ships))
          (set! fireballs (delete fireball fireballs))
          (add-explosion fireball))
        hit-enemies)))
   fireballs))

(define (clear-hit-enemies-script)
  (forever
   (clear-hit-enemies!)
   (sleep 1)))

(spawn-script clear-hit-enemies-script)

(define (add-explosion position)
  (set! explosions (cons (list position 0) explosions)))

(define (clear-old-explosions!)
  (set! explosions
    (filter
     (lambda (explosion)
       (< (cadr explosion) 63))
     explosions)))

(define (draw-explosions!)
  (if (not (nil? explosions))
      (set! explosions
        (map
         (lambda (explosion)
           (let* ((original-position (car explosion))
                  (position (vec2- original-position (vec2 64 64)))
                  (atlas-index (cadr explosion)))
             (draw-sprite (texture-atlas-ref explosion-atlas atlas-index) position)
             (list original-position (+ 1 atlas-index))))

         explosions))))

(at 1
    (script
     (wait-until (player-collides?))
     (set! game-over #t)
     ((explode player-position))))

(define (draw alpha)
  (move-player!)
  (draw-fireballs)
  (move-fireballs)
  ;(draw-debug)
  ;(current-agenda game-world-agenda)
  (update-agenda agenda-dt)
  
  (draw-enemies)
  (clear-old-explosions!)
  (draw-explosions!)
  (if (not game-over)
      (draw-sprite player-ship-sprite player-position)))

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
