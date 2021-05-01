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
(define ROCKET-BLAST-RADIUS 150)
(define ROCKET-ARM-PERIOD 300)

(define enemy-ships-textures-atlas #f)
(define player-ship-sprite #f)
(define fireball #f)
(define explosion-atlas #f)

(define large-explosion-atlas #f)

(define background-map #f)

(define rocket-texture #f)

(define agenda-dt 1)

(define game-over #f)
(define player-position (vec2 300 0))

(define enemy-ships '())
(define fireballs '())
(define explosions '())
(define rocket #f)

(define rocket-last-shot-time 0)

(define keys (list (cons 'left #f)
                   (cons 'right #f)
                   (cons 'up #f)
                   (cons 'down #f)
                   (cons 'space #f)
                   (cons 'm #f)))

(define (key-press key modifiers repeat?)
  (if (and (equal? key 'space) (not repeat?))
      (put-fireball player-position))
  (if (and (equal? key 'm) (not repeat?))
      (fire-rocket))
  (assoc-set! keys key #t))

(define (key-release key modifiers)
  (assoc-set! keys key #f))

(define (load)
  (set! enemy-ships-textures-atlas (load-tileset "graphics/enemy-ships.png" 16 16))
  (set! player-ship-sprite (load-image "graphics/ship.png"))
  (set! fireball (load-image "graphics/fire.png"))

  (set! rocket-texture (load-image "graphics/rocket.png"))
  
  (set! background-map (load-image "graphics/space_dn.png"))
  (set! explosion-atlas (load-tileset "graphics/explosion.png" 128 128))
  (set! large-explosion-atlas (load-tileset "graphics/explosion-large.png" 256 256)))

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

(define-record-type <rocket-weapon>
  (make-rocket-weapon position remaining-fuel)
  rocket?
  (position rocket-position rocket-position-set!)
  (remaining-fuel rocket-fuel rocket-fuel-set!))

(define (fire-rocket)
  (let ((rocket-not-fired (nil? rocket))
        (rocket-available (> (- (agenda-time) rocket-last-shot-time) ROCKET-ARM-PERIOD)))

    (if (and rocket-not-fired rocket-available)
        (begin
          (put-rocket player-position)
          (set! rocket-last-shot-time (agenda-time))))))

(define (put-rocket position)
  (set! rocket (make-rocket-weapon position 250))) 

(define (draw-rocket)
  (if rocket
      (draw-sprite rocket-texture (rocket-position rocket))))

(define (enemies-within-radius center radius)
  
  (filter
   (lambda (enemy)
     (let* ((position (enemy-ship-position enemy))
            (enemy-x (vec2-x position))
            (enemy-y (vec2-y position))
            (center-x (vec2-x center))
            (center-y (vec2-y center))
            (distance-from-center (sqrt
                                   (+
                                    (expt (- enemy-x center-x) 2)
                                    (expt (- enemy-y center-y) 2)))))
       (< distance-from-center radius)))

   enemy-ships))

(define (explode-rocket)
  (add-large-explosion (rocket-position rocket))
  (let ((hit-enemies (enemies-within-radius (rocket-position rocket) ROCKET-BLAST-RADIUS)))
    (explode-enemies hit-enemies)))

(define (move-rocket)
  (if rocket
      (let ((current-position (rocket-position rocket)))
        (if (> (rocket-fuel rocket) 0)
            (begin
              (rocket-position-set! rocket (move-upwards current-position))
              (rocket-fuel-set! rocket (- (rocket-fuel rocket) MOVE-STEP)))

            (begin
              (explode-rocket)
              (set! rocket #f))
      ))))

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
  (make-enemy-ship bezier-path current-t sprite-index)
  enemy-ship?
  (bezier-path enemy-ship-path)
  (current-t enemy-ship-path-t enemy-ship-path-t-set!)
  (sprite-index enemy-ship-sprite-index))

(define (enemy-ship-sprite index)
  (texture-atlas-ref enemy-ships-textures-atlas index))

(define (enemy-ship-position ship)
  (bezier-curve-point-at
   (enemy-ship-path ship) (enemy-ship-path-t ship)))

(define (spawn-enemy)
  (let ((ship (make-enemy-ship (make-enemy-path) 0 (random 5))))
      (set! enemy-ships (cons ship enemy-ships))))

(define (spawn-enemies)
  (forever
   (spawn-enemy)
   (sleep (+ 1 (random 100)))))

(spawn-script spawn-enemies)

(define (hud-rocket-progress)
  (let* ((time-since-last-shot (- (agenda-time) rocket-last-shot-time))
         (time-progress (cond
                         ((= 0 time-since-last-shot) 1)
                         ((> time-since-last-shot ROCKET-ARM-PERIOD) ROCKET-ARM-PERIOD)
                         ((<= time-since-last-shot ROCKET-ARM-PERIOD) time-since-last-shot))))
    (* 42 (/ time-progress ROCKET-ARM-PERIOD)))) 

(define (draw-hud)
  (let ((bar-color (make-color 1.0 1.0 1.0 0.5))
        (bar-x (- SCREEN-WIDTH 60))
        (bar-y 20)
        (progress (hud-rocket-progress)))
    (draw-sprite rocket-texture
                 (vec2 (- bar-x 15) bar-y)
                 #:scale (vec2 0.7 0.7)
                 #:rotation 0.9) 
    (draw-canvas
     (make-canvas
      (with-style
       ((stroke-color bar-color)
        (stroke-width 2.0)
        (fill-color bar-color))
       (superimpose
        (stroke
         (rounded-rectangle (vec2 bar-x bar-y) 50 14))
        (fill
         (rectangle (vec2 (+ bar-x 4) (+ bar-y 4)) progress 6))))))))


(define (draw-debug)
  (if (not (nil? rocket))
      (draw-canvas
       (make-canvas
        (with-style ((stroke-color green)
                     (stroke-width 4.0)
                     (fill-color green))
                    (stroke
                      (circle (rocket-position rocket) ROCKET-BLAST-RADIUS))))))
  
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
    (draw-text (string-append "Rocket last shot time: "
                              (number->string rocket-last-shot-time)) (vec2 190.0 300.0))
    (draw-text (string-append "Fireballs: "
                              (number->string (length fireballs))) (vec2 280.0 290.0))))

(define (draw-enemies)
  (for-each
   
   (lambda (ship)
     (let* ((ship-position (enemy-ship-position ship))
            (sprite (enemy-ship-sprite (enemy-ship-sprite-index ship))))
       (draw-sprite sprite ship-position)))
     
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

(define (explode-enemies enemies)
  (for-each
   (lambda (enemy)
     (set! enemy-ships (delete enemy enemy-ships))
     (add-explosion (enemy-ship-position enemy)))
   enemies))

(define (clear-hit-enemies!)
  (for-each
   (lambda (fireball)
     (let ((hit-enemies (enemy-fireball-collision fireball)))
       (if (not (nil? hit-enemies))
           (set! fireballs (delete fireball fireballs)))

       (explode-enemies hit-enemies)))
     
   fireballs))

(define (clear-hit-enemies-script)
  (forever
   (clear-hit-enemies!)
   (sleep 1)))

(spawn-script clear-hit-enemies-script)

(define-record-type <explosion>
  (make-explosion position frame texture-atlas)
  explosion?
  (position explosion-position explosion-position-set!)
  (frame explosion-frame explosion-frame-set!)
  (texture-atlas explosion-texture-atlas))

(define (add-explosion position)
  (set! explosions (cons (make-explosion position 0 explosion-atlas) explosions)))

(define (add-large-explosion position)
  (set! explosions (cons (make-explosion position 0 large-explosion-atlas) explosions)))

(define (clear-old-explosions!)
  (set! explosions
    (filter
     (lambda (explosion)
       (< (explosion-frame explosion) 63))
     explosions)))

(define (draw-explosions!)
  (if (not (nil? explosions))
      (for-each
       (lambda (explosion)
         (let* ((texture-atlas (explosion-texture-atlas explosion))
                (draw-offset (if (eq? texture-atlas large-explosion-atlas)
                                 (vec2 128 128)
                                 (vec2 64 64)))
                (original-position (explosion-position explosion))
                (position (vec2- original-position draw-offset))
                (atlas-index (explosion-frame explosion)))
           (draw-sprite (texture-atlas-ref texture-atlas atlas-index) position)
           (explosion-frame-set! explosion (+ 1 atlas-index))))

       explosions)))

(at 1
    (script
     (wait-until (player-collides?))
     (set! game-over #t)
     ((explode player-position))))

(define (draw alpha)
  (draw-sprite background-map (vec2 0 0) #:rect (rect 0 0 SCREEN-WIDTH SCREEN-HEIGHT))
  (move-player!)
  (draw-fireballs)
  (move-fireballs)

  (draw-rocket)
  (move-rocket)

  (draw-hud)
  ;(draw-debug)
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
