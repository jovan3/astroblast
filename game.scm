(use-modules (chickadee)
             (srfi srfi-9)
             (system repl coop-server)

             (chickadee math rect)
             (chickadee math vector)
             (chickadee math easings)
             (chickadee math bezier)
             
             (chickadee graphics font)
             (chickadee graphics texture)
             (chickadee graphics color)
             (chickadee graphics sprite)
             (chickadee graphics path)

             (chickadee scripting)

             (util)
             (enemies)
             (graphics))

(set! *random-state* (seed->random-state (current-time)))

(define repl (spawn-coop-repl-server))

(define MOVE-STEP 5)
(define ROCKET-BLAST-RADIUS 400)
(define ROCKET-ARM-PERIOD 300)
(define ENEMY-FIREBAL-SPEED 3)
(define FIREBALL-OFFSET (vec2 16 0))
(define INITIAL-PLAYER-POSITION (vec2 (/ SCREEN-WIDTH 2) 20))

(define hud-color (make-color 1.0 1.0 1.0 0.5))
(define upgrade-color hud-color)

(define agenda-dt 1)

(define game-over #f)
(define player-position INITIAL-PLAYER-POSITION)

(define fireballs '())
(define enemy-fireballs '())

(define explosions '())
(define rocket #f)

(define rocket-last-shot-time 0)

(define score 0)
(define player-ship-upgrades '())
(define flying-upgrades '())

(define-record-type <rocket-weapon>
  (make-rocket-weapon position remaining-fuel)
  rocket?
  (position rocket-position rocket-position-set!)
  (remaining-fuel rocket-fuel rocket-fuel-set!))

(define-record-type <explosion>
  (make-explosion position frame texture-atlas)
  explosion?
  (position explosion-position explosion-position-set!)
  (frame explosion-frame explosion-frame-set!)
  (texture-atlas explosion-texture-atlas))

(define-record-type <fireball>
  (make-fireball position direction)
  fireball?
  (position fireball-position fireball-position-set!)
  (direction fireball-direction))

(define-record-type <upgrade>
  (make-upgrade position type)
  upgrade?
  (position upgrade-position upgrade-position-set!)
  (type upgrade-type))

(define keys (list (cons 'left #f)
                   (cons 'right #f)
                   (cons 'up #f)
                   (cons 'down #f)
                   (cons 'space #f)
                   (cons 'm #f)))

(define (key-press key modifiers repeat?)
  (if (and (equal? key 'space) (not repeat?))
      (fire (vec2+ FIREBALL-OFFSET player-position)))
  (if (and (equal? key 'm) (not repeat?))
      (fire-rocket))
  (if (and (equal? key 'r))
      (reset-game))
  (assoc-set! keys key #t))

(define (key-release key modifiers)
  (assoc-set! keys key #f))

(define (load)
  (load-textures))

(define (reset-game)
  (if game-over
      (begin
        (current-agenda (make-agenda))
        (spawn-scripts)
        (set! player-position INITIAL-PLAYER-POSITION)
        (set! enemy-ships '())
        (set! enemy-fireballs '())
        (set! agenda-dt 1)
        (set! score 0)
        (set! player-ship-upgrades '())
        (set! game-over #f))))

(define (player-move-delta left? right? up? down?)
  (let ((x (cond (left? (- MOVE-STEP))
                 (right? MOVE-STEP)
                 (else 0)))
        (y (cond (up? MOVE-STEP)
                 (down? (- MOVE-STEP))
                 (else 0))))
    (vec2 x y)))

(define (fire position)
  (if (member 'multi-fireballs player-ship-upgrades)
      (put-multi-fireballs position)
      (put-fireball position)))

(define (put-fireball position)
  (let ((new-fireball (make-fireball position (vec2 0 1))))
    (set! fireballs (cons new-fireball fireballs))))

(define (put-multi-fireballs position)
  (for-each
   (lambda (direction)
     (let ((new-fireball (make-fireball position direction)))
       (set! fireballs (cons new-fireball fireballs))))
   (list (vec2 0 1) (vec2 0.03 1) (vec2 -0.03 1))))

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

(define (explode-rocket)
  (add-large-explosion (rocket-position rocket))
  (let ((hit-enemies (enemies-within-radius (rocket-position rocket) ROCKET-BLAST-RADIUS)))
    (explode-enemies hit-enemies)))

(define (move-rocket)
  (if rocket
      (if (> (rocket-fuel rocket) 0)
          (begin
            (move-upwards rocket rocket-position rocket-position-set!)
            (rocket-fuel-set! rocket (- (rocket-fuel rocket) MOVE-STEP)))

          (begin
            (explode-rocket)
            (set! rocket #f)))))

(define (draw-fireballs)
  (for-each
   (lambda (ball)
     (draw-sprite fireball-texture (fireball-position ball)))
   fireballs))

(define (draw-enemy-fireballs)
  (for-each
   (lambda (ball)
     (draw-sprite enemy-fireball-texture (fireball-position ball)))
   enemy-fireballs))

(define (move-upwards object position-fn position-set-fn)
  (let ((coordinates (position-fn object)))
    (position-set-fn object (vec2+ coordinates (vec2 0 MOVE-STEP)))
    object))

(define (move-fireballs)
  (let* ((inside-view? (object-inside-view-fn fireball-position))
         (fireballs-inside-view (filter inside-view? fireballs)))
         
    (set! fireballs (map move-enemy-fireball fireballs-inside-view))))

(define (move-enemy-fireball fireball)
  (let* ((position (fireball-position fireball))
         (direction (fireball-direction fireball)))
    (fireball-position-set! fireball
                            (vec2+ position (vec2* direction ENEMY-FIREBAL-SPEED)))
    fireball))

(define (move-enemy-fireballs)
  (let ((inside-view? (object-inside-view-fn fireball-position)))
    (set! enemy-fireballs
          (map move-enemy-fireball (filter inside-view? enemy-fireballs)))))

(define (move-player!)
  (let ((move-left? (assoc-ref keys 'left))
        (move-right? (assoc-ref keys 'right))
        (move-up? (assoc-ref keys 'up))
        (move-down? (assoc-ref keys 'down)))
    (set! player-position (vec2+ player-position (player-move-delta move-left?
                                                                    move-right?
                                                                    move-up?
                                                                    move-down?)))))          

(define (enemy-ship-rect ship)
  (let ((enemy-position (enemy-ship-position ship)))
    (rect (vec2-x enemy-position)
          (vec2-y enemy-position)
          16 16)))

(define (enemy-fireball-rect fireball)
  (let ((position (fireball-position fireball)))
    (rect (vec2-x position)
          (vec2-y position)
          8 8)))

(define (upgrade-rect upgrade)
  (let ((position (upgrade-position upgrade)))
    (rect (vec2-x position)
          (vec2-y position)
          32 32)))



(define (spawn-enemies)
  (forever
   (spawn-enemy)
   (sleep (+ 1 (random 100)))))

(define (spawn-enemy-fireball)
  (for-each
   (lambda (ship)
     (let* ((position (enemy-ship-position ship))
            (fireball-direction (vec2-normalize (vec2- player-position position)))
            (fireball (make-fireball position fireball-direction)))

       (set! enemy-fireballs (cons fireball enemy-fireballs))))

   enemy-ships))

(define (spawn-enemy-fireballs)
  (forever
   (spawn-enemy-fireball)
   (sleep 20)
   (spawn-enemy-fireball)
   (sleep 20)
   (spawn-enemy-fireball)
   (sleep 100)))


(define (hud-rocket-progress)
  (let* ((time-since-last-shot (- (agenda-time) rocket-last-shot-time))
         (time-progress (cond
                         ((= 0 time-since-last-shot) 1)
                         ((> time-since-last-shot ROCKET-ARM-PERIOD) ROCKET-ARM-PERIOD)
                         ((<= time-since-last-shot ROCKET-ARM-PERIOD) time-since-last-shot))))
    (* 42 (/ time-progress ROCKET-ARM-PERIOD)))) 

(define (draw-hud-rocket-progress)
  (let ((bar-color hud-color)
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

(define (draw-score)
  (let ((text (string-append "Score: " (number->string score)))
        (position (vec2 (- SCREEN-WIDTH 120) (- SCREEN-HEIGHT 20))))

    (draw-text text position
               #:color hud-color
               #:font game-font
               #:scale (vec2 0.3 0.3))))

(define (draw-hud)
  (draw-hud-rocket-progress)
  (draw-score))

(define (draw-game-end)
  (let ((text (string-append "Final score: \n" (number->string score)))
        (position (vec2 (- (/ SCREEN-WIDTH 2) 130) (- (/ SCREEN-HEIGHT 2) 0))))
    (draw-text text position
               #:font game-font
               #:color hud-color)))

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
      (let ((enemy-path (enemy-bezier-path (car enemy-ships))))
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


(define (object-collides-with-player? object-rect-fn)
  (lambda (object)
    (let* ((object-rect (object-rect-fn object))
           (player-rect (rect (vec2-x player-position)
                              (vec2-y player-position)
                              32 24)))

      (rect-intersects? object-rect player-rect))))

(define (player-collides?)
  (let* ((enemy-collides?
          (object-collides-with-player? enemy-ship-rect))
         (ship-collisions
          (filter enemy-collides? enemy-ships))

         (fireball-collides?
          (object-collides-with-player? enemy-fireball-rect))
         (fireball-collisions
          (filter fireball-collides? enemy-fireballs)))

    (or
     (not (nil? ship-collisions))
     (not (nil? fireball-collisions)))))

(define (enemy-fireball-collision fireball)
  (filter
   (lambda (enemy)
     (let* ((enemy-position (enemy-ship-position enemy))
            (fireball-position (fireball-position fireball))
            (enemy-rect (rect (vec2-x enemy-position)
                              (vec2-y enemy-position)
                              16 16))
            (fireball-rect (rect (vec2-x fireball-position)
                                 (vec2-y fireball-position)
                                 4 3)))

       (rect-intersects? enemy-rect fireball-rect)))

   enemy-ships))

(define (explode-enemies enemies)
  (for-each
   (lambda (enemy)
     (set! enemy-ships (delete enemy enemy-ships))
     (set! score (+ score (enemy-destroyed-points enemy)))
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

(define (upgrade-collect-script)
  (forever
   (let* ((player-touches-upgrade? (object-collides-with-player? upgrade-rect))
          (collected-upgrades (filter player-touches-upgrade? flying-upgrades)))
     (if (not (nil? collected-upgrades))
         (for-each
          (lambda (upgrade)
            (set! flying-upgrades (delete upgrade flying-upgrades))
            (set! player-ship-upgrades (cons (upgrade-type upgrade) player-ship-upgrades))
            (add-explosion player-position))
          collected-upgrades))
     (sleep 1))))

(define (move-upgrades)
  (forever
   (for-each
    (lambda (upgrade)
      (upgrade-position-set! upgrade (vec2+ (vec2 0 -1) (upgrade-position upgrade))))
    flying-upgrades)
      
   (sleep 1)))


(define (clear-upgrades-outside-view)
  (let ((upgrade-inside-view?
         (object-inside-view-fn (object-inside-view-fn
                                 (lambda (upgrade-item)
                                   (upgrade-position upgrade-item))))))
    (forever
     (set! flying-upgrades (filter upgrade-inside-view? flying-upgrades))
     (sleep 1))))

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

(define (draw-upgrade)
  (for-each
   (lambda (upgrade)
     (draw-sprite upgrade-texture (upgrade-position upgrade) #:tint upgrade-color))
   flying-upgrades))

(define (spawn-scripts)
  (spawn-script move-enemies)
  (spawn-script spawn-enemy-fireballs)
  (spawn-script clear-hit-enemies-script)
  (spawn-script clear-enemies-outside-view)
  (spawn-script upgrade-collect-script)
  (spawn-script move-upgrades)
  (spawn-script spawn-enemies)

  (at 1
    (script
     (forever
      (wait-until (player-collides?))
      (add-explosion player-position)
      (sleep 1)
      (set! game-over #t)
      (set! agenda-dt 0))))

  (at 1000
    (script
     (let* ((x-pos (random SCREEN-WIDTH))
            (new-upgrade (make-upgrade (vec2 x-pos (- SCREEN-HEIGHT 40)) 'multi-fireballs)))
     (set! flying-upgrades (cons new-upgrade flying-upgrades))))))

(spawn-scripts)

(define (draw alpha)
  (draw-sprite background-map (vec2 0 0) #:rect (rect 0 0 SCREEN-WIDTH SCREEN-HEIGHT))
  (move-player!)
  
  (draw-fireballs)
  (move-fireballs)

  (move-enemy-fireballs)
  (draw-enemy-fireballs)
  
  (draw-rocket)
  (move-rocket)

  (draw-upgrade)
  
  (draw-hud)
  ;(draw-debug)
  (update-agenda agenda-dt)

  (if game-over (draw-game-end))
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
