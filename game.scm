(use-modules (chickadee)
             (srfi srfi-9)
             (system repl coop-server)

             (chickadee math rect)
             (chickadee math vector)
             (chickadee math easings)
             (chickadee math bezier)
             
             (chickadee graphics font)
             (chickadee graphics texture)
             (chickadee graphics sprite)
             (chickadee graphics path)

             (chickadee scripting)

             (util)
             (enemies)
             (graphics)
             (weapons)
             (player))

(set! *random-state* (seed->random-state (current-time)))

(define repl (spawn-coop-repl-server))


(define agenda-dt 1)

(define game-over #f)

(define flying-upgrades '())

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
        (reset-player-position)
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

(define (upgrade-rect upgrade)
  (let ((position (upgrade-position upgrade)))
    (rect (vec2-x position)
          (vec2-y position)
          32 32)))

(define (spawn-enemies)
  (forever
   (spawn-enemy)
   (sleep (+ 1 (random 100)))))


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
