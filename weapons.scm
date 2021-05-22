(define-module (weapons)
  #:use-module (srfi srfi-9)

  #:use-module (graphics)
  #:use-module (util)
  #:use-module (enemies)
  #:use-module (player)
  
  #:use-module (chickadee math vector)
  #:use-module (chickadee math rect)
  #:use-module (chickadee graphics sprite)
  #:use-module (chickadee graphics texture)
  #:use-module (chickadee scripting)
  #:use-module (chickadee graphics path)

  #:export (add-explosion
            fire
            fire-rocket
            fireballs
            enemy-fireballs
            clear-old-explosions!
            draw-explosions!
            draw-fireballs
            move-fireballs
            move-enemy-fireballs
            draw-enemy-fireballs
            draw-rocket
            move-rocket
            draw-hud-rocket-progress
            spawn-enemy-fireballs
            enemy-fireball-rect
            clear-hit-enemies-script))

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

(define fireballs '())
(define enemy-fireballs '())
(define explosions '())
(define rocket #f)
(define rocket-last-shot-time 0)

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

(define (enemy-fireball-rect fireball)
  (let ((position (fireball-position fireball)))
    (rect (vec2-x position)
          (vec2-y position)
          8 8)))

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

(define (explode-enemies enemies)
  (for-each
   (lambda (enemy)
     (set! enemy-ships (delete enemy enemy-ships))
     (set! score (+ score (enemy-destroyed-points enemy)))
     (add-explosion (enemy-ship-position enemy)))
   enemies))
