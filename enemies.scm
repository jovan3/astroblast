(define-module (enemies)
  #:use-module (srfi srfi-9)
  #:use-module (util)
  #:use-module (graphics)
  #:use-module (chickadee math bezier)
  #:use-module (chickadee math vector)
  #:use-module (chickadee graphics sprite)
  #:use-module (chickadee graphics texture)
  #:use-module (chickadee scripting)
  #:export (move-enemies
            enemy-ships
            clear-enemies-outside-view
            spawn-enemy
            enemy-ship-position
            enemies-within-radius
            draw-enemies
            enemy-ship
            enemy-bezier-path
            enemy-destroyed-points))

(define enemy-ships '())

(define-record-type <enemy-ship>
  (make-enemy-ship bezier-path current-t sprite-index hit-score)
  enemy-ship?
  (bezier-path enemy-ship-path)
  (current-t enemy-ship-path-t enemy-ship-path-t-set!)
  (sprite-index enemy-ship-sprite-index)
  (hit-score enemy-hit-score))

(define (enemy-destroyed-points enemy)
  (enemy-hit-score enemy))

(define (enemy-bezier-path enemy)
  (enemy-ship-path enemy))

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

(define (enemy-ship-sprite index)
  (texture-atlas-ref enemy-ships-textures-atlas index))

(define (enemy-ship-position ship)
  (bezier-curve-point-at
   (enemy-ship-path ship) (enemy-ship-path-t ship)))

(define (spawn-enemy)
  (let ((ship (make-enemy-ship (make-enemy-path) 0 (random 5) 100)))
    (set! enemy-ships (cons ship enemy-ships))))

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

(define (clear-enemies-outside-view)
  (let ((enemy-inside-view?
         (object-inside-view-fn enemy-ship-position)))
    (forever
     (set! enemy-ships (filter enemy-inside-view? enemy-ships))
     (sleep 1))))
