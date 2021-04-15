(use-modules (chickadee)
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


(define repl (spawn-coop-repl-server))

(define MOVE-STEP 5)
(define SCREEN-WIDTH 400)
(define SCREEN-HEIGHT 600)

(define textures-atlas #f)
(define enemy-ship-sprite #f)
(define player-ship-sprite #f)
(define fireball #f)

(define player-position (vec2 300 0))

(define enemy-y SCREEN-HEIGHT)
(define enemy-x (/ SCREEN-WIDTH 2))

(define enemy-ships '())
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

(define enemy-path
  (let ((start-x (/ SCREEN-WIDTH 2))
        (start-y SCREEN-HEIGHT))
    (make-bezier-curve
     (vec2 start-x start-y)
     (vec2 (- start-x 300) (- start-y 100))
     (vec2 (+ start-x 300) (- start-y 300))
     (vec2 start-x (- start-y SCREEN-HEIGHT)))))

(define enemy-script
  (script
   (tween 300 0 1
          (lambda (t)
            (let ((point (bezier-curve-point-at enemy-path t)))
              (set! enemy-y (vec2-y point))
              (set! enemy-x (vec2-x point)))))))

;(spawn-script enemy-script)
(display (script-running? enemy-script))

(define game-world-agenda (make-agenda))
(with-agenda game-world-agenda
             (at 10 (enemy-script)))

(define (draw-debug)
  (let ((text (cond ((assoc-ref keys 'left) "left")
                    ((assoc-ref keys 'right) "right")
                    ((assoc-ref keys 'space) "space")
                    (else "none"))))

    (draw-text (number->string (agenda-time)) (vec2 220 220))
    (draw-text text (vec2 260.0 240.0))
    (draw-text (string-append "Fireballs: "
                              (number->string (length fireballs))) (vec2 280.0 290.0))))

(define bezier1 (make-bezier-curve (vec2 0 0) (vec2 100 100) (vec2 200 200) (vec2 300 300)))

(define (draw alpha)
  ;; (draw-canvas
  ;;  (make-canvas
  ;;   (with-style ((stroke-color green)
  ;;                (stroke-width 4.0)
  ;;                (fill-color green))
  ;;               (stroke
  ;;                (path
  ;;                 (move-to (bezier-curve-p0 enemy-path))
  ;;                 (bezier-to
  ;;                  (bezier-curve-p1 enemy-path)
  ;;                  (bezier-curve-p2 enemy-path)
  ;;                  (bezier-curve-p3 enemy-path)))))))
  
  (move-player!)
  (draw-fireballs)
  (move-fireballs)
  (draw-debug)
  ;(current-agenda game-world-agenda)
  (update-agenda 1)

  (draw-sprite enemy-ship-sprite (vec2 enemy-x enemy-y))
  (draw-sprite player-ship-sprite player-position)
  )

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

;;;

(bezier-curve-point-at bezier1 0.5)

(bezier-path (vec2 0 0) (vec2 1 1) (vec2 2 2) (vec2 3 3) (vec2 1 2))

(define bpath1 (bezier-path
                (vec2 200 0)
                (vec2 200 0) (vec2 400 100) (vec2 200 200)
                (vec2 200 200) (vec2 0 300) (vec2 200 400)
                (vec2 200 400) (vec2 400 500) (vec2 200 600)))

(bezier-curve-point-at (car (cdr bpath1)) 0)
(first (bezier-curve-point-at bpath1 0))
