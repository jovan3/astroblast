(define-module (util)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)

  #:export (SCREEN-WIDTH
            SCREEN-HEIGHT

            MOVE-STEP

            ROCKET-BLAST-RADIUS
            ROCKET-ARM-PERIOD
            ENEMY-FIREBAL-SPEED
            FIREBALL-OFFSET

            object-inside-view-fn
            move-upwards))

(define SCREEN-WIDTH 400)
(define SCREEN-HEIGHT 600)

(define MOVE-STEP 5)

(define ROCKET-BLAST-RADIUS 400)
(define ROCKET-ARM-PERIOD 300)
(define ENEMY-FIREBAL-SPEED 3)
(define FIREBALL-OFFSET (vec2 16 0))

(define (object-inside-view-fn coordinates-extract-pred)
  (lambda (object)
    (let ((coordinates (coordinates-extract-pred object))
          (view-rect (rect 0 0 SCREEN-WIDTH SCREEN-HEIGHT)))
      (rect-contains-vec2? view-rect coordinates))))

(define (move-upwards object position-fn position-set-fn)
  (let ((coordinates (position-fn object)))
    (position-set-fn object (vec2+ coordinates (vec2 0 MOVE-STEP)))
    object))
