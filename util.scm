(define-module (util)
  #:use-module (chickadee math rect)
  #:export (SCREEN-WIDTH
            SCREEN-HEIGHT

            object-inside-view-fn))

(define SCREEN-WIDTH 400)
(define SCREEN-HEIGHT 600)

(define (object-inside-view-fn coordinates-extract-pred)
  (lambda (object)
    (let ((coordinates (coordinates-extract-pred object))
          (view-rect (rect 0 0 SCREEN-WIDTH SCREEN-HEIGHT)))
      (rect-contains-vec2? view-rect coordinates))))
