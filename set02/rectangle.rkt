;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;rectangle
;run with (run 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FILE REQUIRED

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

;FUNCTION PROVIDE

(provide run)
(provide initial-world)
(provide world-x)
(provide world-y)
(provide world-selected?)
(provide world-after-mouse-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RUN FUNCTION

;run : Any -> World
;GIVEN: any value
;EFFECT: ignores its argument and starts the interactive program.
;RETURNS: the final state of the world.
(define (run any)
  (big-bang (initial-world INITIAL-VALUE)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CONSTANTS

(define INITIAL-VALUE 0)

(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")

(define RECTANGLE-WIDTH 100)
(define RECTANGLE-HEIGHT 60)
(define HALF-RECTANGLE-WIDTH (/ RECTANGLE-WIDTH 2))
(define HALF-RECTANGLE-HEIGHT (/ RECTANGLE-HEIGHT 2))

(define RECTANGLE-IMAGE 
  (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "solid" "green"))

(define CIRCLE 
  (circle 5 "solid" "red"))

(define RECTANGLE-IMAGE-SELECTED
  (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "green"))

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DATA DEFINITIONS

(define-struct world (x y selected? mx my))
;A World is a (make-world Integer Integer Boolean Integer Integer)
;Interpretation:
;x, y give the position of the center of the rectangle.
;selected? describes whether or not the rectangle is selected.
;mx, my record the position of the mouse if the rectangle is selected.
;range of mx is 0 ~ 300
;range of my is 0 ~ 400
;Examples:
;(make-world 150 200 false 0 0)
;template:
;world-fn : World -> ??
;(define (world-fn w)
;  (... (world-x w)
;       (world-y w)
;       (world-selected? w)
;       (world-mx w)
;       (world-my w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;world-x : World -> Integer
;world-y : World -> Integer
;RETURNS: the coordinates of the center of the rectangle
;NOTE: if these are part of the world struct, you don't need to
;write any deliverables for these functions.
;EXAMPLES:
;(world-x (make-world 100 50 true)) = 100
;(world-y (make-world 100 50 true)) = 50
;STRATEGY: structural decomposition on w : World

;world-selected? : World -> Boolean
;GIVEN a world
;RETURNS: true iff the rectangle is selected.
;NOTE: if selected? is part of the world struct, you don't need to
;write any deliverables for this function
;EXAMPLES:
;(world-selected? (make-world 100 50 true)) = true
;STRATEGY: structural decomposition on w : World

;TESTS:
(begin-for-test
  (check-equal? (world-x (make-world 100 50 true 0 0)) 100)
  (check-equal? (world-y (make-world 100 50 true 0 0)) 50)
  (check-equal? (world-selected? (make-world 100 50 true 0 0)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;initial-world : Any -> World
;GIVEN: any value
;RETURNS: the initial world.
;Ignores its argument.
;EXAMPLES:
;(initial 50) = (make world 200 150 false)
;STRATEGY: function composition
(define (initial-world any)
  (make-world HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT false INITIAL-VALUE INITIAL-VALUE))

;TESTS:
(begin-for-test
  (check-equal? 
   (initial-world 50)
   (make-world 200 150 false 0 0)
   "initial the world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;world->scene : World -> Scene
;GIVEN: a world
;RETURNS: a Scene that portrays the given world.
;EXAMPLE: 
;(world->scene (make-world 150 200 false 0 0)) =>
;(place-image RECTANGLE-IMAGE 150 200 EMPTY-CANVAS)
;(world->scene (make-world 150 200 true 220 170)) =>
;(place-image CIRCLE 220 170
;             (place-image RECTANGLE-IMAGE-SELECTED
;                          150
;                          200
;                          EMPTY-CANVAS))
;STRATEGY: structural decompostion on w : World
(define (world->scene w)
  (if (world-selected? w) 
      (draw-rectangle-selected (world-x w) (world-y w) 
                               (world-mx w) (world-my w))
      (draw-rectangle (world-x w) 
                      (world-y w))))

;draw-rectangle: World -> Image
;GIVEN: a world
;RETURNS: a image of rectangle unselected
;STRATEGY: function composition
(define (draw-rectangle x y)
  (place-image RECTANGLE-IMAGE
               x
               y
               EMPTY-CANVAS))

;draw-rectangle-selected: World -> Image
;GIVEN: a world
;RETURNS: a image of rectangle selected, with a red circle inside
;STRATEGY: function composition
(define (draw-rectangle-selected x y mx my)
  (place-image CIRCLE
               mx
               my
               (place-image RECTANGLE-IMAGE-SELECTED
                            x
                            y
                            EMPTY-CANVAS)))

(begin-for-test
  (check-equal? (world->scene (make-world 150 200 false 0 0))
                (place-image RECTANGLE-IMAGE 150 200 EMPTY-CANVAS)
                "rectangle that is not selected.")
  (check-equal? (world->scene (make-world 150 200 true 220 170))
                (place-image CIRCLE 220 170
                             (place-image RECTANGLE-IMAGE-SELECTED
                                          150
                                          200
                                          EMPTY-CANVAS))
                "rectangle that is selected."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;world-after-mouse-event : World Integer Integer MouseEvent -> World
;GIVEN: a world, and position x, postition y of the mouse
;RETURNS: the world that follows the given mouse event.
;EXAMPLES:
;(world-after-mouse-event 
; (make-world 150 200 false 0 0) 170 220 BUTTON-DOWN) =>
;(make-world 150 200 true 170 220)
;STRATEGY: cases on mev : MouseEvent
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (world-after-button-down w mx my)]
    [(mouse=? mev DRAG) (world-after-drag w mx my)]
    [(mouse=? mev BUTTON-UP)(world-after-button-up w mx my)]
    [else w]))

;world-after-button-down : World Integer Integer -> World
;RETURNS: the world following a button-down at the given location.
;if the button-down is inside the rectangle, return a rectangle 
;just like the given one, except that it is selected.
;STRATEGY: structural decomposition on w : World
(define (world-after-button-down w mx my)
  (if (in-rectangle? w mx my)
      (make-world (world-x w) (world-y w) true mx my)
      w))

;world-after-drag : World Integer Integer -> World
;RETURNS: the world following a drag at the given location.
;if the world is selected, then return a world just like the given
;one, except that it is now centered on the mouse position.
;STRATEGY: structural decomposition on w : World
(define (world-after-drag w mx my)
  (if (world-selected? w)
      (make-world (+ (world-x w) (- mx (world-mx w))) 
                  (+ (world-y w) (- my (world-my w))) 
                  true mx my)
      w))

;world-after-button-up : World Integer Integer -> World
;RETURNS: the world following a button-up at the given location.
;if the rectangle is selected, return a rectangle just like the given one,
;except that it is no longer selected.
;STRATEGY: structural decomposition on w : World
(define (world-after-button-up w mx my)
  (if (world-selected? w)
      (make-world (world-x w) (world-y w) false INITIAL-VALUE INITIAL-VALUE)
      w))

;in-rectangle? : World Integer Integer -> World
;RETURNS: true iff the given coordinate is inside the bounding box of
;the rectangle.
;EXAMPLES: 
;see tests below
;strategy: structural decomposition on w : World
(define (in-rectangle? w x y)
  (and
    (<= 
      (- (world-x w) HALF-RECTANGLE-WIDTH)
      x
      (+ (world-x w) HALF-RECTANGLE-WIDTH))
    (<= 
      (- (world-y w) HALF-RECTANGLE-HEIGHT)
      y
      (+ (world-y w) HALF-RECTANGLE-HEIGHT))))

(begin-for-test
  (check-equal? (world-after-mouse-event 
                 (make-world 150 200 false 0 0) 170 220 BUTTON-DOWN)
                (make-world 150 200 true 170 220)
                "world after mouse button-down with mouse click inside")
  (check-equal? (world-after-mouse-event 
                 (make-world 150 200 false 0 0) 250 220 BUTTON-DOWN)
                (make-world 150 200 false 0 0)
                "world after mouse button-down without mouse click inside")
  (check-equal? (world-after-mouse-event 
                 (make-world 150 200 false 0 0) 170 220 BUTTON-UP)
                (make-world 150 200 false 0 0)
                "world after mouse button-up outside")
  (check-equal? (world-after-mouse-event 
                 (make-world 150 200 true 170 220) 250 220 BUTTON-UP)
                (make-world 150 200 false 0 0)
                "world after mouse button-up inside")
  (check-equal? (world-after-mouse-event 
                 (make-world 150 200 false 0 0) 170 220 DRAG)
                (make-world 150 200 false 0 0)
                "world with mouse drag outside")
  (check-equal? (world-after-mouse-event 
                 (make-world 150 200 true 170 220) 220 240 DRAG)
                (make-world 200 220 true 220 240)
                "world with mouse drag inside")
  (check-equal? (world-after-mouse-event 
                 (make-world 150 200 false 0 0) 170 220 "move")
                (make-world 150 200 false 0 0)
                "world with no instruction"))