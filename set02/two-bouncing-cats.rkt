;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname two-bouncing-cats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#lang racket
;; two bouncing cats
;; start with (run initial-pos)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION.

;; run : Number -> World
;; GIVEN: the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats falling
;; RETURNS: the final state of the world
(define (run initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick TICK-VALUE)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define TICK-VALUE 0.5)

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS 
  (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))
(define Y-COORD 0)
(define GAP 0.5)

;; dimensions of the cat
(define HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

(define NORTH "north")
(define SOUTH "south")
(define WEST "west")
(define EAST "east")

(define UP "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")

(define SPACE " ")

(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean)
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; Example:
;; (make-world
;;  (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;  (make-cat CAT2-X-COORD Y-COORD false EAST)
;;  false))

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-cat1 w) (world-cat2 w) (world-paused? w)))

(define-struct cat (x-pos y-pos selected? orientation))
;; A Cat is a (make-cat Integer Integer Boolean Orientation)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; selected? describes whether or not the cat is selected.

;; An orientationon is an Itemization Data 
;; It describes which orientation the cat is move towards.
;; It is one of the:
;; -- "north"
;; -- "south"
;; -- "west"
;; -- "east"

;; template:
;; cat-fn : Cat -> ??
;(define (cat-fn c)
; (... (cat-x-pos w) (cat-y-pos w) 
; (cat-selected? w) (cat-orientation)))

;; TEMPLATE
;; orientation-fn : Orientation -> ??
;(define (orientation-fn o)
;  (cond
;    [(string=? o "north")    
;     ...]
;    [(string=? o "south")
;     ...]
;    [(string=? o "west")  
;     ...]
;    [(string=? o "east")  
;     ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-cat1 : World -> Cat
;; world-cat2 : World -> Cat
;; world-paused? : World -> Boolean
;; RETURNS: the specified component of the given world
;; NOTE: these are part of the world struct, so you don't need to
;; write any deliverables for these functions.
;; EXAMPLE:
;; (world-cat1 (make-world
;;              (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;              (make-cat CAT2-X-COORD Y-COORD false EAST)
;;              false)) =>
;; (make-cat CAT1-X-COORD Y-COORD false NORTH)

;; STRATEGY: structural decomposition on World

;; cat-x-pos : Cat -> Integer
;; cat-y-pos : Cat -> Integer
;; cat-selected? : Cat -> Boolean
;; RETURNS: the specified component of the given cat
;; NOTE: these are part of the cat struct, so you don't need to
;; write any deliverables for these functions.
;; EXAMPLE:
;; (cat-x-pos (make-cat 100 100 false NORTH)) => 100
;; STRATEGY: structural decomposition on Cat

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-north? : Cat -> Boolean
;; cat-east?  : Cat -> Boolean
;; cat-south? : Cat -> Boolean
;; cat-west?  : Cat -> Boolean
;; GIVEN: a Cat c
;; RETURNS: true iff c is travelling in the specified orientation.
;; NOTE: you will have to extend the cat struct to represent the cat's
;; orientation, so you will need to define these functions, and provide the
;; deliverables for them, as usual.
;; NOTE: you get to design the new cat struct yourself.  However, if
;; you look at these functions and decide to add 4 new fields north?,
;; east?, south?, and west?, that is probably a bad decision.  Surely
;; you can do better than that.
;; STRATEGY: structural decomposition on c : Cat
(define (cat-north? c)
  (cat-north?-helper (cat-orientation c)))

(define (cat-south? c)
  (cat-south?-helper (cat-orientation c)))

(define (cat-west? c)
  (cat-west?-helper (cat-orientation c)))

(define (cat-east? c)
  (cat-east?-helper (cat-orientation c)))

;; cat-north?-helper : Orientation -> Boolean
;; cat-east?-helper  : Orientation -> Boolean
;; cat-south?-helper : Orientation -> Boolean
;; cat-west?-helper  : Orientation -> Boolean
;; GIVEN: an orientation
;; RETURNS: true iff orientation is in the specified orientation.
;; STRATEGY: structural decomposition on orientation : Orientation
(define (cat-north?-helper orientation)
  (string=? orientation NORTH))

(define (cat-south?-helper orientation)
  (string=? orientation SOUTH))

(define (cat-west?-helper orientation)
  (string=? orientation WEST))

(define (cat-east?-helper orientation)
  (string=? orientation EAST))

(begin-for-test
  (equal? (cat-north? (make-cat 50 40 false NORTH)) true)
  (equal? (cat-north? (make-cat 50 40 false SOUTH)) false)
  (equal? (cat-south? (make-cat 50 40 false SOUTH)) true)
  (equal? (cat-south? (make-cat 50 40 false NORTH)) false)
  (equal? (cat-west? (make-cat 50 40 false WEST)) true)
  (equal? (cat-west? (make-cat 50 40 false EAST)) false)
  (equal? (cat-east? (make-cat 50 40 false EAST)) true)
  (equal? (cat-east? (make-cat 50 40 false WEST)) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Integer -> World
;; GIVEN: a y-coordinate
;; RETURNS: a world with two unselected cats, spaced evenly across the
;; canvas in the x-orientation, static, and placed at the given y
;; coordinate.
;; EXAMPLE:
;; (initial-world 0) =>
;; (make-world
;;  (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;  (make-cat CAT2-X-COORD Y-COORD false EAST)
;;  false))

;; STRATEGY: function composition

(define (initial-world y)
  (make-world
   (make-cat CAT1-X-COORD Y-COORD false SOUTH)
   (make-cat CAT2-X-COORD Y-COORD false SOUTH)
   false))

;; TESTS:
(begin-for-test
  (check-equal? (initial-world 0) 
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD false SOUTH)
                 (make-cat CAT2-X-COORD Y-COORD false SOUTH)
                 false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.
;; EXAMPLES:
;; (world-after-tick (make-world
;;                    (make-cat 150 200 false NORTH)
;;                    (make-cat 300 200 false EAST)
;;                    true)) =>
;; (make-world
;;  (make-cat 150 200 false NORTH)
;;  (make-cat 300 200 false EAST)
;;  true)

;; STRATEGY: structural decomposition on w : World
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (cat-after-tick (right-position (world-cat1 w)))
       (cat-after-tick (right-position (world-cat2 w)))
       (world-paused? w))))

;; right-position : Cat -> Cat
;; GIVEN: a cat
;; RETURNS: a cat with the right position
;; EXAMPLES:
;; (right-position (make-cat 150 200 false NORTH)) => 
;; (make-cat 150 200 false NORTH)
;; STRATEGY: structural decomposition on c : Cat
(define (right-position c)
  (if (or (inside-canvas? (cat-x-pos c) (cat-y-pos c)) (cat-selected? c))
      c
      (make-cat (right-x-pos (cat-x-pos c)) (right-y-pos (cat-y-pos c))
                (cat-selected? c) (opposite-orientation (cat-orientation c)))))

;; inside-canvas? : x y
;; GIVEN: x and y coordinate of a cat
;; RETURNS: true if a cat is inside the canvas, false otherwise
;; STRATEGY: function composition
(define (inside-canvas? x y)
  (if (and (<= HALF-CAT-WIDTH
               x
               (- CANVAS-WIDTH HALF-CAT-WIDTH))
           (<= HALF-CAT-HEIGHT
               y
               (- CANVAS-HEIGHT HALF-CAT-HEIGHT)))
      true false))

;; right-x-pos : Interger -> Integer
;; right-y-pos : Interger -> Integer
;; GIVEN: the x and y coordinate of the cat now
;; RETURNS: the right x and y coordinate the cat should be
;; (i.e., inside the canvas)
;; EXAMPLES:
;; (right-x-pos 100) => 100
;; STRATEGY: function composition
(define (right-x-pos x)
  (cond
    [(< x HALF-CAT-WIDTH) 
     (+ HALF-CAT-WIDTH GAP)]
    [(> (+ x HALF-CAT-WIDTH) CANVAS-WIDTH) 
     (- CANVAS-WIDTH HALF-CAT-WIDTH GAP)]
    [else x]))

(define (right-y-pos y)
  (cond
    [(< y HALF-CAT-HEIGHT)
     (+ HALF-CAT-HEIGHT GAP)]
    [(> (+ y HALF-CAT-HEIGHT) CANVAS-HEIGHT) 
     (- CANVAS-HEIGHT HALF-CAT-HEIGHT GAP)]
    [else y]))

;; cat-after-tick : Cat -> Cat
;; GIVEN: a cat c
;; RETURNS: the state of the given cat after a tick if it were in an
;; unpaused world.
;; EXAMPLES:
;; (cat-after-tick (make-cat 150 200 false NORTH)) =>
;; (make-cat 150 192 false NORTH)
;; STRATEGY: structural decomposition on c : Cat

(define (cat-after-tick c)
  (cond
    [(cat-selected? c) 
     (make-cat
      (cat-x-pos c) (cat-y-pos c) (cat-selected? c)
      (cat-orientation c))]
    [(hit-border? (cat-x-pos c) (cat-y-pos c) (cat-orientation c))
     (cat-after-tick-helper-II
      (cat-x-pos c) (cat-y-pos c) (cat-selected? c)
      (opposite-orientation (cat-orientation c)))]
    [else (cat-after-tick-helper 
           (cat-x-pos c) (cat-y-pos c) (cat-selected? c) 
           (cat-orientation c))]))

;; cat-after-tick-helper : Integer Integer Boolean Orientation -> Cat
;; GIVEN: a position and a value for selected?
;; RETURNS: the cat that should follow one in the given position in an
;; unpaused world 
;; EXAMPLES:
;; (cat-after-tick-helper 150 200 false NORTH) =>
;; (make-cat 150 192 false NORTH)
;; STRATEGY: structural decomposition on orientation : Orientation
(define (cat-after-tick-helper x-pos y-pos selected? orientation)
  (cond
    [(string=? orientation NORTH) 
     (make-cat x-pos (- y-pos CATSPEED) selected? orientation)]
    [(string=? orientation SOUTH) 
     (make-cat x-pos (+ y-pos CATSPEED) selected? orientation)]
    [(string=? orientation WEST) 
     (make-cat (- x-pos CATSPEED) y-pos selected? orientation)]
    [(string=? orientation EAST) 
     (make-cat (+ x-pos CATSPEED) y-pos selected? orientation)]))

;; cat-after-tick-helperII : Integer Integer Boolean Orientation -> Cat
;; GIVEN: a position and a value for selected?
;; RETURNS: the cat that change its orientation in the given position in an
;; unpaused world 
;; EXAMPLES:
;; (cat-after-tick-helper 150 200 false NORTH) =>
;; (make-cat 150 192 false NORTH)
;; STRATEGY: structural decomposition on orientation : Orientation
(define (cat-after-tick-helper-II x-pos y-pos selected? orientation)
  (cond
    [(string=? orientation NORTH) 
     (make-cat x-pos (right-y-pos (+ y-pos CATSPEED)) selected? orientation)]
    [(string=? orientation SOUTH) 
     (make-cat x-pos (right-y-pos (- y-pos CATSPEED)) selected? orientation)]
    [(string=? orientation WEST) 
     (make-cat (right-x-pos (+ x-pos CATSPEED)) y-pos selected? orientation)]
    [(string=? orientation EAST) 
     (make-cat (right-x-pos (- x-pos CATSPEED)) y-pos selected? orientation)]))

;; hit-border? : Integer Integer orientation -> Boolean
;; GIVEN: x, y position of a cat and its orientation
;; RETURNS: a result of whether it would hit the border in the next tick
;; EXAMPLES:
;; (hit-border? 50 50 NORTH) => false
;; STRATEGY: structural decomposition on orientation : Orientation
(define (hit-border? x y d)
  (cond
    [(string=? d NORTH) 
     (< y (+ HALF-CAT-HEIGHT CATSPEED))]
    [(string=? d SOUTH) 
     (> (+ y HALF-CAT-HEIGHT CATSPEED) CANVAS-HEIGHT)]
    [(string=? d WEST) 
     (< x (+ HALF-CAT-WIDTH CATSPEED))]
    [(string=? d EAST) 
     (> (+ x HALF-CAT-WIDTH CATSPEED) CANVAS-WIDTH)]))

;; opposite-orientation : Orientation -> Orientation
;; GIVEN: an orientation
;; RETURNS: the opposite orientation
;; EXAMPLES:
;; (opposite-orientation NORTH) => SOUTH
;; STRATEGY: structural decomposition on d : Orientation
(define (opposite-orientation d)
  (cond
    [(string=? d NORTH) SOUTH]
    [(string=? d SOUTH) NORTH]
    [(string=? d WEST) EAST]
    [(string=? d EAST) WEST]))

(begin-for-test
  (check-equal? (world-after-tick (make-world
                                   (make-cat 150 200 false NORTH)
                                   (make-cat 300 200 false EAST)
                                   true))
                (make-world
                 (make-cat 150 200 false NORTH)
                 (make-cat 300 200 false EAST)
                 true)
                "world paused")
  (check-equal? (world-after-tick (make-world
                                   (make-cat 150 200 false NORTH)
                                   (make-cat 300 200 false SOUTH)
                                   false))
                (make-world
                 (make-cat 150 192 false NORTH)
                 (make-cat 300 208 false SOUTH)
                 false)
                "cats normally move to north and south")
  (check-equal? (world-after-tick (make-world
                                   (make-cat 150 200 false WEST)
                                   (make-cat 300 200 false EAST)
                                   false))
                (make-world
                 (make-cat 142 200 false WEST)
                 (make-cat 308 200 false EAST)
                 false)
                "cats normally move to west and east")
  (check-equal? (world-after-tick (make-world
                                   (make-cat 150 10 false NORTH)
                                   (make-cat 300 390 false SOUTH)
                                   false))
                (make-world
                 (make-cat 150 67 false SOUTH)
                 (make-cat 300 333 false NORTH)
                 false)
                "cats hit north and south borders")
  (check-equal? (world-after-tick (make-world
                                   (make-cat 10 200 false WEST)
                                   (make-cat 440 200 false EAST)
                                   false))
                (make-world
                 (make-cat 46 200 false EAST)
                 (make-cat 404 200 false WEST)
                 false)
                "cats hit west and east borders")
  (check-equal? (world-after-tick (make-world
                                   (make-cat 150 200 true NORTH)
                                   (make-cat 300 200 false EAST)
                                   false))
                (make-world
                 (make-cat 150 200 true NORTH)
                 (make-cat 308 200 false EAST)
                 false)
                "cat1 selected")
  (check-equal? (world-after-tick (make-world
                                   (make-cat 150 HALF-CAT-HEIGHT false NORTH)
                                   (make-cat HALF-CAT-WIDTH 200 false WEST)
                                   false))
                (make-world
                 (make-cat 150 59 false SOUTH)
                 (make-cat 38 200 false EAST)
                 false)
                "cats would hit border next tick")
  (check-equal? (world-after-tick (make-world
                                   (make-cat 150 (- CANVAS-HEIGHT HALF-CAT-HEIGHT) false SOUTH)
                                   (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH) 200 false EAST)
                                   false))
                (make-world
                 (make-cat 150 341 false NORTH)
                 (make-cat 412 200 false WEST)
                 false)
                "cats would hit border next tick")
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLES:
;; (world-to-scene 
;;  (make-world
;;   (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;   (make-cat CAT2-X-COORD Y-COORD false EAST)
;;   false)) =>
;; (place-image
;;  CAT-IMAGE
;;  CAT1-X-COORD Y-COORD
;;  (place-image
;;   CAT-IMAGE
;;   CAT2-X-COORD Y-COORD
;;   EMPTY-CANVAS))))
;; STRATEGY: structural decomposition on w : World
(define (world-to-scene w)
  (place-cat
   (world-cat1 w)
   (place-cat
    (world-cat2 w)
    EMPTY-CANVAS)))

;; place-cat : Cat Scene -> Scene
;; GIVEN: a cat and a scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
;; EXAMPLES:
;; (place-cat (make-cat CAT1-X-COORD Y-COORD false NORTH) EMPTY-SCENE)
;; => (place-image CAT-IMAGE CAT1-X-COORD Y-COORD EMPTY-CANVAS)
;; STRATEGY: structural decomposition on c : Cat
(define (place-cat c s)
  (place-image
   CAT-IMAGE
   (cat-x-pos c) (cat-y-pos c)
   s))

(begin-for-test
  (check-equal? (world-to-scene 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD false NORTH)
                  (make-cat CAT2-X-COORD Y-COORD false EAST)
                  false))
                (place-image
                 CAT-IMAGE
                 CAT1-X-COORD Y-COORD
                 (place-image
                  CAT-IMAGE
                  CAT2-X-COORD Y-COORD
                  EMPTY-CANVAS))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world , x and y coordinate of a mouse, a description of mouse event
;; RETURNS: the world that should follow the given mouse event
;; EXAMPLES:
;; (world-after-mouse-event 
;;  (make-world
;;   (make-cat 150 200 false NORTH)
;;   (make-cat 300 200 false EAST)
;;   false)
;;  160 210 BUTTON-DOWN) =>
;; (make-world
;;  (make-cat 150 200 true NORTH)
;;  (make-cat 300 200 false EAST)
;;  false)
;; STRATEGY: structural decomposition on w : World
(define (world-after-mouse-event w mx my mev)
  (make-world
   (cat-after-mouse-event (world-cat1 w) mx my mev)
   (cat-after-mouse-event (world-cat2 w) mx my mev)
   (world-paused? w)))

;; cat-after-mouse-event : Cat Integer Integer MouseEvent -> Cat
;; GIVEN: a cat and a description of a mouse event
;; RETURNS: the cat that should follow the given mouse event
;; EXAMPLES:
;; (cat-after-mouse-event (make-cat 150 200 false NORTH)
;;                        160 210 BUTTON-DOWN) =>
;;  (make-cat 150 200 true NORTH)
;; STRATEGY: cases on mev : MouseEvents
(define (cat-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (cat-after-button-down c mx my)]
    [(mouse=? mev DRAG) (cat-after-drag c mx my)]
    [(mouse=? mev BUTTON-UP) (cat-after-button-up c mx my)]
    [else c]))

;; cat-after-button-down : Cat Integer Integer -> Cat
;; GIVEN: a cat and coordinates mx my of mouse
;; RETURNS: the cat following a button-down at the given location.
;; EXAMPLES:
;; (cat-after-button-down (make-cat 150 200 false NORTH)
;;                        160 210) =>
;; (make-cat 150 200 true NORTH)
;; STRATEGY: structural decomposition on c : Cat
(define (cat-after-button-down c x y)
  (if (in-cat? (cat-x-pos c) (cat-y-pos c) x y)
      (make-cat (cat-x-pos c) (cat-y-pos c) true (cat-orientation c))
      c))

;; cat-after-drag : Cat Integer Integer -> Cat
;; GIVEN: a cat and coordinates mx my of mouse
;; RETURNS: the cat following a drag at the given location
;; EXAMPLES:
;; (cat-after-button-down (make-cat 150 200 false NORTH)
;;                        160 210) =>
;; (make-cat 150 200 false NORTH)
;; STRATEGY: structural decomposition on c : Cat
(define (cat-after-drag c x y)
  (if (cat-selected? c)
      (make-cat x y true (cat-orientation c))
      c))

;; cat-after-button-up : Cat Integer Integer -> Cat
;; GIVEN: a cat and coordinates mx my of mouse
;; RETURNS: the cat following a button-up at the given location
;; EXAMPLES:
;; (cat-after-button-down (make-cat 150 200 true NORTH)
;;                        160 210) =>
;; (make-cat 150 200 false NORTH)
;; STRATEGY: structural decomposition on c : Cat
(define (cat-after-button-up c x y)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c) false (cat-orientation c))
      c))

;; in-cat? : Cat Integer Integer -> Cat
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on c : Cat
(define (in-cat? x y mx my)
  (and
   (<= (- x HALF-CAT-WIDTH) mx (+ x HALF-CAT-WIDTH))
   (<=  (- y HALF-CAT-HEIGHT) my (+ y HALF-CAT-HEIGHT))))

(begin-for-test
  (check-equal? (world-after-mouse-event 
                 (make-world
                  (make-cat 150 200 false NORTH)
                  (make-cat 300 200 false EAST)
                  false)
                 160 210 BUTTON-DOWN)
                (make-world
                 (make-cat 150 200 true NORTH)
                 (make-cat 300 200 false EAST)
                 false)
                "mouse clicks down on cat1")
  (check-equal? (world-after-mouse-event 
                 (make-world
                  (make-cat 150 200 true NORTH)
                  (make-cat 300 200 false EAST)
                  false)
                 160 210 BUTTON-UP)
                (make-world
                 (make-cat 150 200 false NORTH)
                 (make-cat 300 200 false EAST)
                 false)
                "mouse button up out of cat1")
  (check-equal? (world-after-mouse-event 
                 (make-world
                  (make-cat 150 200 true NORTH)
                  (make-cat 300 200 false EAST)
                  false)
                 160 210 DRAG)
                (make-world
                 (make-cat 160 210 true NORTH)
                 (make-cat 300 200 false EAST)
                 false)
                "cat1 draged by the mouse")
  (check-equal? (world-after-mouse-event 
                 (make-world
                  (make-cat 150 200 false NORTH)
                  (make-cat 300 200 false EAST)
                  false)
                 160 210 "move")
                (make-world
                 (make-cat 150 200 false NORTH)
                 (make-cat 300 200 false EAST)
                 false)
                "cat recieves wrong instruction")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; EXAMPLES:
;; (world-after-key-event (make-world
;;                         (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;                         (make-cat CAT2-X-COORD Y-COORD false EAST)
;;                         false) SPACE) =>
;; (make-world
;;  (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;  (make-cat CAT2-X-COORD Y-COORD false EAST)
;;  true)
;; STRATEGY: cases on KeyEvent
(define (world-after-key-event w ke)
  (cond
    [(key=? ke SPACE)
     (world-with-paused-toggled w)]
    [(key=? ke UP) (world-with-direc-up-selection w)]
    [(key=? ke DOWN) (world-with-direc-down-selection w)]
    [(key=? ke LEFT) (world-with-direc-left-selection w)]
    [(key=? ke RIGHT) (world-with-direc-right-selection w)]
    [else w]))


;; world-with-direc-up-selection : World -> World
;; world-with-direc-down-selection : World -> World
;; world-with-direc-left-selection : World -> World
;; world-with-direc-right-selection : World -> World
;; GIVEN: a world
;; RETURNS: a world after a direction selected
;; EXAMPLES:
;; (world-with-direc-up-selection (make-world
;;  (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;  (make-cat CAT2-X-COORD Y-COORD false EAST)
;;  false) UP) =>
;; (make-world
;;  (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;  (make-cat CAT2-X-COORD Y-COORD false EAST)
;;  false)
;; STRATEGY: structural decomposition on w : World
(define (world-with-direc-up-selection w)
  (make-world
   (cat-with-up-direc-selection (world-cat1 w))
   (cat-with-up-direc-selection (world-cat2 w))
   (world-paused? w)))

(define (world-with-direc-down-selection w)
  (make-world
   (cat-with-down-direc-selection (world-cat1 w))
   (cat-with-down-direc-selection (world-cat2 w))
   (world-paused? w)))

(define (world-with-direc-left-selection w)
  (make-world
   (cat-with-left-direc-selection (world-cat1 w))
   (cat-with-left-direc-selection (world-cat2 w))
   (world-paused? w)))

(define (world-with-direc-right-selection w)
  (make-world
   (cat-with-right-direc-selection (world-cat1 w))
   (cat-with-right-direc-selection (world-cat2 w))
   (world-paused? w)))

;; cat-with-up-dire-selection : Cat -> Cat
;; cat-with-down-dire-selection : Cat -> Cat
;; cat-with-left-dire-selection : Cat -> Cat
;; cat-with-right-dire-selection : Cat -> Cat
;; GIVEN: a cat
;; RETURNS: a cat after direction selected
;; EXAMPLES:
;; (cat-with-up-with-direc-selection
;;  (make-cat CAT1-X-COORD Y-COORD false NORTH)) =>
;; (make-cat CAT1-X-COORD Y-COORD false NORTH)
;; STRATEGY: structural decomposition on c : Cat
(define (cat-with-up-direc-selection c)
  (make-cat
   (cat-x-pos c)
   (cat-y-pos c)
   (cat-selected? c)
   (if (cat-selected? c) NORTH (cat-orientation c))))

(define (cat-with-down-direc-selection c)
  (make-cat
   (cat-x-pos c)
   (cat-y-pos c)
   (cat-selected? c)
   (if (cat-selected? c) SOUTH (cat-orientation c))))

(define (cat-with-left-direc-selection c)
  (make-cat
   (cat-x-pos c)
   (cat-y-pos c)
   (cat-selected? c)
   (if (cat-selected? c) WEST (cat-orientation c))))

(define (cat-with-right-direc-selection c)
  (make-cat
   (cat-x-pos c)
   (cat-y-pos c)
   (cat-selected? c)
   (if (cat-selected? c) EAST (cat-orientation c))))

;; world-with-paused-toggled : World -> World
;; GIVEN: a world
;; RETURNS: a world just like the given one, but with paused? toggled
;; EXAMPLES:
;; (world-with-paused-toggled
;;  (make-world
;;   (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;   (make-cat CAT2-X-COORD Y-COORD false EAST)
;;   false)) =>
;; (make-world
;;   (make-cat CAT1-X-COORD Y-COORD false NORTH)
;;   (make-cat CAT2-X-COORD Y-COORD false EAST)
;;   true)
;; STRATEGY: structural decomposition on w : World
(define (world-with-paused-toggled w)
  (make-world
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))

(begin-for-test
  (check-equal? (world-after-key-event 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD false NORTH)
                  (make-cat CAT2-X-COORD Y-COORD false EAST)
                  false) SPACE)
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD false NORTH)
                 (make-cat CAT2-X-COORD Y-COORD false EAST)
                 true)
                "world paused")
  (check-equal? (world-after-key-event 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD true SOUTH)
                  (make-cat CAT2-X-COORD Y-COORD false EAST)
                  false) UP)
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD true NORTH)
                 (make-cat CAT2-X-COORD Y-COORD false EAST)
                 false)
                "cat1 revieves 'up', changes its direction to north")
  (check-equal? (world-after-key-event 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD false NORTH)
                  (make-cat CAT2-X-COORD Y-COORD true EAST)
                  false) DOWN)
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD false NORTH)
                 (make-cat CAT2-X-COORD Y-COORD true SOUTH)
                 false)
                "cat2 recieves 'down', changes its direction to south")
  (check-equal? (world-after-key-event 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD true SOUTH)
                  (make-cat CAT2-X-COORD Y-COORD false EAST)
                  false) LEFT)
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD true WEST)
                 (make-cat CAT2-X-COORD Y-COORD false EAST)
                 false)
                "cat1 revieves 'left', changes its direction to west")
  (check-equal? (world-after-key-event 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD false NORTH)
                  (make-cat CAT2-X-COORD Y-COORD true WEST)
                  false) RIGHT)
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD false NORTH)
                 (make-cat CAT2-X-COORD Y-COORD true EAST)
                 false)
                "cat2 recieves 'right', changes its direction to east")
  (check-equal? (world-after-key-event 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD false NORTH)
                  (make-cat CAT2-X-COORD Y-COORD false EAST)
                  false) UP)
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD false NORTH)
                 (make-cat CAT2-X-COORD Y-COORD false EAST)
                 false)
                "no cat selected")
  (check-equal? (world-after-key-event 
                 (make-world
                  (make-cat CAT1-X-COORD Y-COORD false NORTH)
                  (make-cat CAT2-X-COORD Y-COORD false EAST)
                  false) "a")
                (make-world
                 (make-cat CAT1-X-COORD Y-COORD false NORTH)
                 (make-cat CAT2-X-COORD Y-COORD false EAST)
                 false)
                "meaningless instruction")
  )

