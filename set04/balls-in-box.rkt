;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; balls in box
;; run with (run 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FILE REQUIRED

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION PROVIDE

(provide
 run
 initial-world
 world-after-tick
 world-after-key-event
 world-after-mouse-event
 world-balls
 ball-x-pos
 ball-y-pos
 ball-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION

;run : PosInt PosReal -> World
;GIVEN: a ball speed and a frame rate, in secs/tick.
;EFFECT: runs the world.
;RETURNS: the final state of the world.
;EXAMPLE: (run 8 .25) creates and runs a world in
;which each ball travels at 8 pixels per tick and
;each tick is 0.25 secs.
(define (run ball-speed frame-rate)
  (big-bang (initial-world ball-speed)
            (on-tick world-after-tick frame-rate)
            (on-draw world->scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; NUMBER CONSTANTS
(define INITIAL-VALUE 0)
(define ZERO 0)
(define ONE 1)
(define RADIUS 20)
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define TEXT-X-COORD 70)
(define TEXT-Y-COORD 20)
(define TEXT-SIZE 20)

;; IMAGE CONSTANTS
(define BALL-SELECTED (circle RADIUS "solid" "green"))
(define BALL-UNSELECTED (circle RADIUS "outline" "green"))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; STRING CONSTANTS
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define N "n")
(define SPACE " ")
(define RIGHT "right")
(define LEFT "left")
(define TEXT-COLOR "blue")
(define TEXT-SENTENCE "Total balls: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct ball (x y selected? direction))
;; A Ball is a (make-ball Real Real Boolean Direction)
;; Interpretation:
;;  x, y give the position of the center of the a ball.
;;  selected? describes whether or not the ball is selected.
;;  direction is Direction,it decribes the ball's direction

;; ball-fn : Ball -> ??
;; (define (ball-fn b)
;;   (... (ball-x b)
;;        (ball-y b)
;;        (ball-selected? b)
;;        (ball-direction b)))
;; Examples:
;; (make-ball 150 200 false "right")

;; Direction is one of the:
;;   -- "right" : means a ball is moving right
;;   -- "left" : means a ball is moving left

;; direction-fn : Direction -> ??
;; (define (direction-fn d)
;;   (cond
;;     [(string=? d RIGHT) ...]
;;     [(string=? d LEFT) ...]))

;; ListOf<Ball>
;; A ListOf<Ball> (LOB) is either
;; -- empty
;; -- (cons Ball LOB)

;; lob-fn : LOB -> ??
;; (define (lob-fn lob)
;;   (cond
;;     [(empty? lob) ...]
;;     [else (...
;;             (ball-fn (first lob))
;;             (lob-fn (rest lob)))]))

(define-struct world (balls mx my speed paused?))
;; A World is a (make-world ListOf<Ball> Integer Integer PosInt Boolean)
;; Interpretation:
;;  ListOf<Ball> is a list of balls, it means list of balls in the world
;;  mx, my describe the position of the mouse position now
;;  speed shows the moving speed of balls
;;  paused? means whether a world is paused
;; Examples:
;; (make-world empty 0 0 8 true)

;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-balls w))
;;   (... (world-mx w))
;;   (... (world-my w))
;;   (... (world-speed w)
;;   (... (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-balls : World -> ListOf<Ball>
;; GIVEN: a world,
;; RETURNS: the list of balls that are in the box.
;; (define (world-balls w))

;; ball-x : Ball -> Real
;; ball-y : Ball -> Real
;; GIVEN: a ball
;; RETURNS: the x or y position of its center, respectively.
(define (ball-x-pos b)
  (ball-x b))

(define (ball-y-pos b)
  (ball-y b))

;; ball-selected? : Ball -> Boolean
;; GIVEN: a ball
;; RETURNS: true if and only if it is currently selected
;; (define (ball-selected? b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : PosInt  -> World
;; GIVEN: a ball speed
;; RETURNS: a world with no balls, but with the
;; property that any balls created in that world
;; will travel at the given speed.
;; EXAMPLE:
;; (initial-world 8) = (make-world empty ZERO ZERO 8 false)
;; STRATEGE: funciont composition
(define (initial-world speed)
  (make-world empty ZERO ZERO speed false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world
;; RETURNS: the world that should follow w after a tick.
;; EXAMPLES:
;; (world-after-tick (make-world (list b0 b1) 250 150 10 true)) =
;; (make-world (list b0 b1) 250 150 10 true)
;; STRATEGY: structural decomposition on w : World
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (balls-after-tick (world-balls w) (world-speed w))
       (world-mx w)
       (world-my w)
       (world-speed w)
       (world-paused? w))))

;; balls-after-tick : ListOf<Ball> PosInt -> ListOf<Ball>
;; GIVEN: list of balls, the speed of them
;; RETURNS: the status of balls after a tick
;; EXAMPLES: 
;; (balls-after-tick (list b0) 10) = (list b0)
;; STRATEGY: HOFC + structrual decomposition on b : Ball
(define (balls-after-tick balls s)
  (map
   ;; Ball -> Ball
   ;; GIVEN: a ball
   ;; RETURNS: the status of balls after a tick
   (lambda (b)
     (if (ball-selected? b) 
         b
         (ball-unselected (ball-x b) 
                          (ball-y b) 
                          (ball-direction b) 
                          s)))
   balls))

;; ball-unselected : Real Real Direction PosInt -> Ball
;; GIVEN: the x y position, the direction of a ball, and the speed of it
;; RETURNS: the status of balls after a tick
;; EXAMPLES: 
;; (ball-unselected (make-ball 10 20 false LEFT)) = b4
;; STRATEGY: function composition
(define (ball-unselected x y d s)
  (if (<= RADIUS x (- CANVAS-WIDTH RADIUS))
      (ball-move-after-tick x y d s)
      (make-ball (right-x x)
                 y
                 false
                 (right-direction x d))))

;; ball-move-after-tick : Real Real Direction Integer -> Ball
;; GIVEN: a ball's position x y, direction and the speed
;; RETURNS: the status of balls after it moves
;; EXAMPLES: 
;; (ball-move-after-tick (make-ball 240 150 false RIGHT)) = b2
;; STRATEGY: structural decomposition on d : Direction
(define (ball-move-after-tick x y d s)
  (cond
    [(string=? d RIGHT) 
     (make-ball (right-x (+ x s))
                y
                false
                (right-direction (+ x s) d))]
    [(string=? d LEFT) 
     (make-ball (right-x (- x s))
                y
                false
                (right-direction (- x s) d))]))

;; right-x : Real -> Real
;; GIVEN: the x position of a ball
;; RETURNS: the x position where it should be, to make a ball inside
;; EXAMPLE: (right-x 50) = 50
;; STRATEGY: cases on x
(define (right-x x)
  (cond
    [(< x RADIUS) RADIUS]
    [(> x (- CANVAS-WIDTH RADIUS)) (- CANVAS-WIDTH RADIUS)]
    [else x]))

;; right-direction : Real Direction -> Direction
;; GIVEN: the x position and the direction of a ball
;; RETURNS: the proper direction it should have
;; EXAMPLE: (right-direction 50 RIGHT) = RIGHT
;; STRATEGY: cases on x
(define (right-direction x d)
  (cond
    [(< x RADIUS) RIGHT]
    [(> x (- CANVAS-WIDTH RADIUS)) LEFT]
    [else d]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->scene : World -> Image
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: 
;; (world->scene w0) = 
;; (place-image (text (string-append TEXT-SENTENCE (number->string 2)) 
;;                    TEXT-SIZETEXT-COLOR)
;;              TEXT-X-COORD TEXT-Y-COORD (draw-balls (list b0 b1)))
;; STRATEGY: structural decompostion on w : World
(define (world->scene w)
  (place-image (draw-text (world-balls w))
               TEXT-X-COORD
               TEXT-Y-COORD
               (draw-balls (world-balls w))))

;; draw-text : ListOf<Ball> -> Image
;; GIVEN: a world
;; RETURNS: an image with the num of balls
;; EXAMPLE: 
;; (draw-text (list b0 b1)) =
;; (text (string-append TEXT-SENTENCE (number->string 2)))
;; STRATEGY: function composition
(define (draw-text balls)
  (text 
   (string-append TEXT-SENTENCE              
                  (number->string (length balls))) 
   TEXT-SIZE 
   TEXT-COLOR))

;; draw-balls : ListOf<Ball> -> Image
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given list of balls in world.
;; EXAMPLE:
;; (draw-balls (list b0 b1)) = 
;; (place-image BALL-SELECTED
;;              (ball-x b0) (ball-y b0)
;;              (place-image BALL-UNSELECTED
;;                           (ball-x b1) (ball-y b1)
;;                           EMPTY_CANVAS))
;; STRATEGY: HOFC + structural decomposition on b : Ball
(define (draw-balls balls)
  (foldr
   ;; Ball Image -> Image
   ;; GIVEN: a ball and an image that already exist
   ;; RETURNS: a scene after the ball drawn on it
   (lambda (b rest)
     (place-image (if (ball-selected? b) 
                      BALL-SELECTED 
                      BALL-UNSELECTED)
                  (ball-x b) (ball-y b)
                  rest))
   EMPTY-CANVAS
   balls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world, and keyevent passed in
;; RETURNS: the world that should follow the given world after the given
;; key event.
;; WHERE: key event will only be "n" and " ", ohters will be ignored
;; EXAMPLE: 
;; (world-after-key-event w0 "a") = w0             
;; STRATEGY: cases on ke : KeyEvent
(define (world-after-key-event w ke)
  (cond
    [(key=? ke N) (key-event-new-ball w)]
    [(key=? ke SPACE) (key-event-pause w)]
    [else w]))

;; key-event-new-ball : World -> World
;; GIVEN: a world
;; RETURNS: the world that should follow the given world after a new ball
;; added into it
;; EXAMPLE: 
;; (key-event-new-ball w0) =
;; (make-world (list (make-ball 200 150 false RIGHT)b0 b1) 
;;             250 150 10 false)
;; STRATEGY: structural decomposition on w : World
(define (key-event-new-ball w)
  (make-world (cons (make-ball HALF-CANVAS-WIDTH 
                               HALF-CANVAS-HEIGHT 
                               false 
                               RIGHT) 
                    (world-balls w) ) 
              (world-mx w)
              (world-my w)
              (world-speed w)
              (world-paused? w)))

;; key-event-pause : World -> World
;; GIVEN: a world
;; RETURNS: the world that should be just like the last tick
;; EXAMPLE: 
;; (key-event-pause w0) =
;; (make-world (list b0 b1) 250 150 10 true)
;; STRATEGY: structural decomposition on w : World
(define (key-event-pause w)
  (make-world
   (world-balls w)
   (world-mx w)
   (world-my w)
   (world-speed w)
   (not (world-paused? w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: A world, the location of a mouse event, and the mouse event itself
;; RETURNS: the world that should follow the given world after the given
;; mouse event at the given location.
;; EXAMPLE:
;; (world-after-mouse-event w0 250 150 "move") = w0
;; STRATEGY: cases on me : MouseEvent
(define (world-after-mouse-event w mx my me)
  (cond
    [(mouse=? me BUTTON-DOWN) (world-after-button-down w mx my)]
    [(mouse=? me DRAG) (world-after-drag w mx my)]
    [(mouse=? me BUTTON-UP)(world-after-button-up w mx my)]
    [else w]))

;; world-after-button-down : World Integer Integer -> World
;; GIVEN: A world, and the location of a mouse event
;; RETURNS: the world following a button-down at the given location.
;; EXAMPLE: 
;; (world-after-button-down w00 250 150) = w0
;; STRATEGY: structural decomposition on w : World
(define (world-after-button-down w mx my)
  (make-world (balls-after-button-down (world-balls w) mx my)
              mx
              my
              (world-speed w)
              (world-paused? w) ))

;; balls-after-button-down : ListOf<Ball> Integer Integer -> ListOf<Ball>
;; GIVEN: list of balls, and the location of a mouse event
;; RETURNS: list of balls
;; if the button-down is inside a ball, make a ball like the given one, 
;; but it is selected, and added coordinates of mouse into it
;; EXAMPLE: 
;; (balls-after-button-down (list b0 b1) 250 150) = (list b2 b1)
;; STRATEGY: HOFC + structural decomposition on b : Ball
(define (balls-after-button-down balls mx my)
  (map
   ;; Ball -> Ball
   ;; GIVEN: a ball, and the location of a mouse event
   ;; RETURNS: a ball according to whether it's selected
   (lambda (b)
     (make-ball (ball-x b)
                (ball-y b)
                (in-ball? (ball-x b) (ball-y b) mx my)
                (ball-direction b)))
   balls))

;; world-after-drag : World Integer Integer -> World
;; GIVEN: a world, the location of a mouse
;; RETURNS: the world following a drag at the given location.
;; EXAMPLE: 
;; (world-after-drag w0 250 150 DRAG) = w0
;; STRATEGY: structural decomposition on w : World
(define (world-after-drag w mx my)
  (make-world (balls-after-drag (world-balls w) 
                                (world-mx w) 
                                (world-my w) 
                                mx my) 
              mx
              my
              (world-speed w)
              (world-paused? w)))

;; balls-after-drag : ListOf<Ball> Integer Integer 
;;                                Integer Integer -> ListOf<Ball>
;; GIVEN: list of balls, the location of the old mouse when it button down,
;;  and the location of a mouse event
;; RETURNS: list of balls
;; if the drag begins inside a ball, make a ball dragged away
;; EXAMPLE: 
;; (balls-after-drag (list b0 b1) 250 150 DRAG) = (list b0 b1)
;; STRATEGY: HOFC
(define (balls-after-drag balls old-mx old-my mx my)
  (map
   ;; Ball -> Ball
   ;; GIVEN: a ball
   ;; RETURNS: a ball according to whether it is dragged
   (lambda (b)
     (if (ball-selected? b)
         (make-ball (+ (- mx old-mx) (ball-x b))
                    (+ (- my old-my) (ball-y b))
                    true
                    (ball-direction b))
         b))
   balls))

;; world-after-button-up : World Integer Integer -> World
;; GIVEN: a world, and the position of the mouse
;; RETURNS: the world following a button-up at the given location.
;; EXAMPLE: 
;; (world-after-button-up w0 250 150) = (make-world (list b2 b1) 0 0 10 false)
;; STRATEGY: structural decomposition on w : World
(define (world-after-button-up w mx my)
  (make-world (balls-after-button-up (world-balls w) mx my) 
              ZERO 
              ZERO
              (world-speed w)
              (world-paused? w)))

;; balls-after-button-up : ListOf<Ball> Integer Integer -> ListOf<Ball>
;; GIVEN: list of balls, and the location of a mouse event
;; RETURNS: list of balls
;; if the button up is inside a ball, make a ball like the given one, 
;; but set the selected? to false
;; EXAMPLE: 
;; (balls-after-button-up (list b0 b1) 250 150) = (list b2 b1)
;; STRATEGY: HOFC + structural decomposition on b : Ball
(define (balls-after-button-up balls mx my)
  (map
   ;; Ball -> Ball
   ;; GIVEN: a ball
   ;; RETURNS: a ball, according to whether it is choosed
   (lambda (b) 
     (make-ball (ball-x b)
                (ball-y b)
                false
                (ball-direction b)))
   balls))

;; in-ball? : Real Real Integer Integer -> Boolean
;; GIVEN: the coordinate (x, y) of a circle, and the 
;; the coordinate (mx, my) of the mouse
;; RETURNS: true if the mouse is inside the circle, false otherwise
;; EXAMPLE: 
;; (in-ball? 50 50 50 50) => true
;; STRATEGY: function composition
(define (in-ball? x y mx my)
  (<= (+ (* (- x mx) (- x mx))
         (* (- y my) (- y my)))
      (* RADIUS RADIUS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TEST

(define b0 (make-ball 250 150 true LEFT))
(define b1 (make-ball 150 150 false RIGHT))
(define b2 (make-ball 250 150 false LEFT))
(define b3 (make-ball 380 280 false RIGHT))
(define b4 (make-ball 20 20 false LEFT))
(define b5 (make-ball 390 280 false RIGHT))
(define b6 (make-ball 10 20 false LEFT))

(define w0 (make-world (list b0 b1) 250 150 10 false))
(define w00 (make-world (list b2 b1) 250 150 10 false))
(define w1 (make-world (list b0 b1 b2 b3 b4 b5 b6) 250 150 10 false))

(begin-for-test
  (check-equal? (ball-x-pos b0) 250 "test ball-x-pos")
  (check-equal? (ball-y-pos b0) 150 "test ball-y-pos")
  (check-equal? (initial-world 8) (make-world empty ZERO ZERO 8 false)
                "initial the world")
  
  (check-equal? (world->scene w0)
                (place-image (text 
                              (string-append TEXT-SENTENCE              
                                             (number->string 2))
                              TEXT-SIZE
                              TEXT-COLOR)
                             TEXT-X-COORD
                             TEXT-Y-COORD
                             (draw-balls (list b0 b1)))
                "test world->scene function")
  
  (check-equal? (world-after-tick w1)
                (make-world (list 
                             b0
                             (make-ball 160 150 false RIGHT)
                             (make-ball 240 150 false LEFT)
                             (make-ball 380 280 false LEFT)
                             (make-ball 20 20 false RIGHT)
                             (make-ball 380 280 false LEFT)
                             (make-ball 20 20 false RIGHT))
                            250 150
                            10
                            false)
                "test world-after-tick function: balls move")
  
  (check-equal? (world-after-tick (make-world (list b0 b1) 250 150 10 true))
                (make-world (list b0 b1) 250 150 10 true)
                "test world-after-tick function: paused")
  
  
  (check-equal? (world-after-key-event w0 N)
                (make-world (list 
                             (make-ball 200 150 false RIGHT)
                             b0 
                             b1) 
                            250 150 10 false)
                "add a new ball to balls")
  (check-equal? (world-after-key-event w0 " ") 
                (make-world (list b0 b1) 250 150 10 true)
                "world paused")
  (check-equal? (world-after-key-event w0 "a") w0
                "wrong key event instruction")      
  
  (check-equal? (world-after-mouse-event w00 250 150 BUTTON-DOWN) w0
                "botton down in one of tow balls")
  (check-equal? (world-after-mouse-event w0 250 150 BUTTON-UP) 
                (make-world (list b2 b1) 0 0 10 false)
                "botton up in one of tow balls")
  (check-equal? (world-after-mouse-event w0 250 150 DRAG) w0
                "drag one of tow balls")
  (check-equal? (world-after-mouse-event w0 250 150 "move") w0
                "wrong mouse event instruction")
  )