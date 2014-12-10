;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
 world-after-key-event
 world-after-mouse-event
 world-balls
 ball-x-pos
 ball-y-pos
 ball-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION

;; run : Any -> World
;; GIVEN: An argument, which is ignored.
;; EFFECT: runs the world at tick rate of 0.25 secs/tick.
;; RETURNS: the final state of the world.
;; Note that the world does not respond to time passing, so the tick rate
;; doesn't make a difference.
(define (run any)
  (big-bang (initial-world 0)
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
(define TEXT-COLOR "blue")
(define TEXT-SENTENCE "Total balls: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct ball (x-pos y-pos selected? mx my))
;; A Ball is a (make-ball Integer Integer Boolean Integer Integer)
;; Interpretation:
;;  x, y give the position of the center of the a ball.
;;  selected? describes whether or not the ball is selected.
;;  mx, my record the position of the mouse if the rectangle is selected.
;;   range of mx is 0 ~ 300
;;   range of my is 0 ~ 400
;; Examples:
;; (make-ball 150 200 false 0 0)
;; template:

;; ball-fn : Ball -> ??
;; (define (ball-fn b)
;;   (... (ball-x b)
;;        (ball-y b)
;;        (ball-selected? b)
;;        (ball-mx b)
;;        (ball-my b)))

;; ListofBalls
;; A ListOfBalls (LOB) is either
;; -- empty
;; -- (cons Ball LOB)

;; lob-fn : LOB -> ??
;; (define (lob-fn lob)
;;   (cond
;;     [(empty? lob) ...]
;;     [else (...
;;             (ball-fn (first lob))
;;             (lob-fn (rest lob)))]))

(define-struct world (balls))
;; A World is a (make-world ListOfBalls)
;; Interpretation:
;;  ListOfBalls is a ListOfBalls, it means list of balls in the world
;; Examples:
;; (make-world empty)

;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-balls w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-balls : World -> ListOfBalls
;; GIVEN: a world,
;; RETURNS: the list of balls that are in the box.
;; (define (world-balls w)

;; ball-x-pos : Ball -> Integer
;; ball-y-pos : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: the x or y position of its center, respectively.
;; (define (ball-x-pos b))
;; (define (ball-y-pos b))

;; ball-selected? : Ball -> Boolean
;; GIVEN: a ball
;; RETURNS: true if and only if it is currently selected
;; (define (ball-selected? b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: An argument, which is ignored.
;; RETURNS: a world with no balls.
;; EXAMPLE:
;; (initial-world 0) => (make-world empty) 
;; STRATEGY: function composition
(define (initial-world any)
  (make-world empty))

(begin-for-test
  (check-equal? (initial-world 0) (make-world empty) 
                "initial world with nothing"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE:
;; (world->scene (make-world
;;                (cons
;;                 (make-ball 250 150 true 0 0)
;;                 (cons (make-ball 150 150 false 0 0) 
;;                       empty)))) =>
;; (place-image (text 
;;               (string-append TEXT-SENTENCE (number->string 2))
;;               TEXT-SIZE TEXT-COLOR)
;;              TEXT-X-COORD TEXT-Y-COORD
;;              (place-image BALL-SELECTED 250 150
;;                           (place-image BALL-UNSELECTED 150 150
;;                                        EMPTY-CANVAS)))
;; STRATEGY: structural decompostion on w : World
(define (world->scene w)
  (place-image (draw-text (world-balls w))
               TEXT-X-COORD
               TEXT-Y-COORD
               (draw-balls (world-balls w))))

;; draw-text : ListOfBalls -> Image
;; GIVEN: a world
;; RETURNS: an image with the num of balls
;; EXAMPLE:
;; (draw-text (cons (make-ball 250 150 true 0 0)
;;                  (cons (make-ball 150 150 false 0 0) empty))) =>
;; (text (string-append TEXT-SENTENCE (number->string 2)) TEXT-SIZE TEXT-COLOR)
;; STRATEGY: function composition
(define (draw-text balls)
  (text 
   (string-append TEXT-SENTENCE              
                  (number->string (length balls))) 
   TEXT-SIZE 
   TEXT-COLOR))

;; draw-balls : ListOfBalls -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given list of balls in world.
;; EXAMPLE:
;; (draw-balls (cons (make-ball 250 150 true 0 0)
;;                   (cons (make-ball 150 150 false 0 0) empty))) =>
;; (place-image BALL-SELECTED 250 150
;;              (place-image BALL-UNSELECTED
;;                           150 150
;;                           EMPTY-CANVAS))
;; STRATEGY: structural decompostion on balls : ListOfBalls
(define (draw-balls balls)
  (cond
    [(empty? balls) EMPTY-CANVAS]
    [else (draw-ball (first balls) (draw-balls (rest balls)))]))

;; draw-ball : Ball Scene -> Scene
;; GIVEN: a ball and a scene that already exist
;; RETURNS: a scene after the ball drawn on it
;; EXAMPLE:
;; (make-ball 250 150 true 0 0) => BALL-UNSELECTED
;; STRATEGY: structural decomposition on b : Ball
(define (draw-ball b background)
  (if (ball-selected? b)
      (place-image BALL-SELECTED
                   (ball-x-pos b) (ball-y-pos b)
                   background)
      (place-image BALL-UNSELECTED
                   (ball-x-pos b) (ball-y-pos b)
                   background)))
(begin-for-test
  (check-equal? (world->scene (make-world
                               (cons
                                (make-ball 250 150 true 0 0)
                                (cons (make-ball 150 150 false 0 0) 
                                      empty))))
                (place-image (text 
                              (string-append TEXT-SENTENCE              
                                             (number->string 2))
                              TEXT-SIZE
                              TEXT-COLOR)
                             TEXT-X-COORD
                             TEXT-Y-COORD
                             (place-image BALL-SELECTED
                                          250 150
                                          (place-image BALL-UNSELECTED
                                                       150 150
                                                       EMPTY-CANVAS)))
                             "draw a selected ball and an unselected ball"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world, and keyevent passed in
;; RETURNS: the world that should follow the given world after the given
;; key event.
;; EXAMPLE:
;; (world-after-key-event (make-world empty) " ") => (make-world empty)
;; STRATEGY: cases on ke : KeyEvent
(define (world-after-key-event w ke)
  (cond
    [(key=? ke N)
     (key-event-helper w)]
    [else w]))

;; key-event-helper : World -> World
;; GIVEN: a world
;; RETURNS: the world that should follow the given world after a new ball
;; added into it
;; EXAMPLE:
;; (key-event-helper (make-world empty)) =>
;; (make-world (cons (make-ball 200 150 false 0 0) empty))
;; STRATEGY: structural decomposition on w : World
(define (key-event-helper w)
  (make-world (cons (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT 
                               false INITIAL-VALUE INITIAL-VALUE) 
                    (world-balls w))))

(begin-for-test
  (check-equal? (world-after-key-event (make-world empty) N)
                (make-world (cons (make-ball 200 150 false 0 0) empty))
                "recieve instruction N, create a new circle")
  (check-equal? (world-after-key-event (make-world empty) " ")
                (make-world empty)
                "recieve instruction else, do nothing"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: A world, the location of a mouse event, and the mouse event itself
;; RETURNS: the world that should follow the given world after the given
;; mouse event at the given location.
;; EXAMPLE:
;; (world-after-mouse-event 
;;  (make-world (cons (make-ball 250 150 false 0 0)
;;                    (cons (make-ball 150 150 false 0 0) empty)))
;;  250 150 BUTTON-DOWN) =>
;; (make-world (cons (make-ball 250 150 true 250 150)
;;                   (cons (make-ball 150 150 false 0 0) 
;;                         empty)))
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
;; (world-after-button-down
;;  (make-world (cons (make-ball 250 150 false 0 0)
;;                    (cons (make-ball 150 150 false 0 0) 
;;                          empty))) 250 150 )
;; (make-world (cons (make-ball 250 150 true 250 150)
;;                   (cons (make-ball 150 150 false 0 0) 
;;                         empty)))
;; STRATEGY: structural decomposition on w : World
(define (world-after-button-down w mx my)
  (make-world (balls-after-button-down (world-balls w) mx my)))

;; balls-after-button-down : ListOfBalls Integer Integer -> ListOfBalls
;; GIVEN: list of balls, and the location of a mouse event
;; RETURNS: list of balls
;; if the button-down is inside a ball, make a ball like the given one, 
;; but it is selected, and added coordinates of mouse into it
;; EXAMPLE:
;; (balls-after-button-down
;;  (cons (make-ball 250 150 false 0 0)
;;        (cons (make-ball 150 150 false 0 0) 
;;              empty)) 250 150) =>
;; (cons (make-ball 250 150 true 250 150)
;;       (cons (make-ball 150 150 false 0 0) 
;;             empty)))
;; STRATEGY: structural decomposition on balls : ListOfBalls
(define (balls-after-button-down balls mx my)
  (cond
    [(empty? balls) empty]
    [else (cons (ball-after-button-down (first balls) mx my)
                (balls-after-button-down (rest balls) mx my))]))

;; ball-after-button-down : Ball Integer Integer -> Ball
;; GIVEN: a ball, and the location of a mouse event
;; RETURNS: a ball
;; if the button-down is inside a ball, make a ball like the given one, 
;; but it is selected, and added coordinates of mouse into it
;; EXAMPLE:
;; (ball-after-button-down (make-ball 250 150 false 0 0)) =>
;; (make-ball 250 150 true 250 150)
;; STRATEGY: structural decomposition on b : Ball
(define (ball-after-button-down b mx my)
  (if (in-ball? (ball-x-pos b) (ball-y-pos b) mx my)
      (make-ball (ball-x-pos b)
                 (ball-y-pos b)
                 true mx my)
      b))

;; world-after-drag : World Integer Integer -> World
;; GIVEN: a world, the location of a mouse
;; RETURNS: the world following a drag at the given location.
;; EXAMPLE:
;; (world-after-mouse-event 
;;  (make-world (cons (make-ball 250 150 true 250 150)
;;                    (cons (make-ball 150 150 false 0 0) 
;;                          empty))) =>
;;  250 150 DRAG)
;; (make-world (cons (make-ball 250 150 true 250 150)
;;                   (cons (make-ball 150 150 false 0 0) 
;;                         empty)))
;; STRATEGY: structural decomposition on w : World
(define (world-after-drag w mx my)
  (make-world (balls-after-drag (world-balls w) mx my)))

;; balls-after-drag : ListOfBalls Integer Integer -> ListOfBalls
;; GIVEN: list of balls, and the location of a mouse event
;; RETURNS: list of balls
;; if the drag begins inside a ball, make a ball like the given one, 
;; but dragged away
;; EXAMPLE:
;; (balls-after-drag
;;  (cons (make-ball 250 150 true 250 150)
;;        (cons (make-ball 150 150 false 0 0) 
;;              empty)) 250 150) =>
;; (cons (make-ball 250 150 true 250 150)
;;       (cons (make-ball 150 150 false 0 0) 
;;             empty)))
;; STRATEGY: structural decomposition on balls : ListOfBalls
(define (balls-after-drag balls mx my)
  (cond
    [(empty? balls) empty]
    [else (cons (ball-after-drag (first balls) mx my)
                (balls-after-drag (rest balls) mx my))]))

;; ball-after-drag : Ball Integer Integer -> Ball
;; GIVEN: a ball, and the location of a mouse event
;; RETURNS: a ball
;; if the drag begins inside a ball, make a ball like the given one, 
;; but dragged away
;; EXAMPLE:
;; (ball-after-drag (make-ball 250 150 true 250 150)) =>
;; (make-ball 250 150 true 250 150)
;; STRATEGY: structural decomposition on b : Ball
(define (ball-after-drag b mx my)
  (if (ball-selected? b)
      (make-ball (+ (- mx (ball-mx b)) (ball-x-pos b)) 
                 (+ (- my (ball-my b)) (ball-y-pos b))
                 true mx my)
      b))

;; world-after-button-up : World Integer Integer -> World
;; GIVEN: a world, and the position of the mouse
;; RETURNS: the world following a button-up at the given location.
;; EXAMPLE:
;; (world-after-button-up 
;;  (make-world (cons (make-ball 250 150 true 250 150)
;;                    (cons (make-ball 150 150 false 0 0) 
;;                          empty)))
;;  250 150) =>
;; (make-world (cons (make-ball 250 150 false 0 0)
;;                   (cons (make-ball 150 150 false 0 0) 
;;                         empty)))
;; STRATEGY: structural decomposition on w : World
(define (world-after-button-up w mx my)
  (make-world (balls-after-button-up (world-balls w) mx my)))

;; balls-after-button-up : ListOfBalls Integer Integer -> ListOfBalls
;; GIVEN: list of balls, and the location of a mouse event
;; RETURNS: list of balls
;; if the button up is inside a ball, make a ball like the given one, 
;; but set the selected? to false and the mouse location to (0, 0)
;; EXAMPLE:
;; (balls-after-mouse-event 
;;  (cons (make-ball 250 150 true 250 150)
;;        (cons (make-ball 150 150 false 0 0) 
;;              empty)) 250 150) =>
;; (cons (make-ball 250 150 false 0 0)
;;       (cons (make-ball 150 150 false 0 0) 
;;             empty))
;; STRATEGY: structural decomposition on balls : ListOfBalls
(define (balls-after-button-up balls mx my)
  (cond
    [(empty? balls) empty]
    [else (cons (ball-after-button-up (first balls) mx my)
                (balls-after-button-up (rest balls) mx my))]))

;; ball-after-button-up : Ball Integer Integer -> Ball
;; GIVEN: a ball, and the location of a mouse event
;; RETURNS: a ball
;; if the button up is inside a ball, make a ball like the given one, 
;; but set the selected? to false and the mouse location to (0, 0)
;; EXAMPLE:
;; (ball-after-mouse-event (make-ball 250 150 true 250 150) 250 150) =>
;; (make-ball 250 150 false 0 0)
;; STRATEGY: structural decomposition on b : Ball
(define (ball-after-button-up b mx my)
  (if (ball-selected? b)
      (make-ball (ball-x-pos b) (ball-y-pos b)
                 false INITIAL-VALUE INITIAL-VALUE)
      b))

;; in-ball? : Integer Integer Integer Integer -> Boolean
;; GIVEN: the coordinate (x, y) of a circle, and the 
;; the coordinate (mx, my) of the mouse
;; RETURNS: true if the mouse is inside the circle, false otherwise
;; EXAMPLE:
;; (in-ball? 0 0 0 0) => true
;; STRATEGY: function composition
(define (in-ball? x y mx my)
  (<= (+ (abs (* (- x mx) (- x mx))) 
         (abs (* (- y my) (- y my)))) 
      (* RADIUS RADIUS)))

(begin-for-test
  (check-equal? (world-after-mouse-event 
                 (make-world (cons (make-ball 250 150 false 0 0)
                                   (cons (make-ball 150 150 false 0 0) 
                                         empty)))
                 250 150 BUTTON-DOWN)
                (make-world (cons (make-ball 250 150 true 250 150)
                                  (cons (make-ball 150 150 false 0 0) 
                                        empty)))
                "botton down in one of tow balls")
  (check-equal? (world-after-mouse-event 
                 (make-world (cons (make-ball 250 150 true 250 150)
                                   (cons (make-ball 150 150 false 0 0) 
                                         empty)))
                 250 150 BUTTON-UP)
                (make-world (cons (make-ball 250 150 false 0 0)
                                  (cons (make-ball 150 150 false 0 0) 
                                        empty)))
                "botton up in one of tow balls")
  (check-equal? (world-after-mouse-event 
                 (make-world (cons (make-ball 250 150 true 250 150)
                                   (cons (make-ball 150 150 false 0 0) 
                                         empty)))
                 250 150 DRAG)
                (make-world (cons (make-ball 250 150 true 250 150)
                                  (cons (make-ball 150 150 false 0 0) 
                                        empty)))
                "drag one of tow balls")
  (check-equal? (world-after-mouse-event 
                 (make-world (cons (make-ball 250 150 false 0 0)
                                   (cons (make-ball 150 150 false 0 0) 
                                         empty)))
                 250 150 "move")
                (make-world (cons (make-ball 250 150 false 0 0)
                                  (cons (make-ball 150 150 false 0 0) 
                                        empty)))
                "recieve wrong mouse instuction, do nothing"))