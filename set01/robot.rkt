;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require rackunit/text-ui)

(provide
  initial-robot
  robot-left
  robot-right
  robot-forward
  robot-north?
  robot-south? 
  robot-east? 
  robot-west?)

(define radius 15)
(define border-x 200)
(define border-y 400)

(define-struct robot (position-x position-y facing-direction))
;; a Robot is a
;; (make-robot Real Real String)
;;   position-x is a real number in coodinate that shows a robot's distance to the west
;;   position-y is a real number in coodinate that shows a robot's distance to the north
;;   facting-direction is a string showing the robot's facing direciton now
;;    e.g. north, south, west, east

;; robot-fn : Robot -> ??
;; (define (robot-fn r)
;;  (...
;;    (robot-position-x r)
;;    (robto-position-y r)
;;    (robot-facing-direction r)))

;initial-robot : Real Real -> Robot
;GIVEN: a set of (x,y) coordinates
;RETURNS: a robot with its center at those coordinates, facing north(up).
;Examples:
;(initial-robot 30 40) = (make-robot 30 40 "north")
;Design Strategy: Function Composition
;Function Definition
(define (initial-robot x y)
  (make-robot x y "north"));

;robot-north? : Robot -> Boolean
;robot-south? : Robot -> Boolean
;robot-east? : Robot -> Boolean
;robot-west? : Robot -> Boolean
;GIVEN: a robot
;RETURNS: whether the robot is facing in the specified direction.
;Examples:
;(robot-north? (make-robot 30 40 "north") = true
;(robot-south? (make-robot 30 40 "south") = true
;(robot-west? (make-robot 30 40 "west") = true
;(robot-east? (make-robot 30 40 "east") = true
;Design Strategy: Structural Decomposition on Robot
;Function Definition
(define (robot-north? robot)
  (equal? (robot-facing-direction robot) "north"))

(define (robot-south? robot)
  (equal? (robot-facing-direction robot) "south"))

(define (robot-east? robot)
  (equal? (robot-facing-direction robot) "east"))

(define (robot-west? robot)
  (equal? (robot-facing-direction robot) "west"))

;direction-left : String -> String
;direction-right : String -> String
;GIVEN: a robot's facing direction
;RETURNS: its facing direciton after it turns left
;or right
;Design Strategy: Function Composition
(define (direction-left d)
  (cond
    [(string=? d "north") "west"]
    [(string=? d "west") "south"]
    [(string=? d "south") "east"]
    [(string=? d "east") "north"]))

(define (direction-right d)
  (cond
    [(string=? d "north") "east"]
    [(string=? d "east") "south"]
    [(string=? d "south") "west"]
    [(string=? d "west") "north"]))

;robot-left : Robot -> Robot
;robot-right : Robot -> Robot
;GIVEN: a robot
;RETURNS: a robot like the original, but turned 90 degrees either left
;or right.
;Examples:
;(robot-left (make-robot 30 40 "north")) = (make-robot 30 40 "west")
;(robot-right (make-robot 30 40 "north")) = (make-robot 30 40 "esst")
;Design Strategy: Structural Decomposition on Robot
;Function definition
(define (robot-left robot)
  (make-robot (robot-position-x robot) 
              (robot-position-y robot) 
              (direction-left (robot-facing-direction robot))))

(define (robot-right robot)
  (make-robot (robot-position-x robot) 
              (robot-position-y robot) 
              (direction-right (robot-facing-direction robot))))

;inside-x-range? : Real -> boolean
;inside-y-range? : Real -> boolean
;GIVEN: a num indicating the x coodinate/ y coodinate of the robot
;RETURNS: a boolean, 1 if the num is inside of the range of 
;x([0, 200]) / y([0, 400]), 0 otherwise
;Examples:
;(inside-x-range? 30) = true;
;(inside-y-range? 30) = true;
;Design Strategy: Function Compostition
;Fuction Definition
(define (inside-x-range? position)
  (if (and (>= position radius) (<= position (- border-x radius)))
      true false))

(define (inside-y-range? position)
  (if (and (>= position radius) (<= position (- border-y radius)))
      true false))

;hit-border?: Robot PosInt -> Boolean
;GIVEN: a robot and the distance it is commanded to move forward
;RETURNS: a boolean number, 1 if the robot would hit the border, 0 otherwise
;Examples:
;(hit-border (make-robot 30 40 "north") 50) = true
;(hit-border (make-robot 30 40 "north") 20) = false
;Design Strategy: Function Compostition
;Function Definition
(define (hit-border? robot distance)
  (cond
    [(robot-north? robot) 
     (if (< (- (robot-position-y robot) distance) (+ 0 radius)) true false)]
    [(robot-south? robot)
     (if (> (+ (robot-position-y robot) distance) (- border-y radius)) true false)]
    [(robot-west? robot) 
     (if (< (- (robot-position-x robot) distance) (+ 0 radius)) true false)]
    [(robot-east? robot) 
     (if (> (+ (robot-position-x robot) distance) (- border-x radius)) true false)]))

;robot-forward : Robot PosInt -> Robot
;GIVEN: a robot and a distance
;RETURNS: a robot like the given one, but moved forward by the
;specified number of pixels.  If moving forward the specified number of
;pixels would cause the robot to move from being
;entirely inside the canvas to being even partially outside the canvas,
;then the robot should stop at the wall.
;examples:
;...
;Dstign Strategy: Function Composition
;Function Definition
(define (robot-forward robot distance)
  (cond
    [(robot-north? robot)
     (if (and (inside-x-range? (robot-position-x robot)) (hit-border? robot distance))
         (make-robot (robot-position-x robot) (+ 0 radius) "north")
         (make-robot (robot-position-x robot) (- (robot-position-y robot) distance) "north"))]
    [(robot-south? robot)
     (if (and (inside-x-range? (robot-position-x robot)) (hit-border? robot distance))
         (make-robot (robot-position-x robot) (- border-y radius) "south")
         (make-robot (robot-position-x robot) (+ (robot-position-y robot) distance) "south"))]
    [(robot-west? robot)
     (if (and (inside-y-range? (robot-position-y robot)) (hit-border? robot distance))
         (make-robot (+ 0 radius) (robot-position-y robot) "west")
         (make-robot (- (robot-position-x robot) distance) (robot-position-y robot) "west"))]
    [(robot-east? robot)
     (if (and (inside-y-range? (robot-position-y robot)) (hit-border? robot distance))
         (make-robot (- border-x radius) (robot-position-y robot) "east")
         (make-robot (+ (robot-position-x robot) distance) (robot-position-y robot) "east"))]))

(begin-for-test
  (check-equal? (initial-robot 30 40) (make-robot 30 40 "north") 
                "initial a robot with integer")
  (check-equal? (initial-robot 30.5 40.5) (make-robot 30.5 40.5 "north")
                "initial a robot with fractional number")
  
  (check-equal? (robot-north? (make-robot 30 40 "north")) true
                "test if a robot is facing north")
  (check-equal? (robot-north? (make-robot 30 40 "south")) false
                "test if a robot is facing north")
  (check-equal? (robot-north? (make-robot 30 40 "west")) false
                "test if a robot is facing north")
  (check-equal? (robot-north? (make-robot 30 40 "east")) false
                "test if a robot is facing north")  
  (check-equal? (robot-south? (make-robot 30 40 "north")) false
                "test if a robot is facing south")
  (check-equal? (robot-south? (make-robot 30 40 "south")) true
                "test if a robot is facing south")
  (check-equal? (robot-south? (make-robot 30 40 "west")) false
                "test if a robot is facing south")
  (check-equal? (robot-south? (make-robot 30 40 "east")) false
                "test if a robot is facing south")
  (check-equal? (robot-west? (make-robot 30 40 "north")) false
                "test if a robot is facing east")
  (check-equal? (robot-west? (make-robot 30 40 "south")) false
                "test if a robot is facing east")
  (check-equal? (robot-west? (make-robot 30 40 "west")) true
                "test if a robot is facing east")
  (check-equal? (robot-west? (make-robot 30 40 "east")) false
                "test if a robot is facing east")
  (check-equal? (robot-east? (make-robot 30 40 "north")) false
                "test if a robot is facing east")
  (check-equal? (robot-east? (make-robot 30 40 "south")) false
                "test if a robot is facing east")
  (check-equal? (robot-east? (make-robot 30 40 "west")) false
                "test if a robot is facing east")
  (check-equal? (robot-east? (make-robot 30 40 "east")) true
                "test if a robot is facing east")
  
  (check-equal? (robot-left (make-robot 30 40 "north")) (make-robot 30 40 "west")
                "test after a robot turns left")
  (check-equal? (robot-left (make-robot 30 40 "west")) (make-robot 30 40 "south")
                "test after a robot turns left")
  (check-equal? (robot-left (make-robot 30 40 "south")) (make-robot 30 40 "east")
                "test after a robot turns left")
  (check-equal? (robot-left (make-robot 30 40 "east")) (make-robot 30 40 "north")
                "test after a robot turns left")
  (check-equal? (robot-right (make-robot 30 40 "north")) (make-robot 30 40 "east")
                "test after a robot turns right")
  (check-equal? (robot-right (make-robot 30 40 "east")) (make-robot 30 40 "south")
                "test after a robot turns right")
  (check-equal? (robot-right (make-robot 30 40 "south")) (make-robot 30 40 "west")
                "test after a robot turns right")
  (check-equal? (robot-right (make-robot 30 40 "west")) (make-robot 30 40 "north")
                "test after a robot turns right")
  
  (check-equal? (robot-forward (make-robot 30 40 "north") 10) (make-robot 30 30 "north")
                "forward, north: inside -> inside")
  (check-equal? (robot-forward (make-robot 30 40 "north") 25) (make-robot 30 15 "north")
                "forward, north: inside -> inside (hit the wall)")
  (check-equal? (robot-forward (make-robot 30 40 "north") 35) (make-robot 30 15 "north")
                "forward, north: inside -> outside (partly), but cannot get out")
  (check-equal? (robot-forward (make-robot 30 40 "north") 45) (make-robot 30 15 "north")
                "forward, north: inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot 10 40 "north") 35) (make-robot 10 5 "north")
                "forward, north: (partly) outside -> outside")
  (check-equal? (robot-forward (make-robot 10 40 "north") 45) (make-robot 10 -5 "north")
                "forward, north: (partly) outside -> outside")
  (check-equal? (robot-forward (make-robot -20 40 "north") 60) (make-robot -20 -20 "north")
                "forward, north: (totally) outside -> outside")
  (check-equal? (robot-forward (make-robot 30 420 "north") 100) (make-robot 30 320 "north")
                "forward, north: outside -> inside")
  (check-equal? (robot-forward (make-robot 30 420 "north") 410) (make-robot 30 15 "north")
                "forward, north: outside -> inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot 30 420 "north") 500) (make-robot 30 15 "north")
                "forward, north: outside -> inside -> outside, but cannot get out")
  
  (check-equal? (robot-forward (make-robot 30 40 "south") 10) (make-robot 30 50 "south")
                "forward, south: inside -> inside")
  (check-equal? (robot-forward (make-robot 30 40 "south") 345) (make-robot 30 385 "south")
                "forward, south: inside -> inside (hit the wall)")
  (check-equal? (robot-forward (make-robot 30 40 "south") 355) (make-robot 30 385 "south")
                "forward, south: inside -> outside (partly), but cannot get out")
  (check-equal? (robot-forward (make-robot 30 40 "south") 400) (make-robot 30 385 "south")
                "forward, south: inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot 10 40 "south") 355) (make-robot 10 395 "south")
                "forward, south: (partly) outside -> outside")
  (check-equal? (robot-forward (make-robot 10 40 "south") 400) (make-robot 10 440 "south")
                "forward, south: (partly) outside -> outside")
  (check-equal? (robot-forward (make-robot -20 40 "south") 400) (make-robot -20 440 "south")
                "forward, south: (totally) outside -> outside")
  (check-equal? (robot-forward (make-robot 30 -30 "south") 100) (make-robot 30 70 "south")
                "forward, south: outside -> inside")
  (check-equal? (robot-forward (make-robot 30 -30 "south") 420) (make-robot 30 385 "south")
                "forward, south: outside -> inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot 30 -30 "south") 500) (make-robot 30 385 "south")
                "forward, south: outside -> inside -> outside, but cannot get out")
  
  (check-equal? (robot-forward (make-robot 30 40 "west") 10) (make-robot 20 40 "west")
                "forward, west: inside -> inside")
  (check-equal? (robot-forward (make-robot 30 40 "west") 15) (make-robot 15 40 "west")
                "forward, west: inside -> inside (hit the wall)")
  (check-equal? (robot-forward (make-robot 30 40 "west") 25) (make-robot 15 40 "west")
                "forward, west: inside -> outside (partly), but cannot get out")
  (check-equal? (robot-forward (make-robot 30 40 "west") 35) (make-robot 15 40 "west")
                "forward, west: inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot 30 -10 "west") 25) (make-robot 5 -10 "west")
                "forward, west: (partly) outside -> outside")
  (check-equal? (robot-forward (make-robot 30 -10 "west") 55) (make-robot -25 -10 "west")
                "forward, west: (partly) outside -> outside")
  (check-equal? (robot-forward (make-robot 30 -40 "west") 60) (make-robot -30 -40 "west")
                "forward, west: (totally) outside -> outside")
  (check-equal? (robot-forward (make-robot 220 40 "west") 100) (make-robot 120 40 "west")
                "forward, west: outside -> inside")
  (check-equal? (robot-forward (make-robot 220 40 "west") 210) (make-robot 15 40 "west")
                "forward, west: outside -> inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot 220 40 "west") 300) (make-robot 15 40 "west")
                "forward, west: outside -> inside -> outside, but cannot get out")
    
  (check-equal? (robot-forward (make-robot 30 40 "east") 100) (make-robot 130 40 "east")
                "forward, east: inside -> inside")
  (check-equal? (robot-forward (make-robot 30 40 "east") 155) (make-robot 185 40 "east")
                "forward, east: inside -> inside (hit the wall)")
  (check-equal? (robot-forward (make-robot 30 40 "east") 165) (make-robot 185 40 "east")
                "forward, east: inside -> outside (partly), but cannot get out")
  (check-equal? (robot-forward (make-robot 30 40 "east") 200) (make-robot 185 40 "east")
                "forward, east: inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot 30 -10 "east") 165) (make-robot 195 -10 "east")
                "forward, east: (partly) outside -> outside")
  (check-equal? (robot-forward (make-robot 30 -10 "east") 200) (make-robot 230 -10 "east")
                "forward, east: (partly) outside -> outside") 
  (check-equal? (robot-forward (make-robot 30 -40 "east") 200) (make-robot 230 -40 "east")
                "forward, east: (totally) outside -> outside")
  (check-equal? (robot-forward (make-robot -30 40 "east") 100) (make-robot 70 40 "east")
                "forward, east: outside -> inside")
  (check-equal? (robot-forward (make-robot -30 40 "east") 225) (make-robot 185 40 "east")
                "forward, east: outside -> inside -> outside, but cannot get out")
  (check-equal? (robot-forward (make-robot -30 40 "east") 300) (make-robot 185 40 "east")
                "forward, east: outside -> inside -> outside, but cannot get out")
)