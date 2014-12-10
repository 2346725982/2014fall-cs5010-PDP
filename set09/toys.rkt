;; toys.rkt
;; Set09-Question 1
;; Zoltan Wang, Pankaj Tripathi

;; Use (run rate speed) to run the program
;; Where rate is a positive number and speed is a positive integer
;; Toys in world of canvas that is 400x500 pixels with circle of radius 10 in
;; outline mode as target 
;; toys in world are
;; 1) child types "s", a new square-shaped(40*40) toy pops up on canvas and 
;; travelling rightward at a constant rate, bounces back from wall of canvas
;; 2)child types "c", a new circle-shaped toy of radius 5 appears and changes
;; colors from green and red on every 5 ticks
;; 3)target will have property of smooth drag

#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide World%)
(provide SquareToy%)
(provide CircleToy%)
(provide make-world)
(provide run)
(provide make-square-toy)
(provide make-circle-toy)
(provide World<%>)
(provide Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ZERO 0)
(define MINUS-ONE -1)
(define FOUR 4)
(define FIVE 5)
(define TEN 10)

(define SOLID "solid")
(define OUTLINE "outline")
(define GREEN "green")
(define RED "red")

(define SQUARE-LENGTH 40)
(define HALF-SQUARE-LENGTH (/ SQUARE-LENGTH 2))
(define SQUARE-IMAGE (square 40 "outline" "green"))

(define CIRCLE-RADIUS 5)

(define TARGET-RADIUS 10)
(define TARGET-IMAGE (circle 10 "outline" "red"))

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))



(define LEFT-EDGE HALF-SQUARE-LENGTH)
(define RIGHT-EDGE (- CANVAS-WIDTH HALF-SQUARE-LENGTH))

(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define RIGHT "right")
(define LEFT "left")
(define S "s")
(define C "c")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Direction is one of
;; -- "RIGHT"   -- square is moving to the right
;; -- "LEFT"    -- square is moving to the left
;;TEMPLATE
;; direction-fn:  Direction -> ??
;; (cond
;;   [(string=? direction RIGHT) ...]
;;   [(string=? direction LEFT) ...]))


;; A ListOfToy<%>(LOT) is one of
;; -- (empty)                      -- it is empty with no toys
;; -- (cons Toy<%> ListOfToy<%>)   -- first element is a Toy<%> and second 
;;                                    element is ListOfToy<%>
;; TEMPLATE:
;; lot-fn : LOT -> ??
;; (define (lot-fn lot)
;;   (cond
;;    [(empty? lot)...]
;;    [else (...(first lot))
;;              (lot-fn (rest lot)))]))

;; ColorString
;; A ColorString is a string that specifies the color of the toy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              INTERFACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define World<%>
  (interface ()
    
    ;; on-tick : -> World<%>
    ;; Given: no arguement
    ;; Returns: the World<%> that should follow this one after a tick
    on-tick                             
    
    ;; on-mouse : Integer Integer MouseEvent -> World<%>
    ;; Given: the coordinates of the mouse and the mouse-event
    ;; Returns: the World<%> that should follow this one after the
    ;; given MouseEvent
    on-mouse
    
    ;; on-key : KeyEvent -> World<%>
    ;; Given: the input key-event
    ;; Returns: the World<%> that should follow this one after the
    ;; given KeyEvent
    on-key
    
    ;; on-draw : -> Scene
    ;; Given : no arguement
    ;; Returns : a Scene depicting this world on it.
    on-draw 
    
    ;; target-x : -> Integer
    ;; target-y : -> Integer
    ;; GIVEN: no arguement
    ;; RETURNS: the x and y coordinates of the target
    target-x
    target-y
    
    ;; target-selected? : -> Boolean
    ;; GIVEN: no arguement
    ;; RETURNS: true iff the target is selected, flase otherwise
    target-selected?
    
    ;; get-toys : -> ListOfToy<%>
    ;; GIVEN: no arguements
    ;; RETURNS: the list of toys constructed
    get-toys
    
    ))
;-------------------------------------------------------------------------------

(define Toy<%> 
  (interface ()
    
    ;; on-tick : -> Toy<%>
    ;; GIVEN: no arguement
    ;; RETURNS: the Toy that should follow this one after a tick
    on-tick                             
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; Returns a Scene like the given one, but with this toy drawn
    ;; on it.
    add-to-scene
    
    ;; toy-x : -> Integer
    ;; toy-y : -> Integer
    ;; GIVEN: no arguement
    ;; RETURNS: the coordinates of the toy
    toy-x
    toy-y
    
    ;; toy-color : -> ColorString
    ;; GIVEN: no arguement
    ;; RETURNS: the current color of this toy
    toy-color
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               CLASS WORLD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; World%     -- a class that satisfies the World<%> interface (shown below).
;; A World is a (new World% [target Target] [toys ListOf<Toy<%>>]  
;;[speed PosInt])
;; INTERP: It represents a world, which has a Target, a list of toys and speed 
;;         with which toys move

(define World%
  (class* object% (World<%>)         
    (init-field 
     target    ;; it is a circel of radius 10
     toys      ;; toys can include number of squares and circles
               ;; a toy can be square moving right or it can be 
               ;; color changing circle
     speed)    ;; it is speed with which square moves from left to right
    
    (super-new)
    
    ;; on-tick: -> World<%>
    ;; RETURNS: the World<%> that should follow this one after a tick
    ;; EXAMPLES: It gives the world that should follow after tick 
    ;;           when called on World% object
    ;; STRATEGY: High Order Function Composition
    (define/public (on-tick)
      (new World%
           [target (send target on-tick)]
           [toys (map 
                  ;; Toy% -> Toy%
                  ;; GIVEN: a toy which can be circle or square
                  ;; RETURNS: a toy like given that should follow 
                  ;; after a tick
                  (lambda (toy) (send toy on-tick))
                  toys)]
           [speed speed]))
    
    ;; on-mouse: Integer Integer MouseEvent -> World<%>
    ;; GIVEN: x and y coordinates of mouse pointer and mousevent
    ;; RETURNS: the World<%> that should follow this one after the
    ;; given MouseEvent 
    ;; EXAMPLES: This method gives the world that should follow after it's 
    ;            called with mouse position and the mouse event based on the
    ;;           mouse event which are namely button-down, button-up, drag
    ;; STRATEGY: Function Composition
    (define/public (on-mouse x y evt)
      (new World%
           [target (send target on-mouse x y evt)]
           [toys toys]
           [speed speed]))
    
    ;; on-key: KeyEvent -> Scene
    ;; GIVEN: A KeyEvent
    ;; RETURNS: a Scene depicting this world
    ;; on it.
    ;; EXAMPLES: This gives the world that should follow after the key event
    ;;           the acceptable key events being "s" and "c" and with other key
    ;;           it will return the calling object as it is. For "s" it will
    ;;           create a  square toy and for "c" ot will create a circle toy
    ;; STRATEGY: Cases on kev: KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev S)
         (new World%
              [target target]
              [toys (cons (make-square-toy (target-x) (target-y) speed) toys)]
              [speed speed])]
        [(key=? kev C)
         (new World%
              [target target]
              [toys (cons (make-circle-toy (target-x) (target-y)) toys)]
              [speed speed])]
        [else this]))
    
    ;; on-draw: -> Scene
    ;; RETURNS: a Scene depicting this world
    ;; on it.
    ;; EXAMPLES: This method will add a circle toy or a square toy to the scene
    ;;           when called on World% object.
    ;; STRATEGY: High Order Function Composition
    (define/public (on-draw)
      (local
        ((define scene-with-target (send target on-draw EMPTY-CANVAS)))
        (foldr
         ;; Toy% scene ->scene
         ;; GIVEN: a toy and a scene
         ;; RETURNS: a scene after toy is painted on it
         (lambda (toy scene)
           (send toy add-to-scene scene))
         scene-with-target
         toys)))
    
    ;; target-x: -> Integer
    ;; target-y: -> Integer
    ;; RETURNS: below mentioned functions returns
    ;; the x and y coordinates of the target respectively
    ;; EXAMPLES: These methods return the x and y coordinates of center of the 
    ;;           target when called on Target% object
    ;; STRATEGY: Function Composition
    (define/public (target-x) (send target target-x))
    (define/public (target-y) (send target target-y))
    
    ;; target-selected?: -> Boolean
    ;; RETURNS: true if the target is selected
    ;; EXAMPLES: These methods return whether the target is selected or not.
    ;;           if it is selected then it returns true else it returns false.
    ;; STRATEGY: Function Composition
    (define/public (target-selected?) (send target target-selected?))
    
    ;; get-toys: -> ListOfToy<%>
    ;; RETURNS: all toys in the world
    ;; EXAMPLES: This method returns the list of toys when called on the 
    ;;           World% object.
    ;; STRATEGY: Function Composition
    (define/public (get-toys) toys)
     
    ;; Methods to be used for testing
    
    ;; world-target: -> Target%
    ;; RETURNS: a Target% object
    ;; EXAMPLES: This method returns the target when called on World% object
    ;; STRATEGY: Function Composition
    (define/public (world-target) target)
    
    ;; world-target: -> PosInt
    ;; RETURNS: a speed with which the square toy moves
    ;; EXAMPLES: This method returns the speed with which the square toy moves
    ;;           when called on World% object
    ;; STRATEGY: Function Composition
    (define/public (world-speed) speed)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              CLASS TARGET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Target is a (new Target% [x Integer] [y Integer] [selected? Boolean]
;;                            [mx Integer] [my Integer])
;; INTERP: It represents a Target, which it self is a circle and can be smoothly
;; dragged on the canvas

(define Target%
  (class* object% (World<%>)         
    (init-field 
     x                      ;; the x position of the target
     y                      ;; the y position of the target
     [selected? false]      ;; indicate whether the target is selected
     [mx ZERO]              ;; the x position of the mouse pointer in the target
     [my ZERO])             ;; the y position of the mouse pointer in the target
    
    (field [IMG TARGET-IMAGE]) ;; image for displaying the target
    
    (super-new)
    
    ;; on-tick : -> World
    ;; RETURNS: A world like this one, but as it should be after a tick
    ;; EXAMPLES: It gives the target that should follow after tick 
    ;;           when called on Target% object. It gives the current location
    ;;           and state(selected or not) of the target after a tick. 
    ;; STRATEGY: Function Composition
    (define/public (on-tick)
      (new Target% 
           [x x]
           [y y]
           [selected? selected?]
           [mx mx]
           [my my]))
    
    ;; on-mouse : Interger Integer MouseEvent -> World
    ;; GIVEN: mouse x and y position with mouse event
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given mouse event.
    ;; EXAMPLES: This method gives the target that should follow after it's 
    ;            called with mouse position and the mouse event. Based on the
    ;;           mouse event which are namely button-down, button-up, drag the
    ;;           corresponding methods are called
    ;; STRATEGY: Cases on evt: MouseEvent
    (define/public (on-mouse mouse-x mouse-y evt)
      (cond
        [(mouse=? evt BUTTON-DOWN)
         (send this target-after-button-down mouse-x mouse-y)]
        [(mouse=? evt DRAG) 
         (send this target-after-drag mouse-x mouse-y)]
        [(mouse=? evt BUTTON-UP)
         (send this target-after-button-up)]
        [else this]))
    
    ;; target-after-button-down : Integer Integer ->Target%
    ;; GIVEN: the location of a mouse event
    ;; RETURNS: the target that should follow this one after a button
    ;; down at the given location
    ;; DETAILS:  If the event is inside
    ;; the target, returns a target just like this target, except that it is
    ;; selected.  Otherwise returns the target unchanged.
    ;; EXAMPLES: This method returns the target with it being selected only 
    ;;           if the mouse pointers are in the target when called on 
    ;;           Target% object. 
    ;; STRATEGY: Function Composition
    (define/public (target-after-button-down mouse-x mouse-y)
      (if (send this in-target? mouse-x mouse-y)
          (new Target% 
               [x x]
               [y y]
               [selected? true]
               [mx mouse-x]
               [my mouse-y])
          this))
    
    ;; target-after-drag : Integer Integer -> ->Target%
    ;; GIVEN: the location of a mouse event
    ;; RETURNS: the target that should follow this one after a drag at
    ;; the given location 
    ;; DETAILS: if target is selected, move the target to the mouse location,
    ;; otherwise ignore.
    ;; EXAMPLES: This method retuerns the changed location of the target 
    ;;           with it being selected and dragged to a new location
    ;; STRATEGY: Function Composition
    (define/public (target-after-drag mouse-x mouse-y)
      (if selected?
          (new Target% 
               [x (+ x (- mouse-x mx))]
               [y (+ y (- mouse-y my))]
               [selected? true]
               [mx mouse-x]
               [my mouse-y])
          this))
    
    ;; target-after-button-up : -> Target%
    ;; RETURNS: the target that should follow this one after a button-up
    ;; DETAILS: button-up unselects all targets
    ;; EXAMPLES: This method returns the location of the target with it being 
    ;;           unselected when called on Target% object
    ;; STRATEGY: Function Composition
    (define/public (target-after-button-up)
      (new Target% 
           [x x]
           [y y]
           [selected? false]))
    
    ;; in-target? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this target.
    ;; EXAMPLES: This method checks and returns a boolean value if the mouse
    ;;           pointer is in the target. It returns true if mouse pointer
    ;;           is in the target else false
    ;; STRATEGY: Function Composition
    (define/public (in-target? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr TARGET-RADIUS)))
    
    ;; on-key: KeyEvent -> Target%
    ;; GIVEN: KeyEvent
    ;; RETURNS: the Target% that should follow this one after the key event
    ;; EXAMPLES: This method returns the target object as it is after it is
    ;;           called on Target% object 
    ;; STRATEGY: Function Composition
    (define/public (on-key kev)
      this)
    
    ;; on-draw: -> Scene
    ;; RETURNS: a Scene depicting this world
    ;; on it.
    ;; EXAMPLES: This method will add a target that is a circle of radius
    ;;           10 pixels to the scene when called on Target% object.
    ;; STRATEGY: Function Composition
    (define/public (on-draw scene)
      (place-image IMG x y scene))
    
    ;; target-x: -> Integer
    ;; target-y: -> Integer
    ;; RETURNS: the x and y coordinates of the target
    ;; EXAMPLES: These methods return the x and y coordinates of center of 
    ;;           the target when called on Target% object          
    ;; STRATEGY: Function Composition
    (define/public (target-x) x)
    (define/public (target-y) y)
    
    ;; target-selected?: -> Boolean
    ;; RETURNS: Is the target selected?
    ;; EXAMPLES: These methods return whether the target is selected or not.
    ;;           if it is selected then it returns true else it returns false.
    ;; STRATEGY: Function Composition
    (define/public (target-selected?) selected?)
    
    ;; get-toys: -> ListOfToy<%>
    ;; RETURNS: empty. no toys present in target
    ;; EXAMPLES: This method returns empty list as target won't have any toys 
    ;;           in it when called on Target% object
    ;; STRATEGY: Function Compositon
    (define/public (get-toys) empty)
    
    ;; Methods to be used for testing
    
    ;; target-mx: -> Integer
    ;; target-my: -> Integer
    ;; RETURNS: the x and y coordinates of center of the mouse pointer
    ;; EXAMPLES: This method returns the mouse pointers position when called on
    ;;           Target% object.
    ;; STRATEGY: Function Composition
    (define/public (target-mx) mx)
    (define/public (target-my) my)
    
    ;; testing-case: Target% -> Boolean
    ;; GIVEN: a Target%.
    ;; RETURNS: true if the current calling target object has same values as 
    ;;          obtained when individual functions are called for each field
    ;; EXAMPLES: This method returns true if the current calling target object
    ;;           has same values as obtained when individual functions are 
    ;;           called for each field else it will return false.
    ;; STRATEGY: Function Composition
    (define/public (testing-case obj)
      (and (= x (send obj target-x))
           (= y (send obj target-y))
           (equal? selected? (send obj target-selected?))
           (= mx (send obj target-mx))
           (= my (send obj target-my))))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            CLASS SQUARETOY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquareToy% -- a class that satisfies the Toy<%> interface
;; A SquareToy is a (new SquareToy% [x Integer] [y Integer] 
;;                                  [speed PosInt] [direction Direction]
;;                                  [color ColorString])
;; INTERP: A SquareToy represents a square-shaped toy
(define SquareToy%
  (class* object% (Toy<%>) 
    (init-field 
     x                     ;; the x position of the toy
     y                     ;; the y position of the toy
     speed                 ;; the speed with which the toy travels
     [direction RIGHT]     ;; indicates the direction of the  moving square
     [color GREEN])        ;; indicate the color of the toy
    
    (field [IMG SQUARE-IMAGE])  ;; image for displaying the toy
    
    (super-new)
    
    ;; on-tick: -> SquareToy%
    ;; RETURNS: the Toy that should follow this one after a tick
    ;; EXAMPLES: It gives the square toy that should follow after tick 
    ;;           when called on SquareToy% object. It gives the direction
    ;;           and speed of the square toy after a tick. 
    ;; STRATEGY: Structural Decompostion on direction: Direction
    (define/public (on-tick)
      (cond
        [(string=? direction RIGHT)
         (move-right)]
        [(string=? direction LEFT)
         (move-left)]))
    
    ;; move-right: ->SquareToy%
    ;; RETURNS: a SquareToy% with direction changed if it touches the wall
    ;;          to left else it will be right
    ;; EXAMPLES: This method checks whether the square toy is moving in the 
    ;;           right direction  and if while moving to right it touches the
    ;;           wall then it should change its direction and start moving left
    ;; STRATEGY: Function Composition
    (define/public (move-right)
      (if (>= (+ x speed) RIGHT-EDGE)
          (new SquareToy%
               [x RIGHT-EDGE]
               [y y]
               [speed speed]
               [direction LEFT]
               [color color])
          (new SquareToy%
               [x (+ x speed)]
               [y y]
               [speed speed]
               [direction RIGHT]
               [color color])))
               
    ;; move-left: ->SquareToy%
    ;; RETURNS: a SquareToy% with direction changed if it touches the wall
    ;;          to right else it will be left
    ;; EXAMPLES: This method checks whether the square toy is moving in the 
    ;;           left direction  and if while moving to left it touches the
    ;;           wall then it should change its direction and start moving right
    ;; STRATEGY: Function Composition
    (define/public (move-left)
      (if (<= (- x speed) LEFT-EDGE)
          (new SquareToy%
               [x LEFT-EDGE]
               [y y]
               [speed speed]
               [direction RIGHT]
               [color color])
          (new SquareToy%
               [x (- x speed)]
               [y y]
               [speed speed]
               [direction LEFT]
               [color color])))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the given one, but with square toy drawn
    ;;          on it.
    ;; EXAMPLES: This method will add a square toy that is a square of side 40
    ;;           pixels to the scene when called on SquareToy% object.
    ;; STRATEGY: Function Composition
    (define/public (add-to-scene scene)
      (place-image IMG x y scene))
    
    ;; toy-x: -> Integer
    ;; toy-y: -> Integer
    ;; RETURNS: x and y coordinates of the square toy repectively
    ;; EXAMPLES: These methods return the x and y coordinates of center of 
    ;;           the square toy when called on SquareToy% object          
    ;; STRATEGY: Function Composition
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-color: -> ColorString
    ;; RETURNS: the current color of this toy
    ;; EXAMPLES: This method returns the color of the square which will be green
    ;;           when called on SquareToy% object.
    ;; STRATEGY: Function Composition
    (define/public (toy-color) color)
    
    ;; Methods to be used for testing
    
    ;; toy-speed: -> PosInt
    ;; RETURNS: speed of SquareToy%
    ;; EXAMPLES: This method returns the speed with which the square toy is
    ;;           moving when called on SquareToy% object.
    ;; STRATEGY: Function Composition
    (define/public (toy-speed) speed)
    
    ;; toy-direction: -> Direction
    ;; RETURNS: direction of SquareToy%
    ;; EXAMPLES: This method returns the direction in which the square toy is
    ;;           moving when called on SquareToy% object.
    ;; STRATEGY: Function Composition
    (define/public (toy-direction) direction)
    
    ;; testing-case: SquareToy% -> Boolean
    ;; GIVEN: a SquareToy%.
    ;; RETURNS: true if the current calling SquareToy% object has same values  
    ;;          as obtained when individual functions are called for each field
    ;; EXAMPLES: This method returns true if the current calling SquareToy% 
    ;;           object has same values as obtained when individual functions  
    ;;           are called for each field else it will return false.
    ;; STRATEGY: Function Composition
    (define/public (testing-case obj)
      (and (= x (send obj toy-x))
           (= y (send obj toy-y))
           (= speed (send obj toy-speed))
           (string=? direction (send obj toy-direction))
           (equal? color (send obj toy-color))))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            CLASS CIRCLETOY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CircleToy% -- a class that satisfies the Toy<%> interface
;; A CircleToy is a (new CircleToy% [x Integer] [y Integer] 
;;                                  [count NonNegInt][color ColorString]
;; INTERP: A CircleToy represents a circle-shaped toy

(define CircleToy%
  (class* object% (Toy<%>)
    (init-field 
     x                     ;; the x position of the toy
     y                     ;; the y position of the toy
     [count ZERO]          ;; a counter to count the number of ticks of the toy
     [color GREEN])        ;; indicates the color of the toy that changes with
                           ;; every five ticks
    
    ;; image for displaying the toy
    (field [IMG (circle CIRCLE-RADIUS SOLID color)]) 
    
    (super-new)
    
    ;; on-tick: -> SquareToy%
    ;; RETURNS: the Toy that should follow this one after a tick
    ;; EXAMPLES: It gives the circle toy that should follow after tick 
    ;;           when called on CircleToy% object. It gives the count
    ;;           and olor of the CircleToy% after a tick. 
    ;; STRATEGY: Function Composition
    (define/public (on-tick)
      (if (= count FOUR)
          (new CircleToy%
               [x x]
               [y y]
               [count ZERO]
               [color (change-color color)])
          (new CircleToy%
               [x x]
               [y y]
               [count (add1 count)]
               [color color])))
    
    ;; change-color: ColorString -> ColorString
    ;; GIVEN: a colorstring
    ;; RETURNS: a colorstring with color changed for fifth tick
    ;; EXAMPLES: This method returns the color of the CircleToy% with it being
    ;;           changed on fifth tick else it will be unchanged
    ;; STRATEGY: Function Composition
    (define/public (change-color color)
      (if (string=? color GREEN)
          RED
          GREEN))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: A scene
    ;; RETURNS: a Scene like the given one, but with circle toy drawn
    ;;          on it.
    ;; EXAMPLES: This method will add a cirlce toy that is a circle of radius 5
    ;;           pixels to the scene when called on CircleToy% object.
    ;; STRATEGY: Function Composition
    (define/public (add-to-scene scene)
      (place-image IMG x y scene))
    
    ;; toy-x:-> Int
    ;; toy-y:-> Int
    ;; RETURNS: x and y coordinates of this toy
    ;; EXAMPLES: These methods return the x and y coordinates of center of 
    ;;           the circle toy when called on CircleToy% object          
    ;; STRATEGY: Function Composition
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-color:-> ColorString
    ;; RETURNS: the current color of this toy
    ;; EXAMPLES: This method returns the color of the circle toy which changes
    ;;           after every fifth tick. It will be green initially.
    ;; STRATEGY: Function Composition
    (define/public (toy-color) color)
    
    ;; Methods to be used for testing
    
    ;; toy-count: -> NonNegInt
    ;; RETURNS: the current color of this toy
    ;; EXAMPLES: This method returns the color of the circle toy which changes
    ;;           after every fifth tick. It will be green initially.
    ;; STRATEGY: Function Composition
    (define/public (toy-count) count)
    
    ;; testing-case: CircleToy% -> Boolean
    ;; GIVEN: a CircleToy%.
    ;; RETURNS: true if the current calling CircleToy% object has same values  
    ;;          as obtained when individual functions are called for each field
    ;; EXAMPLES: This method returns true if the current calling CircleToy% 
    ;;           object has same values as obtained when individual functions  
    ;;           are called for each field else it will return false.
    ;; STRATEGY: Function Composition
    (define/public (testing-case obj)
      (and (= x (send obj toy-x))
           (= y (send obj toy-y))
           (= count (send obj toy-count))
           (equal? color (send obj toy-color))))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            MAKE-WORLD METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-world : PosInt -> World%
;; GIVEN: a positive integer which is speed in (in pixels/tick).
;; RETURNS: a world with a target, but no toys, and in which any
;; toys created in the future will travel at the given speed (in pixels/tick).
;; EXAMPLES: This method returns a World% object when called with speed passed
;;           to it. A world with a target, but no toys, and in which any
;;           toys created in the future will travel at the given speed 
;;           (in pixels/tick).
;; STRATEGY: Function Composition

(define (make-world s)
  (new World% 
       [target (new Target% [x HALF-CANVAS-WIDTH][y HALF-CANVAS-HEIGHT])]
       [toys empty]
       [speed s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              RUN METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosNum PosInt -> World%
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;;       creates and runs a world.  
;; RETURNS: the final state of the world.
;; STRATEGY: High Order Function Composition

(define (run rate speed)
  (big-bang (make-world speed)
            (on-tick
             ;; World -> World
             ;; GIVEN: a world
             ;; RETURNS: a world after tick
             (lambda (w) (send w on-tick))
             rate)
            (on-draw
             ;; World -> World
             ;; GIVEN: a world
             ;; RETURNS: a world drawn on the scene
             (lambda (w) (send w on-draw)))
            (on-key
             ;; World MyKeyEvent -> World
             ;; GIVEN: a world and a key event
             ;; RETURNS: a world after a key event
             (lambda (w kev) (send w on-key kev)))
            (on-mouse
             ;; World Integer Integer MyMouseEvent -> World
             ;; GIVEN: a world, mouse location(x,y coordinates) and a mouse
             ;; event
             ;; RETURNS: a world after a mouse event
             (lambda (w x y evt) (send w on-mouse x y evt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           MAKE-SQUARE-TOY METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-square-toy : PosInt PosInt PosInt -> SquareToy%
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;;          travelling right at the given speed.
;; EXAMPLES: This method returns a an object representing a square toy at the 
;;           given position, travelling right at the given speed. It is called
;;           with the position and speed of square toy passed to it.
;; STRATEGY: Function Composition

(define (make-square-toy toy-x toy-y toy-speed)
  (new SquareToy% 
       [x toy-x]
       [y toy-y]
       [speed toy-speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           MAKE-CIRCLE-TOY METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-circle-toy : PosInt PosInt -> CircleToy%
;; GIVEN: an x and a y position
;; RETURNS: an object representing a circle toy at the given position.
;; EXAMPLES: This method returns a an object representing circle toy at the 
;;           given position. It is called with the position of circle toy passed
;;           to it.          
;; STRATEGY: Function Composition

(define (make-circle-toy toy-x toy-y)
  (new CircleToy% 
       [x toy-x]
       [y toy-y]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                  TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define world-on-draw
  (place-image (square 40 "outline" "green") 300 150
               (place-image (circle 5 "solid" "green") 200 200
                            (place-image (circle 10 "outline" RED) 
                                         200 250 EMPTY-CANVAS))))

(define empty-world (make-world 8))

(define target1 (new Target% [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT]
                            [selected? false] [mx 0] [my 0]))

(define square1 (new SquareToy% [x 370] [y HALF-CANVAS-HEIGHT] 
                                   [speed 8][direction RIGHT] [color GREEN]))

(define square2 (new SquareToy% [x 360] [y HALF-CANVAS-HEIGHT] 
                                   [speed 8][direction RIGHT] [color GREEN]))

(define world1 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 300][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN])
                                       (new CircleToy% [x 200][y 200]
                                                       [count 0][color GREEN]))]
                           [speed 8]))

(define world2 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 308][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN])
                                       (new CircleToy% [x 200][y 200]
                                                       [count 1][color GREEN]))]
                           [speed 8]))

(define world3 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 300][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN]))]
                           [speed 8]))

(define world4 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 308][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN]))]
                           [speed 8]))

(define world5 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new CircleToy% [x 300][y 150]
                                                       [count 4][color RED]))]
                           [speed 8]))

(define world6 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new CircleToy% [x 300][y 150]
                                                      [count 0][color GREEN]))]
                           [speed 8]))

(define world7 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new CircleToy% [x 300][y 150]
                                                      [count 3][color GREEN]))]
                           [speed 8]))

(define world8 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new CircleToy% [x 300][y 150]
                                                      [count 4][color GREEN]))]
                           [speed 8]))

(define world9 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 400][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN]))]
                           [speed 8]))

(define world10 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 380][y 150]
                                                      [speed 8][direction LEFT]
                                                      [color GREEN]))]
                           [speed 8]))

(define world11 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 0][y 150]
                                                       [speed 8][direction LEFT]
                                                       [color GREEN]))]
                           [speed 8]))

(define world12 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 20][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN]))]
                           [speed 8]))

(define world13 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 20][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN]))]
                           [speed 8]))

(define world14 (new World% [target (new Target% [x 200][y 250]
                                                [selected? true]
                                                [mx 205][my 255])]
                           [toys (list (new SquareToy% [x 20][y 150]
                                                     [speed 8][direction RIGHT]
                                                     [color GREEN]))]
                           [speed 8]))

(define world15 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new CircleToy% [x 300][y 150]
                                                       [count 4][color GREEN]))]
                           [speed 8]))

(define world16 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new CircleToy% [x 300][y 150]
                                                      [count 0][color RED]))]
                           [speed 8]))
(define world17 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 80][y 150]
                                                       [speed 8][direction LEFT]
                                                       [color GREEN]))]
                           [speed 8]))
(define world18 (new World% [target (new Target% [x 200][y 250]
                                                [selected? false]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 72][y 150]
                                                       [speed 8][direction LEFT]
                                                       [color GREEN]))]
                           [speed 8]))


(define world-selected (new World% [target (new Target% [x 200][y 250]
                                                [selected? true]
                                                [mx 0][my 0])]
                           [toys (list (new SquareToy% [x 308][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN]))]
                           [speed 8]))


(define world-selected-res (new World% [target (new Target% [x 220][y 270]
                                                [selected? true]
                                                [mx 20][my 20])]
                           [toys (list (new SquareToy% [x 308][y 150]
                                                      [speed 8][direction RIGHT]
                                                      [color GREEN]))]
                           [speed 8]))

(define world-with-square
  (new World%
       [target (new Target% [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT]
                            [selected? false] [mx 0] [my 0])]
       [toys (list (new SquareToy% [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT] 
                                   [speed 8] [color GREEN]))]    
       [speed 8]))

(define world-with-circle
  (new World%
       [target (new Target% [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT]
                            [selected? false] [mx 0] [my 0])]
       [toys (list (new CircleToy% [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT] 
                                   [count 0] [color GREEN]))]    
       [speed 8]))



;; compare-world? : World% World% ->Boolean
;; GIVEN: two worlds for comparison
;; RETURNS: true iff two worlds are same
;; EXAMPLE: as per test cases
;; STRATEGY: High Order Function Composition

(define (compare-world? world1 world2)
  (and (send (send world1 world-target) testing-case 
                     (send world2 world-target))
       (andmap
        ; Toy% Toy% -> Boolean
        ; GIVEN: two toy objects
        ; RETURNS: true iff the two toys are same
        (lambda (t1 t2) (send t1 testing-case t2))
        (send world1 get-toys)
        (send world2 get-toys))
       (equal? (send world1 world-speed) (send world2 world-speed))))

(begin-for-test
  (check-equal? (send world1 on-draw) world-on-draw
                "Test case to check on-draw of world")
  (check-equal? (send target1 get-toys) empty
                "Test case to check get toys function of target"))
                 

(begin-for-test
  (check-true (compare-world? (send world1 on-mouse 20 20 "button-down")
                               world1)
              "Test case to check the button-down mouse event")
  (check-true (compare-world? (send world1 on-mouse 20 20 "button-up")
                               world1)
              "Test case to check the button-up mouse event")
  (check-true (compare-world? (send world-selected on-mouse 20 20 "drag")
                               world-selected-res)
              "Test case to check the drag mouse event when target selected")
  (check-true (compare-world? (send world1 on-mouse 20 20 "drag")
                               world1)
              "Test case to check the drag mouse event when target unselected")
  (check-true (compare-world? (send world1 on-mouse 20 20 "move")
                               world1)
              "Test case to check the mouse event other than button-down
               button-up and drag")
  (check-true (compare-world? (send empty-world on-key "s")
                               world-with-square)
              "Test case to check the key event s")
  (check-true (compare-world? (send empty-world on-key "c")
                               world-with-circle)
              "Test case to check the key event c")
  (check-true (compare-world? (send empty-world on-key " ")
                               empty-world)
              "Test case to check the key event other than s and c")
  (check-true (compare-world? (send world7 on-tick) world8)
              "Test to check the default on tick function")
  (check-true (compare-world? (send world5 on-tick) world6)
              "Test cases to check whether the circle toy changes color
               on fifth tick red-green")
  (check-true (compare-world? (send world15 on-tick) world16)
              "Test cases to check whether the circle toy changes color
               on fifth tick green-red")
  (check-true (compare-world? (send world7 on-tick) world8)
              "Test cases to check whether the circle toy changes color
               on fifth tick")
  (check-true (compare-world? (send world9 on-tick) world10) 
              "Test case to check whether square bounces off the right wall")
  (check-true (compare-world? (send world11 on-tick) world12) 
              "Test case to check whether square bounces off the left wall")
  (check-true (compare-world? (send world3 on-tick) world4) 
              "Test case to check whether square is moving correctly to right")
  (check-true (compare-world? (send world17 on-tick) world18) 
              "Test case to check whether square is moving correctly to left")
  (check-true (send (send target1 on-key " " ) testing-case target1)
              "Test case for on-key of target")
  (check-true (compare-world? (send world13 on-mouse 205 255 "button-down")
                               world14) 
              "Test case to check button-down in target")
  (check-true (send world-selected target-selected?) 
              "Test case for target-selected of class world"))


