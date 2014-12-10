;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; trees.rkt
;; start with (run 0)

;; example

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILES REQUIRED

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION PROVIDE

(provide
 initial-world
 run
 world-after-mouse-event 
 world-after-key-event
 world-to-roots
 node-to-center
 node-to-sons
 node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

;; NUMBER CONSTANTS
(define CANVAS-SIZE 400)
(define HALF-CANVAS-SIZE (/ CANVAS-SIZE 2))

(define NODE-SIZE 20)
(define HALF-NODE-SIZE (/ NODE-SIZE 2))

(define SON-OFFSET-X (* 2 NODE-SIZE))
(define SON-OFFSET-Y (* 3 NODE-SIZE))

(define ZERO 0)
(define INFINITY 10000)

;; IMAGE CONSTANTS
(define CANVAS (empty-scene CANVAS-SIZE CANVAS-SIZE))
(define EMPTY-CANVAS (rectangle CANVAS-SIZE CANVAS-SIZE "outline" "black"))
(define NODE-UNSELECTED (square NODE-SIZE "outline" "green"))
(define NODE-SELECTED (square NODE-SIZE "solid" "green"))
(define NODE-OUT (square NODE-SIZE "solid" "red"))

;; STRING CONSTANTS
(define NEW-ROOT-KEYEVENT "t")
(define NEW-SON-KEYEVENT "n")
(define DELETE-KEYEVENT "d")
(define DELETE-UPPER-KEYEVENT "u")
(define NOOP-KEYEVENT "a")

(define SELECT-MOUSEEVENT "button-down")
(define UNSELECT-MOUSEEVENT "button-up")
(define DRAG-MOUSEEVENT "drag")
(define NOOP-MOUSEEVENT "move")

(define BLACK "black")
(define BLUE "blue")
(define RED "red")

;; OTHER
(define NEW-NODE-POSN (make-posn HALF-CANVAS-SIZE HALF-NODE-SIZE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
(define (run any)
  (big-bang (initial-world any)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

(define-struct node (x y sons selected?))
;; A Node is a (make-node Integer Integer ListOf<Node> Boolean)
;; Interp:
;;  - x, y : the coordinate of the node's center
;;  - sons : the children of this node, it is a list of nodes 
;;  - selected? : whether or not a node is selected

;; node-fn : Node -> ???
;; (define (node-fn n)
;;   (...
;;    (node-x n)
;;    (node-y n)
;;    (sons-fn (node-sons n))
;;    (node-selected? n)))

;; A ListOf<Node> is one of:
;; -- empty
;; -- (cons node ListOf<Node>)

;; lon-fn : ListOf<Node> -> ???
;; (define (sons-fn s)
;;   (cond
;;     [(empty? s) ...]
;;     [else (...
;;            (node-fn (first s))
;;            (lon-fn (rest s)))]))

;; Examples:
;; (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE empty false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world (roots))
;; A World is a (make-world ListOf<Node>)
;; Interp:
;;  -roots : a list of roots of all trees, it's a list of nodes

;; world-fn : World -> ???
;; (define (world-fn w)
;;   (... (world-roots w)))

;; Examples:
;; (make-world empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-roots : World -> ListOf<Node>
;; GIVEN: a World
;; RETURNS: a list of all the root nodes in the given world.
;; Examples: (world-to-root EMPTY-WORLD) = empty
;; (world-to-roots EMPTY-WORLD) = empty
;; STRATEGY: Structural Decomposition on w : World
(define (world-to-roots w)
  (world-roots w))

;; node-to-center : Node -> Posn
;; GIVEN: a node
;; RETURNS: the center of the given node as it is to be displayed on the scene.
;; Examples:
;; (node-to-center EMPTY-UNSELECTED-NODE)
;; = (make-posn HALF-CANVAS-SIZE HALF-NODE-SIZE)
;; STRATEGY: Structural Decomposition on n : Node
(define (node-to-center n)
  (make-posn (node-x n) (node-y n)))

;; node-to-sons : Node -> ListOf<Node>
;; GIVEN: a node
;; RETURNS: the sons of that node
;; Examples:
;; (node-to-sons EMPTY-UNSELECTED-NODE) = empty
;; STRATEGY: Structural Decomposition on n : Node
(define (node-to-sons n)
  (node-sons n))

;; node-to-selected? : Node -> Boolean
;; GIVEN: a node 
;; RETURNS: true iff the node is selected, false otherwise
;; Examples:
;; (node-to-selected? EMPTY-UNSELECTED-NODE)  = false
;; STRATEGY: Structural Decomposition on n : Node
(define (node-to-selected? n)
  (node-selected? n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; STRATEGY: function comp
(define (initial-world any)
  (make-world empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->scene : World -> Image
;; GIVEN: a world
;; RETURNS: a scene that portrays all the elements inside
;; EXAMPLE: see test
;; STRATEGY: struct decomp on w : World
(define (world->scene w)
  (place-image (draw-roots (world-roots w))
               HALF-CANVAS-SIZE
               HALF-CANVAS-SIZE
               CANVAS))

;; draw-roots : ListOf<Node> -> Image
;; GIVEN: a list of roots recorded in the world
;; RETURNS: an image with all the nodes
;; EXAMPLE: see test
;; STRATEGY: struct decomp on n : Node
(define (draw-roots r)
  (foldr
   ;; Node Image -> Image
   ;; GIVEN: a node and an image
   ;; RETURNS: an image after that node drawn
   (lambda (n rest)
     (place-image (draw-node n)
                  HALF-CANVAS-SIZE
                  HALF-CANVAS-SIZE
                  rest))
   EMPTY-CANVAS
   r))               

;; draw-sons : ListOf<Node> Integer Integer -> Image
;; GIVEN: a list of sons
;; RETURNS: an image with all the nodes
;; EXAMPLES: see test
;; STRATEGY: struct decomp on n : Node
(define (draw-sons s px py)
  (foldr
   ;; Node Image -> Image
   ;; GIVEN: a node and an image
   ;; RETURNS: an image after that node drawn
   (lambda (n rest)
     (place-image (scene+line (draw-node n) 
                              (node-x n) 
                              (node-y n) 
                              px py 
                              BLUE)
                  HALF-CANVAS-SIZE HALF-CANVAS-SIZE rest))
   EMPTY-CANVAS
   s))

;; draw-node : Node -> Image
;; GIVEN: a node
;; RETURNS: an image after a node drawn on it according to its status
;; EXAMPLE: see test
;; STRATEGY: struct decomp on n : Node
(define (draw-node n)
  (if (node-selected? n)
      (draw-node-selected (leftmost-n n) (node-x n) (node-y n) (node-sons n))
      (draw-node-unselected (node-x n) (node-y n) (node-sons n))))

;; draw-node-selected : Integer Integer Integer ListOf<Node> -> Image
;; GIVEN: the leftmost coordinate, the location and sons of a node
;; RETURNS: an image after a node drawn on it according to its status
;; EXAMPLE: see test
;; STRATEGY: function comp
(define (draw-node-selected lm x y sons)    
  (scene+line (place-image (if (< lm ZERO) NODE-OUT NODE-SELECTED) 
                           x y 
                           (draw-sons sons x y))
              lm ZERO 
              lm CANVAS-SIZE 
              RED))

;; draw-node-unselected : Integer Integer ListOf<Node> -> Image
;; GIVEN: the original image, the location and sons of a node
;; RETURNS: an image after a node drawn on it according to its status
;; EXAMPLE: see test
;; STRATEGY: function comp
(define (draw-node-unselected x y sons)
  (place-image NODE-UNSELECTED x y (draw-sons sons x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; leftmost-n : Node -> Integer
;; GIVEN: a node
;; RETURNS: the leftmost coordinate of it
;; EXAMPLE: see test
;; STRATEGY: sturct decomp on n : Node
(define (leftmost-n n)
  (- (leftmost-coord (node-sons n) (node-x n)) HALF-NODE-SIZE))

;; leftmost-coord : ListOf<Node> Interger -> Interger
;; GIVEN: the sons and the x coordinate of a node
;; RETURNS: the leftmost coordinate of this node according to its status
;; EXAMPLE: see test
;; STRATEGY: struct decomp on s : ListOf<Node>
(define (leftmost-coord s x)
  (if (empty? s)
      x
      (- (leftmost-helper s)  
         SON-OFFSET-X)))

;; leftmost-helper : ListOf<Node> -> Integer
;; GIVEN: a list of nodes
;; RETURNS: the leftmost x coordinate of this list
;; EXAMPLE: see test
;; STRATEGY: struct decomp on n : Node
(define (leftmost-helper s)
  (foldr
   (lambda (n rest)
     (min (node-x n) rest))
   INFINITY
   s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be following the given mouse 
;; event at that location.
;; EXAMPLE:
;; (world-after-mouse-event EMPTY-WORLD 
;;                          HALF-CANVAS-SIZE HALF-CANVAS-SIZE
;;                          SELECT-MOUSEEVENT)
;; EMPTY-WORLD
;; STRATEGY: cases on mev: MouseEvent
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev SELECT-MOUSEEVENT) (world-after-button-down w mx my)]
    [(mouse=? mev DRAG-MOUSEEVENT) (world-after-drag w mx my)]
    [(mouse=? mev UNSELECT-MOUSEEVENT) (world-after-button-up w mx my)]
    [else w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-down : World Integer Integer -> World
;; GIVEN: a world and the location of a mouse
;; RETURNS: a world after a mouse button down
;; EXAMPLE:
;; (world-after-button-down EMPTY-WORLD HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;; = EMPTY-WORLD
;; (world-after-button-down WORLD-WITH-ONE-ROOT-NO-CHILDREN
;;                          HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN
;; (world-after-button-down WORLD-WITH-ONE-ROOT-NO-CHILDREN
;;                          HALF-CANVAS-SIZE HALF-NODE-SIZE)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-button-down w mx my)
  (make-world (nodes-selected-toggle (world-roots w) mx my)))

;; nodes-selected-toggle : ListOf<Node> Integer Integer -> ListOf<Node>
;; GIVEN: list of nodes, the location of a mouse 
;; RETURNS: a list of nodes like the given, but with any nodes in the
;; mouse position selected.
;; Examples:
;; (nodes-selected-toggle empty HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;; = empty
;; Strategy: Structural Decomposition on n : Node
(define (nodes-selected-toggle lon mx my)
  (map 
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node like the given but selected if the cursor is in the node
   (lambda (n) 
     (make-node (node-x n)
                (node-y n)
                (nodes-selected-toggle (node-sons n) mx my) 
                (in-node? (node-x n) (node-y n) mx my)))
   lon))

;; in-node? : Integer Integer Integer Integer -> Boolean
;; GIVEN: the location of a node and the location of the mouse
;; RETURNS: true iff the given mouse position is within the square centered
;; around the given position
;; Examples:
;; (in-node? HALF-CANVAS-SIZE HALF-CANVAS-SIZE 0 0) = false
;; Strategy: function comp
(define (in-node? x y mx my)
  (and
   (<= (- x HALF-NODE-SIZE) mx (+ x HALF-NODE-SIZE))
   (<= (- y HALF-NODE-SIZE) my (+ y HALF-NODE-SIZE))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-up : World Integer Integer -> World
;; GIVEN: a world and the location of a mouse
;; RETURNS: a world after a mouse button up
;; EXAMPLE:
;; (world-after-button-up EMPTY-WORLD HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;; = EMPTY-WORLD
;; (world-after-button-up WORLD-WITH-ONE-ROOT-NO-CHILDREN-UNSELECTED 0 0)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN
;; (world-after-button-up WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED 0 0)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-button-up w mx my)
  (make-world (nodes-selected-untoggle (world-roots w))))

;; nodes-selected-untoggle : ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes
;; RETURNS: that list of nodes after a mouse button up
;; Examples:
;; (nodes-selected-untoggle empty) = empty
;; STRATEGY: Structural Decomposition on n : Node
(define (nodes-selected-untoggle lon)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: A node like the given one, but unselected
   (lambda (n) 
     (make-node (node-x n)
                (node-y n)
                (nodes-selected-untoggle (node-sons n))
                false))
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-drag : World Integer Integer -> World
;; GIVEN: a world and the location of a mouse
;; RETURNS: a world after a mouse drag
;; EXAMPLE:
;; (world-after-drag EMPTY-WORLD HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;; = EMPTY-WORLD
;; (world-after-drag WORLD-WITH-ONE-ROOT-NO-CHILDREN 
;;                   HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN
;; (world-after-drag WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
;;                   HALF-CANVAS-SIZEHALF-CANVAS-SIZE)
;; = (make-world (list (make-node (make-posn HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;;                                empty true)))
;; STRATEGY: struct decomp on w : World
(define (world-after-drag w mx my)
  (make-world (nodes-after-drag (world-roots w) mx my)))

;; nodes-after-drag : ListOf<Node> Integer Integer -> ListOf<Node>
;; GIVEN: a list of nodes and the location of a mouse
;; RETURNS: that list of nodes after a mouse drag
;; Examples:
;; (nodes-after-drag (list EMPTY-UNSELECTED-NODE) 
;;                   HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
;; = (list EMPTY-UNSELECTED-NODE)
;; STRATEGY: Structural Decomposition on n : Node
(define (nodes-after-drag lon mx my)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node accroding to the drag event
   (lambda (n)
     (if (node-selected? n)
      (make-node
       mx 
       my
       (sons-after-drag (node-sons n) (node-x n) (node-y n) mx my)
       true)
      (make-node
       (node-x n)
       (node-y n)
       (nodes-after-drag (node-sons n) mx my)
       false)))
   lon))

;; sons-after-drag : ListOf<Node> Integer Integer 
;;                                Integer Integer -> ListOf<Node>
;; GIVEN: a list of nodes and the location of a mouse
;; RETURNS: that list of nodes after a mouse drag
;; EXAMPLE: see test
;; STRATEGY: Structural Decomposition on n : Node
(define (sons-after-drag sons px py mx my)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node accroding to the drag event
   (lambda (n)
     (make-node
      (if (node-selected? n) mx (+ (node-x n) (- mx px)))
      (if (node-selected? n) my (+ (node-y n) (- my py)))
      (sons-after-drag (node-sons n) px py mx my)
      (node-selected? n)))
   sons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world following the given key event
;; Examples:
;; (world-after-key-event EMPTY-WORLD NEW-ROOT-KEYEVENT)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN
;; STRATEGY: Cases on kev : KeyEvent
(define (world-after-key-event w kev)
  (cond
    [(string=? kev NEW-ROOT-KEYEVENT) (world-with-root-added w)]
    [(string=? kev NEW-SON-KEYEVENT) (world-with-son-added w)]
    [(string=? kev DELETE-KEYEVENT) (world-with-node-deleted w)]
    [(string=? kev DELETE-UPPER-KEYEVENT) (world-with-upper-nodes-deleted w)]
    [else w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-root-added : World -> World
;; GIVEN: a world
;; RETURNS: a world like the given but with a new root
;; EXAMPLE:
;; (world-with-root-added EMPTY-WORLD)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN
;; (world-with-root-added WORLD-WITH-ONE-ROOT-NO-CHILDREN)
;; = (make-world (list EMPTY-UNSELECTED-NODE EMPTY-UNSELECTED-NODE))
;; STRATEGY: Structural Decomposition on w : World
(define (world-with-root-added w)
  (make-world (cons (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE empty false)
                    (world-roots w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-son-added : World -> World
;; GIVEN: a world
;; RETURNS: a world like the given but with a new son added 
;; (if a root is selected)
;; EXAMPLE:
;; (world-with-son-added EMPTY-WORLD)
;; = EMPTY-WORLD
;; (world-with-son-added WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED)
;; = WORLD-WITH-ONE-ROOT-ONE-SON-SELECTED
;; STRATEGY: Structural Decomposition on w : World
(define (world-with-son-added w)
  (make-world (nodes-with-son-added (world-roots w))))

;; nodes-with-son-added : ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes
;; RETURNS: that list of nodes with a new son added to any
;; selected nodes
;; Examples:
;; (nodes-with-son-added empty) = empty
;; STRATEGY: Structural Decomposition on n : Node
(define (nodes-with-son-added lon)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node with a son added if it is selected
   (lambda (n)
     (make-node
      (node-x n) 
      (node-y n)
      (if (and (node-selected? n) (> (leftmost-n n) ZERO))
          (add-son (node-sons n) n)
          (nodes-with-son-added (node-sons n)))
      (node-selected? n)))
   lon))

;; add-son : ListOf<Node> Node -> ListOf<Node>
;; GIVEN: a list of nodes and a position of their parent/root
;; RETURNS: that list of nodes after added a node into it
;; EXAMPLE:
;; (add-son empty NEW-NODE-POSN) = (list CHILD-BELOW)
;; STRATEGY: Structural Decomposition on lon : ListOf<Node>
(define (add-son sons n)
  (cons (make-node (+ (leftmost-n n) HALF-NODE-SIZE)
                   (+ (node-y n) SON-OFFSET-Y)
                   empty
                   false)
        (nodes-with-son-added sons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-node-deleted : World -> World
;; GIVEN: a world
;; RETURNS: a world like the given but with all selected nodes deleted
;; EXAMPLE:
;; (world-with-node-deleted EMPTY-WORLD)
;; = EMPTY-WORLD
;; (world-with-node-deleted WORLD-WITH-ONE-ROOT-NO-CHILDREN)
;; = WORLD-WITH-ONE-ROOT-NO-CHILDREN
;; (world-with-node-deleted WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED)
;; = EMPTY-WORLD
;; STRATEGY: Structural Decomposition on w : World
(define (world-with-node-deleted w)
  (make-world (nodes-with-node-deleted (world-roots w))))

;; nodes-with-node-deleted : ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes like the given but with any selected nodes deleted
;; EXAMPLE:
;; (nodes-with-node-deleted empty) = empty
;; (nodes-with-node-deleted (list EMPTY-UNSELECTED-NODE))
;; = (list EMPTY-UNSELECTED-NODE)
;; (nodes-with-node-deleted (list EMPTY-SELECTED-NODE))
;; = empty
;; STRATEGY: Structural Decomposition on n : Node
(define (nodes-with-node-deleted lon)
  (foldr
   ;; Node ListOf<Node> -> ListOf<Node>
   ;; GIVEN: a node and a list of nodes
   ;; RETURNS: a list of node reconstructed according to whether it is deleted
   (lambda (n rest)
     (if (not (node-selected? n))
         (cons (make-node (node-x n)
                          (node-y n)
                          (nodes-with-node-deleted (node-sons n))
                          (node-selected? n))
               rest)
         rest))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-upper-nodes-deleted : World -> World
;; GIVEN: a world
;; RETURNS: a world like the given but with all upper nodes
;; and their children deleted
;; EXAMPLE:
;; (world-with-upper-nodes-deleted WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED)
;; = EMPTY-WORLD
;; (world-with-upper-nodes-deleted EMPTY-WORLD)
;; = EMPTY-WORLD
;; (world-with-upper-nodes-deleted 
;;       (make-world (list EMPTY-SELECTED-NODE-BOTTOM))
;; = (make-world (list EMPTY-SELECTED-NODE-BOTTOM))
;; STRATEGY: struct decomp on w : World
(define (world-with-upper-nodes-deleted w)
  (make-world (nodes-with-upper-nodes-deleted (world-roots w))))

;; nodes-with-upper-nodes-deleted : ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes like the given but with nodes
;; in the upper half of the canvas deleted
;; EXAMPLE:
;; (nodes-with-upper-nodes-deleted empty) = empty
;; (nodes-with-upper-nodes-deleted (list EMPTY-UNSELECTED-NODE))
;; = empty
;; (nodes-with-upper-nodes-deleted (list EMPTY-SELECTED-NODE))
;; = empty
;; (nodes-with-upper-nodes-deleted (list EMPTY-UNSELECTED-NODE 
;;                                       EMPTY-SELECTED-NODE-BOTTOM)
;; = (list EMPTY-SELECTED-NODE-BOTTOM)
;; STRATEGY: Structural Decomposition on n : Node
(define (nodes-with-upper-nodes-deleted lon)
  (foldr
   ;; Node ListOf<Node> -> ListOf<Node>
   ;; GIVEN: a node and a list of nodes
   ;; RETURNS: a list of node reconstructed according to whether it is deleted
   (lambda (n rest)
     (if (>= (node-y n) HALF-CANVAS-SIZE)
         (cons (make-node (node-x n)
                          (node-y n)
                          (nodes-with-upper-nodes-deleted (node-sons n))
                          (node-selected? n))
               rest)
         rest))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TEST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIFINE FOR TEST

;; NODE
(define EMPTY-UNSELECTED-NODE
  (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE empty false))

(define EMPTY-UNSELECTED-NODE-DRAGGED
  (make-node HALF-CANVAS-SIZE HALF-CANVAS-SIZE empty false))

(define EMPTY-SELECTED-NODE 
  (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE empty true))

(define EMPTY-SELECTED-NODE-DRAGGED
  (make-node HALF-CANVAS-SIZE HALF-CANVAS-SIZE empty true))

(define EMPTY-SELECTED-NODE-BOTTOM
  (make-node HALF-CANVAS-SIZE (- CANVAS-SIZE HALF-NODE-SIZE)
             empty true))

(define CHILD-BELOW
  (make-node HALF-CANVAS-SIZE (+ HALF-NODE-SIZE SON-OFFSET-Y)
             empty false))

(define CHILD-BELOW-SELECTED
  (make-node HALF-CANVAS-SIZE (+ HALF-NODE-SIZE SON-OFFSET-Y)
             empty true))

(define CHILD-LEFT
  (make-node (- HALF-CANVAS-SIZE SON-OFFSET-X)
             (+ HALF-NODE-SIZE SON-OFFSET-Y)
             empty false))

(define CHILD-BELOW-DRAGGED
  (make-node HALF-CANVAS-SIZE 
             (+ SON-OFFSET-Y HALF-CANVAS-SIZE)
             empty false))

(define CHILD-LEFT-DRAGGED
  (make-node (- HALF-CANVAS-SIZE SON-OFFSET-X)
             (+ SON-OFFSET-Y HALF-CANVAS-SIZE)
             empty false))

(define CHILD-FAR-LEFT
  (make-node SON-OFFSET-X
             (+ HALF-NODE-SIZE SON-OFFSET-Y)
             empty false))

(define SELECTED-NODE-WITH-ONE-CHILD
  (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE
             (list CHILD-BELOW) true))

(define NODE-WITH-TWO-CHILDREN 
  (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE
             (list CHILD-BELOW CHILD-LEFT) false))

(define NODE-WITH-TWO-CHILDREN-SELECTED 
  (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE
             (list CHILD-BELOW CHILD-LEFT) true))

(define NODE-WITH-TWO-CHILDREN-ONE-SELECTED 
  (make-node HALF-CANVAS-SIZE HALF-NODE-SIZE
             (list CHILD-BELOW-SELECTED CHILD-LEFT) false))

(define NODE-WITH-TWO-CHILDREN-ONE-BELOW
  (make-node HALF-CANVAS-SIZE (- CANVAS-SIZE HALF-NODE-SIZE)
             (list EMPTY-SELECTED-NODE-BOTTOM CHILD-BELOW) false))

(define NODE-WITH-TWO-CHILDREN-DRAGGED
  (make-node HALF-CANVAS-SIZE HALF-CANVAS-SIZE
             (list CHILD-BELOW-DRAGGED CHILD-LEFT-DRAGGED) true))

;; WORLD
(define EMPTY-WORLD (make-world empty))

(define WORLD-WITH-ONE-ROOT-NO-CHILDREN 
  (make-world (list EMPTY-UNSELECTED-NODE)))

(define WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
  (make-world (list EMPTY-SELECTED-NODE)))

(define WORLD-WITH-ONE-ROOT-ONE-SON-SELECTED
  (make-world (list SELECTED-NODE-WITH-ONE-CHILD)))

(define WORLD-WITH-ONE-ROOT-TWO-CHILDREN 
  (make-world (list NODE-WITH-TWO-CHILDREN)))

(define ROOT-NODE (make-node 200 10 empty false))

(define NODES-TO-DRAW (list (make-node 50 10 
                                (list (make-node 30 10 empty true)
                                      (make-node 10 10 empty false))
                                true)))

(define NODES-FOR-KEV (list (make-node 20 100 empty false)
                            (make-node 20 300 empty true)))

(define NODES-FOR-MEV (list (make-node 200 10 
                                       (list
                                        (make-node 200 70 empty false))
                                       true)))

(define NODES-TO-DRAG 
  (list (make-node 200 10 
                   (list
                    (make-node 200 70 
                               (list 
                                (make-node 210 70 empty true)
                                (make-node 200 130 empty false))
                               true))
                   false)))

(define WORLD-TO-DRAW (make-world NODES-TO-DRAW))
(define WORLD-FOR-KEV (make-world NODES-FOR-KEV))
(define WORLD-FOR-MEV (make-world NODES-FOR-MEV))
(define WORLD-TO-DRAG (make-world NODES-TO-DRAG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN TEST 
  
(begin-for-test  
  (check-equal? (world-to-roots EMPTY-WORLD) 
                empty
                "Empty world has no roots")
  (check-equal? (world-to-roots WORLD-WITH-ONE-ROOT-TWO-CHILDREN)
                (list NODE-WITH-TWO-CHILDREN)
                "World with one node with two children has one root")
  (check-equal? (node-to-center EMPTY-UNSELECTED-NODE)
                (make-posn HALF-CANVAS-SIZE HALF-NODE-SIZE)
                "Center of 'empty' node is horizontally center at top")
  (check-equal? (node-to-sons EMPTY-UNSELECTED-NODE)
                empty
                "'Empty' node has no sons")
  (check-equal? (node-to-sons NODE-WITH-TWO-CHILDREN)
                (list CHILD-BELOW CHILD-LEFT)
                "'Node with two children' has two sons")
  (check-equal? (node-to-selected? EMPTY-UNSELECTED-NODE) false
                "'Empty' node is unselected")
  (check-equal? (node-to-selected? EMPTY-SELECTED-NODE) true
                "'Empty' node is selected")
  (check-equal? (initial-world ZERO) EMPTY-WORLD
               "initial world with no nodes")
  (check-equal? 
   (world-after-mouse-event EMPTY-WORLD 
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            SELECT-MOUSEEVENT)
   EMPTY-WORLD
   "Empty world after selecting nothing changes nothing")
  (check-equal? 
   (world-after-mouse-event WORLD-WITH-ONE-ROOT-NO-CHILDREN
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            SELECT-MOUSEEVENT)
   WORLD-WITH-ONE-ROOT-NO-CHILDREN
   "Not clicking on node changes nothing")
  (check-equal? 
   (world-after-mouse-event WORLD-WITH-ONE-ROOT-NO-CHILDREN
                            HALF-CANVAS-SIZE HALF-NODE-SIZE
                            SELECT-MOUSEEVENT)
   WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
   "Clicking on node changes it to selected")
  (check-equal?
   (world-after-mouse-event EMPTY-WORLD 
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            UNSELECT-MOUSEEVENT)
   EMPTY-WORLD
   "Empty world after unselecting changes nothing")
  (check-equal? 
   (world-after-mouse-event WORLD-WITH-ONE-ROOT-NO-CHILDREN
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            UNSELECT-MOUSEEVENT)
   WORLD-WITH-ONE-ROOT-NO-CHILDREN
   "World unselecting with no selected nodes is unchanged")
  (check-equal? 
   (world-after-mouse-event WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            UNSELECT-MOUSEEVENT)
   WORLD-WITH-ONE-ROOT-NO-CHILDREN
   "World unselecting with one selected node changes")
  (check-equal?
   (world-after-mouse-event WORLD-WITH-ONE-ROOT-NO-CHILDREN
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            NOOP-MOUSEEVENT)
   WORLD-WITH-ONE-ROOT-NO-CHILDREN
   "World on non-select/drag/unselect mouse event does nothing")
  (check-equal? 
   (world-after-mouse-event EMPTY-WORLD 
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            DRAG-MOUSEEVENT)
   EMPTY-WORLD
   "Empty world doesn't change on drag")
  (check-equal? 
   (world-after-mouse-event WORLD-WITH-ONE-ROOT-NO-CHILDREN 
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            DRAG-MOUSEEVENT)
   WORLD-WITH-ONE-ROOT-NO-CHILDREN
   "World with nothing selected doesn't change on drag")
  (check-equal? 
   (world-after-mouse-event WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
                            HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                            DRAG-MOUSEEVENT)
   (make-world 
    (list (make-node HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                     empty true)))
   "World with root selected moves on drag")
  (check-equal? (nodes-selected-toggle empty 
                                       HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
                empty
                "Empty list of nodes won't be toggled")
  (check-equal? (nodes-selected-toggle (list EMPTY-UNSELECTED-NODE) 
                                       HALF-CANVAS-SIZE HALF-CANVAS-SIZE)
                (list EMPTY-UNSELECTED-NODE)
                "Selecting outside of node won't toggle it")
  (check-equal? (nodes-selected-toggle (list EMPTY-UNSELECTED-NODE) 
                                       HALF-CANVAS-SIZE HALF-NODE-SIZE)
                (list EMPTY-SELECTED-NODE)
                "Selecting inside of node will toggle it")
  (check-equal? (nodes-selected-toggle (list EMPTY-UNSELECTED-NODE CHILD-BELOW) 
                                       HALF-CANVAS-SIZE HALF-NODE-SIZE)
                (list EMPTY-SELECTED-NODE CHILD-BELOW)
                "Selecting inside first node will toggle only it")
  (check-equal? (in-node? HALF-CANVAS-SIZE HALF-CANVAS-SIZE 0 0)
                false
                "0,0 is not one half a node distance from center")
  (check-equal? (in-node? HALF-CANVAS-SIZE HALF-CANVAS-SIZE
                          (- HALF-CANVAS-SIZE HALF-NODE-SIZE)
                          (- HALF-CANVAS-SIZE HALF-NODE-SIZE))
                true
                "side of node is one half a node distance from center")
  (check-equal? (nodes-selected-untoggle empty)
                empty
                "Empty list unselected is empty")
  (check-equal? (nodes-selected-untoggle (list EMPTY-SELECTED-NODE))
                (list EMPTY-UNSELECTED-NODE)
                "Selected node turns unselected")
  (check-equal? (nodes-selected-untoggle 
                 (list EMPTY-SELECTED-NODE EMPTY-UNSELECTED-NODE))
                (list EMPTY-UNSELECTED-NODE EMPTY-UNSELECTED-NODE)
                "Selected node turns unselected, unselected node is unchanged")

  (check-equal? (world-after-key-event EMPTY-WORLD NEW-ROOT-KEYEVENT)
                WORLD-WITH-ONE-ROOT-NO-CHILDREN
                "Creating new root in an empty world makes one root")
  (check-equal? (world-after-key-event WORLD-WITH-ONE-ROOT-NO-CHILDREN
                                       NEW-ROOT-KEYEVENT)
                (make-world (list EMPTY-UNSELECTED-NODE EMPTY-UNSELECTED-NODE))
                "Creating new root in a world with one root makes two roots")
  (check-equal? (world-after-key-event WORLD-WITH-ONE-ROOT-NO-CHILDREN
                                       NOOP-KEYEVENT)
                WORLD-WITH-ONE-ROOT-NO-CHILDREN
                "World doesn't change on a non-supported keyevent")
  (check-equal? (world-after-key-event EMPTY-WORLD NEW-SON-KEYEVENT)
                EMPTY-WORLD
                "Creating a new son in an empty world does nothing")
  (check-equal? (world-after-key-event WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
                                       NEW-SON-KEYEVENT)
                WORLD-WITH-ONE-ROOT-ONE-SON-SELECTED
                "Creating a new son from a selected root does something")
  (check-equal? (world-after-key-event EMPTY-WORLD DELETE-KEYEVENT)
                EMPTY-WORLD
                "Empty world has nothing to delete")
  (check-equal? (world-after-key-event WORLD-WITH-ONE-ROOT-NO-CHILDREN
                                       DELETE-KEYEVENT)
                WORLD-WITH-ONE-ROOT-NO-CHILDREN
                "World with nothing selected has nothing to delete")
  (check-equal? (world-after-key-event WORLD-WITH-ONE-ROOT-NO-CHILDREN-SELECTED
                                       DELETE-KEYEVENT)
                EMPTY-WORLD
                "World with root selected to delete results in nothing left")
  (check-equal? (world-after-key-event EMPTY-WORLD DELETE-UPPER-KEYEVENT)
                EMPTY-WORLD
                "World with nothing doesn't change when upper nodes deleted")
  (check-equal? (world-after-key-event 
                 (make-world (list EMPTY-SELECTED-NODE-BOTTOM))
                 DELETE-UPPER-KEYEVENT)
                (make-world (list EMPTY-SELECTED-NODE-BOTTOM))
                "World with node in lower half of scene is unchanged")
  (check-equal? (nodes-with-son-added empty) empty
                "No nodes to add a son to")
  (check-equal? (nodes-with-son-added (list EMPTY-UNSELECTED-NODE))
                (list EMPTY-UNSELECTED-NODE)
                "Unselected node does not get a son")
  (check-equal? (nodes-with-son-added (list EMPTY-SELECTED-NODE))
                (list SELECTED-NODE-WITH-ONE-CHILD)
                "Selected node gets one son"))

(begin-for-test
  (check-equal? (world->scene WORLD-TO-DRAW) 
                (place-image
                 (scene+line
                  (place-image
                   NODE-OUT
                   50 10
                   (place-image 
                    (scene+line 
                     (scene+line 
                      (place-image 
                       NODE-SELECTED
                       30 10
                       EMPTY-CANVAS)
                      30 10 50 10 "blue")
                     20 0 20 400 "red")
                    200 200
                    (scene+line 
                     (place-image 
                      NODE-UNSELECTED
                      10 10
                      EMPTY-CANVAS)
                     10 10 50 10 "blue")))
                  -20 0 -20 400 "red")
                 200 200
                 CANVAS)
                "test world->scene function")
  ;; test world-to-roots, initial-world, all node functions 
  ;; and key event function"
  (check-equal? (world-after-key-event (initial-world 0) "t")
                (make-world (cons 
                             (make-node 
                              (posn-x (node-to-center ROOT-NODE))
                              (posn-y (node-to-center ROOT-NODE))
                              (node-to-sons ROOT-NODE)
                              (node-to-selected? ROOT-NODE))
                             (world-to-roots (initial-world 0))))
                "test some functions, but mostly key event T function")
  (check-equal? (world-after-key-event WORLD-FOR-KEV "n")
                (make-world (list (make-node 20 100 empty false)
                                  (make-node 20 300 
                                             (list 
                                              (make-node 20 360 empty false))
                                             true)))
                "test key event N function")
  (check-equal? (world-after-key-event WORLD-FOR-KEV "d")
                (make-world (list (make-node 20 100 empty false)))
                "test key event D function")
  (check-equal? (world-after-key-event WORLD-FOR-KEV "u")
                (make-world (list (make-node 20 300 empty true)))
                "test key event U function")
  (check-equal? (world-after-key-event (initial-world 0) "a")
                (initial-world 0)
                "test key event ELSE function")
  (check-equal? (world-after-mouse-event WORLD-FOR-MEV 200 10 "button-down")
                (make-world (list (make-node 200 10 
                                             (list
                                              (make-node 200 70 empty false))
                                             true)))
                "test mouse event BUTTON DOWN function")
  (check-equal? (world-after-mouse-event WORLD-FOR-MEV 200 10 "button-up")
                (make-world (list (make-node 200 10 
                                             (list
                                              (make-node 200 70 empty false))
                                             false)))
                "test mouse event BUTTON UP function")
  (check-equal? (world-after-mouse-event WORLD-TO-DRAG 200 70 "drag")
                (make-world 
                 (list (make-node 200 10 
                                  (list
                                   (make-node 200 70 
                                              (list 
                                               (make-node 200 70 empty true)
                                               (make-node 200 130 empty false))
                                              true))
                                  false)))
                "test mouse event DRAG function")
  (check-equal? (world-after-mouse-event (initial-world 0)  0 0 "move")
                (initial-world 0)
                "test mouse event ELSE function"))