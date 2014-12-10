;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; robot.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FILE REQUIRED

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION PROVIDED

(provide path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define ZERO 0)
(define ONE 1)
(define TWO 2)
(define INITIAL-POS (list 0 0))
(define INITIAL-POS-SET (list INITIAL-POS))
(define WEST "west")
(define EAST "east")
(define SOUTH "south")
(define NORTH "north")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

;; A Position is a (list PosInt PosInt)
;; (x y) represents the position at position x, y.
;; Note: this is not to be confused with the built-in data type Posn.

;; Template:
;; position-fn : Position -> ??
;(define (position-fn p)
;  (... (first p) (second p)))

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction. 

;; Template:
;; move-fn : Move -> ??
;(define (move-fn m)
;  (... (direction-fn (first m)) (second m)))

;; A Direction is one of
;; -- "north"
;; -- "east"
;; -- "south"
;; -- "west"

;; Template:
;; direction-fn : Direction -> ??
;(define (direction-fn d)
;  (cond [(string=? NORTH d) ...]
;        [(string=? EAST d) ...]
;        [(string=? SOUTH d) ...]
;        [(string=? WEST d) ...]))

;; A Plan is a ListOf<Move>
;; WHERE: the list does not contain two consecutive moves in the same
;; direction. 

;; Template:
;; plan-fn : Plan -> ??
;(define (plan-fn p)
;  (cond [(empty? p) ...]
;        [else ... (move-fn (first p))
;              ... (rest p)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path : Position Position ListOf<Position> -> Maybe<Plan>
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.

;; EXAMPLE:

;; STRATEGY: Function Composition

(define (path start target blocks)
  (path/acc 
   start target (add-border start target blocks) empty INITIAL-POS-SET))

;; path/acc : Position Position ListOf<Position> ListOf<Position> 
;;                              ListOf<Position> -> Maybe<Path>
;; GIVEN: the current position, the target position, a list of unreachable 
;;        positions, a path, the options accumulated so far
;; RETURNS: false iff the target pos cannot be reached, otherwise output path
;; EXAMPLE: see test
;; STRATEGY: SD on options : ListOf<Position>
;;             and adj-set : ListOf<Position>
(define (path/acc now target unreachable path options)
  (cond
    [(empty? options) false]
    [(equal? now target) (combine (path->plan (reverse (cons now path))))]
    [else
     (local
       ((define adj-set (adjacent-set now unreachable))
        (define backtrack? (empty? adj-set)))
       (path/acc (if backtrack? (first options) (first adj-set))
                 target
                 (if backtrack? unreachable (cons now unreachable))
                 (if backtrack?
                     (roll-back (first options) path)
                     (cons now path))
                 (if backtrack?
                     (rest options)
                     (set-union (rest adj-set) options))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-border : Position Position ListOf<Position> -> ListOf<Position>
;; GIVEN: the start position, the targer position, and the blocks given
;; RETURNS: the new block after added the new border, using the maximun
;;          coordinates of blocks, start pos and target pos
;; EXAMPLE: see test
;; STRATEGY: function comp
(define (add-border start target blocks)
  (local
    ((define max-x (+ (max-coord start target blocks ZERO) TWO))
     (define max-y (+ (max-coord start target blocks ONE) TWO))
     (define border-x (border-generation max-x max-y ZERO))
     (define border-y (border-generation max-x max-y ONE)))
    (append border-x border-y blocks)))

;; max-coord : Position Position ListOf<Position> Integer -> Integer
;; GIVEN: the start position, the targer position, the blocks given
;;        and an integer for presenting which coordinate to calculate
;; RETURNS: the max coordinate of the corresponding position we want
;; EXAMPLE: see test
;; STRATEGY: HOFC
(define (max-coord start target blocks ind)
  (local
    ((define index (if (zero? ind) first second)))
    (foldr
     (lambda 
         ;; ->
         ;;
         (f rest) (max (index f) rest))
     (max (index target) (index start))
     blocks)))

;; border-generation : Integer Integer Integer -> ListOf<Position>
;; GIVEN: the max x coordinate, the max y coordinate and an integer
;;        showing which coordinate should be doing the operation
;; RETURNS: a list of position forming a new border outside the blocks,
;;          the start position and the target position
;; EXAMPLE: see test
;; STRATEGY: function comp
(define (border-generation f s ind)
  (local
    ((define index (if (zero? ind) f s))
     (define index1 (if (zero? ind) (sub1 f) f))
     (define index2 (if (zero? ind) s (sub1 s))))
    (cond
      [(zero? index) empty]
      [else (cons (list f s) (border-generation index1 index2 ind))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; combine : ListOf<Move> -> ListOf<Move>
;; GIVEN: a list of moves consist of move with only 1 step
;; RETURNS: a list moves combining all the adjacent moves 
;;          with the same direction
;; EXAMPLE: see test
;; STRATEGY: SD on plan : ListOf<Move>
(define (combine plan)
  (cond
    [(empty? (rest plan)) plan]
    [else  
     (if (move-direction=? (first plan) (second plan))
         (combine (cons (merge-moves (first plan) (second plan))
                        (rest (rest plan))))
         (cons (first plan) 
               (combine (rest plan))))]))

;; move-direction=? : Move Move -> Boolean
;; GIVEN: two moves
;; RETURNS: true iff they have the same direction, false otherwise
;; EXAMPLES:
(begin-for-test (check-equal? (move-direction=? (list NORTH 5) (list NORTH 3))
                              true
                              "Both given directions are North")
                (check-equal? (move-direction=? (list NORTH 5) (list EAST 5))
                              false
                              "Both given directions are different"))

;; STRATEGY: SD on mov1 and mov2 : Move
(define (move-direction=? mov1 mov2)
  (equal? (first mov1) (first mov2)))

;; merge-moves : Move Move -> Move
;; GIVEN: two moves with the same direction
;; RETURNS: a new move adding their steps together
;; EXAMPLE:
(begin-for-test 
  (check-equal? (merge-moves (list NORTH 1) (list NORTH 2))
                (list NORTH 3)
                "Combined moves of North-1 and North-2 equal North-3"))

;; STRATEGY: SD on mov1 and mov2 : Move
(define (merge-moves mov1 mov2)
  (list (first mov1) 
        (+ (second mov1) (second mov2))))

;; path->plan : ListOf<Position> -> ListOf<Move>
;; GIVEN: a path
;; RETURNS: a list of move generated by this path
;; EXAMPLE: see test
;; STRATEGY: SD on path : ListOf<Position>
(define (path->plan path)
  (cond
    [(empty? (rest path)) empty]
    [else (cons (move-generation (first path) (second path)) 
                (path->plan (rest path)))]))

;; move-generation : Position Position -> Move
;; GIVEN: two positions in the a path, a "first" and "second"
;; RETURNS: a move generated by going from the position f to s
;; EXAMPLES:
(begin-for-test (check-equal? (move-generation '(1 1) '(1 2))
                              (list SOUTH ONE)
                              "Going from 1,1 to 1,2 is an Southerly move")
                (check-equal? (move-generation '(1 1) '(1 0))
                              (list NORTH ONE)
                              "Going from 1,1 to 1,0 is an Northerly move")
                (check-equal? (move-generation '(1 1) '(2 1))
                              (list EAST ONE)
                              "Going from 1,1 to 2,1 is an Easterly move")
                (check-equal? (move-generation '(1 1) '(0 1))
                              (list WEST ONE)
                              "Going from 1,1 to 0,1 is an Westerly move"))

;; STRATEGY: SD on f and s : Position
(define (move-generation f s)
  (cond
    [(equal? (first f) (sub1 (first s))) (list EAST ONE)]
    [(equal? (first f) (add1 (first s))) (list WEST ONE)]
    [(equal? (second f) (sub1 (second s))) (list SOUTH ONE)]
    [(equal? (second f) (add1 (second s))) (list NORTH ONE)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; roll-back : Position ListOf<Position> -> ListOf<Position>
;; GIVEN: a position taken from the top of the options
;; RETURNS: the path after rolled back
;; EXAMPLE: see test
;; STRATEGY: SD on path : ListOf<Position>
(define (roll-back pos path)
  (cond
    [(empty? path) empty]
    [else (if (my-member? pos (adjacent-pos (first path)))
              path
              (roll-back pos (rest path)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjacent-set : Position ListOf<Position> -> ListOf<Position>
;; GIVEN: a position and a list of positions cannot be reached, 
;;        inluding pos visited and blocks
;; RETURNS: the possible positions adjacent to the current position,
;;          meaning that those positions cannot be visited before or in blocks
;; EXAMPLE: see test
;; STRATEGY: HOFC
(define (adjacent-set p unreachable)
  (filter
   ;; Position -> Boolean
   ;; Returns true iff the position is valid and not visited
   (lambda (f)
     (and (positive? (first f))
          (positive? (second f))
          (not (my-member? f unreachable))))
   (adjacent-pos p)))

;; adjacent-pos : Position -> ListOf<Position>
;; GIVEN: a position
;; RETURNS: the adjacent positions of the given position
;; EXAMPLES:
(begin-for-test (check-equal? (adjacent-pos INITIAL-POS)
                              '((1 0) (-1 0) (0 1) (0 -1))
                              "The adjacent positions to the initial position")
                (check-equal? (adjacent-pos '(2 3))
                              '((3 3) (1 3) (2 4) (2 2))
                              "The adjacent positions to (2 3)"))

;; STRATEGY: struct decomp on p : Position
(define (adjacent-pos p)  
  (list
   (list (add1 (first p)) (second p))
   (list (sub1 (first p)) (second p))
   (list (first p) (add1 (second p)))
   (list (first p) (sub1 (second p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST

;; DEFINE FOR TEST

(define START '(1 1))
(define TARGET '(5 5))
(define block1 '((1 2) (3 4) (4 2)))
(define block-seal-start '((3 1) (2 2) (1 3)))
(define block-seal-target '((4 4) (4 5) (4 6) (5 6) (6 6) (6 5) (6 4) (5 4)))
(define block-recall '((1 3) (1 4) (2 5) (3 2) (3 3) (3 4)))
(define block-recall2 '((3 1) (3 2) (3 3) (2 4) (1 2)))
(define block-recall3 '((1 5) (2 4) (3 2) (4 5) (4 3) (4 4) (2 6) (3 6)))
(define block-recall4 '((1 5) (2 3) (2 5) (3 1) (3 2) (3 5) (4 4)))

;; BEGIN TEST

(begin-for-test
  (check-equal? (path START TARGET block-seal-start)
                false
                "block the start position")
  (check-equal? (path START TARGET block-seal-target)
                false
                "block the target position")
  (check-equal? (path START TARGET block1) 
                (list
                 (list "east" 5)
                 (list "south" 1)
                 (list "west" 1)
                 (list "south" 1)
                 (list "east" 1)
                 (list "south" 1)
                 (list "west" 2)
                 (list "south" 1)
                 (list "east" 1))
                "normal block")
  (check-equal? (path START TARGET block-recall)
                (list
                 (list "east" 5)
                 (list "south" 1)
                 (list "west" 2)
                 (list "south" 1)
                 (list "east" 2)
                 (list "south" 1)
                 (list "west" 2)
                 (list "south" 1)
                 (list "east" 1))
                "backtracking path no.1")
  (check-equal? (path START TARGET block-recall2)
                (list
                 (list "east" 1)
                 (list "south" 2)
                 (list "west" 1)
                 (list "south" 2)
                 (list "east" 4))
                "backtracking path no.2")
  (check-equal? (path START TARGET block-recall3)
                (list
                 (list "east" 5)
                 (list "south" 1)
                 (list "west" 1)
                 (list "south" 1)
                 (list "east" 1)
                 (list "south" 1)
                 (list "west" 1)
                 (list "south" 1))
                "backtracking path no.3")
  (check-equal? (path START TARGET block-recall4)
                (list
                 (list "east" 1)
                 (list "south" 1)
                 (list "west" 1)
                 (list "south" 2)
                 (list "east" 2)
                 (list "north" 1)
                 (list "east" 3)
                 (list "south" 1)
                 (list "west" 1)
                 (list "south" 1))
                "backtracking path no.4"))