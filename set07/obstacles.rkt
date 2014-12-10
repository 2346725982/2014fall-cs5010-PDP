;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname obstacles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; obstacles.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FILES REQUIRED

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS PROVIDED

(provide 
 position-set-equal?
 obstacle?
 blocks-to-obstacles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define ONE 1) ;; Minimum dimensions for a Position (1,1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Position is a (list PosInt PosInt)
;; (x y) represents the position x, y.
;; Note: this is not to be confused with the built-in data type Posn.

;; Examples: see below

;; Template:
;; position-fn : Position -> ??
;(define (position-fn p)
;  (... (first p)
;       (second p)))

;; A PositionSet is one of:
;; -- empty
;; -- (cons Position PositionSet)
;; WHERE no Position is duplicated.

;; Examples: see below

;; Template:
;; position-set-fn : PositionSet -> ??
;(define (position-set-fn ps)
;  (cond [(empty? ps) ...]
;        [else ... (position-fn (first ps))
;              ... (position-set-fn (rest ps))]))

;; A PositionSetSet is one of:
;; -- empty
;; -- (cons PositionSet PositionSetSet)
;; WHERE no two PositionSets share the same set of positions.

;; Examples: see below

;; Template:
;; position-set-set-fn : PositionSetSet -> ??
;(define (position-set-set-fn pss)
;  (cond [(empty? pss) ...]
;        [else ... (position-set-fn (first ps))
;              ... (position-set-set-fn (rest ps))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; position-set-equal? : PositionSet PositionSet -> Boolean
;; GIVEN: two PositionSets
;; RETURNS: true iff they denote the same set of positions.

;; Examples/Tests: see below

;; Strategy: Function Composition

(define (position-set-equal? ps1 ps2)
  (set-equal? ps1 ps2))


;; obstacle? : PositionSet -> Boolean
;; GIVEN: a PositionSet
;; RETURNS: true iff the set of positions would be an obstacle if they
;; were all occupied and all other positions were vacant.

;; Examples/Tests: see below

;; Strategy: Structural Decomposition on ps : PositionSet

(define (obstacle? ps)
  (cond
    [(empty? ps) false]
    [(= (length ps) ONE) true]
    [else (subset? ps (obstacle?/visited ps empty))]))

;; obstacle?/visited : PositionSet PositionSet -> PositionSet
;; PositionSet PositionSet -> PositionSet
;; Given: A position set ps and a position set visit
;; Returns: A position set comprising all valid adjacent positions
;;          to the given position set ps
;; WHERE: visit is a position set of visited positions from ps and
;;        positions adjacent to them

;; Examples (tested by obstacle?):
; (obstacle?/visited '((1 2)) empty) = '((2 3) (2 1))

;; Strategy: Structural Decomposition on ps : PositionSet

(define (obstacle?/visited ps visit)
  (cond
    [(empty? ps) visit]
    [else (obstacle?/visited (rest ps) 
                             (set-union visit 
                                        (adjacent-positions (first ps))))]))


;; adjacent-positions : Position -> ListOfPosition
;; Given: A position p
;; Returns: A list of valid positions adjacent to the given position,
;;          that is, positions that share a corner, but not an edge.

;; Examples (tested by obstacle?):
; (adjacent-positions (list 1 1)) = (list (list 2 2))
; (adjacent-positions (list 2 3)) = (list (list 3 4) (list 3 2)
;                                         (list 1 4) (list 1 2))

;; Strategy: Structural Decomposition on p : Position

(define (adjacent-positions p)
  (filter
   ;; ListOfInt -> Boolean
   ;; Returns true iff the ListOfInt is a valid Position
   (lambda (pos) (and (>= (first pos) ONE) (>= (second pos) ONE)))
   (list
    (list (add1 (first p)) (add1 (second p)))
    (list (add1 (first p)) (sub1 (second p)))
    (list (sub1 (first p)) (add1 (second p)))
    (list (sub1 (first p)) (sub1 (second p))))))


;; blocks-to-obstacles : PositionSet -> PositionSetSet
;; GIVEN: the set of occupied positions on a chessboard
;; RETURNS: the set of obstacles on that chessboard.

;; Examples/Tests: see below

;; Strategy: Structural Decomposition on ps : PositionSet

(define (blocks-to-obstacles ps)
  (cond [(empty? ps) empty]
        [else 
         (local ((define obstacle 
                   (find-obstacle (first ps) ps (list (first ps)))))
           (cons obstacle
                 (blocks-to-obstacles (set-remove obstacle (rest ps)))))]))

;; find-obstacle : Position PositionSet PositionSet -> PositionSet
;; Given: A position p, a position set to-visit and a position set acc
;; Returns: A position set representing the first obstacle in to-visit
;;          containing that position
;; Where: acc contains at least p and
;;        to-visit is a subset of some position set and
;;        acc is also a subset of that same position set,
;;        and to-visit will get smaller while acc grows in size

;; Examples (tested by blocks-to-obstacles):
; (find-obstacle '(1 2) '((1 2) (2 3) (3 4) (1 3) (11 11) (4 5)) '((1 2)))
; = (list (list 2 3) (list 1 2) (list 3 4) (list 4 5))

;; Strategy: General Recursion

;; Termination Argument: The position set to-visit will eventually be reduced
;;                       so that the intersection of to-visit and a list of
;;                       p's adjacent positions will be empty

(define (find-obstacle p to-visit acc)
  (local 
    ((define next-obstacles (set-intersect (adjacent-positions p) to-visit)))
    (cond [(empty? next-obstacles) acc]
          [else (unionall (map 
                           ;; Position -> PositionSet
                           ;; Returns the position set representing
                           ;; the largest obstacle this position is in
                           (lambda (pos) 
                             (find-obstacle pos 
                                            (set-remove next-obstacles to-visit)
                                            (cons p next-obstacles)))
                           next-obstacles))])))

;; unionall : ListOf(SetOf<X>) -> SetOf<X>
;; GIVEN: a list of sets
;; RETURNS: the union of all the sets in the list
;; EXAMPLES:
(begin-for-test 
  (check-equal? (unionall '((1 2 3) () ()))
                '(1 2 3)
                "Unionall with empty sets returns the non-empty one")
  (check-equal? (unionall '((1 2 3) (2 3 4) (3 4 5)))
                '(1 2 3 4 5)
                "Union of 3 sets combines all 3 with no duplicates"))

;; Strategy: HOFC

(define (unionall sets)
  (foldr set-union empty sets))

;; set-intersect : SetOf<X> SetOf<X> -> SetOf<X>
;; Returns the intersection of two given sets

;; Examples:
(begin-for-test (check-equal? (set-intersect empty '(1 2 3)) empty
                              "Intersection with empty set is empty")
                (check-equal? (set-intersect '(1 2 3) empty) empty
                              "Intersection with empty set is empty")
                (check-equal? (set-intersect '(1 2 3) '(2 3 4)) '(2 3)
                              "Intersection of (1 2 3) and (2 3 4) is (2 3)"))

;; STRATEGY: struct decomp on set1 : SetOf<X>

(define (set-intersect set1 set2)
  (filter (lambda (x) (my-member? x set1)) set2))

;; set-remove : SetOf<X> SetOf<X> -> SetOf<X>
;; Given: A set of X set1 and a set of X set2
;; Returns: A set like set2 but with anything from set1 removed

;; Examples:
(begin-for-test (check-equal? (set-remove empty '(1 2 3)) '(1 2 3)
                              "(1 2 3) with empty removed is (1 2 3)")
                (check-equal? (set-remove '(1 2 3) empty) empty
                              "empty with (1 2 3) removed is empty")
                (check-equal? (set-remove '(1 2 3) '(2 3 4)) '(4)
                              "(2 3 4) with (1 2 3) removed is (4)"))

(define (set-remove set1 set2)
  (filter (lambda (x) (not (my-member? x set1))) set2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFINES FOR TESTING

(define POSITION12 (list 1 2))
(define POSITION14 (list 1 4))
(define POSITION13 (list 1 3))

(define POSITION22 (list 2 2))
(define POSITION23 (list 2 3))
(define POSITION25 (list 2 5))

(define POSITION31 (list 3 1))
(define POSITION32 (list 3 2))
(define POSITION33 (list 3 3))
(define POSITION34 (list 3 4))

(define POSITION41 (list 4 1))
(define POSITION42 (list 4 2))
(define POSITION43 (list 4 3))
(define POSITION44 (list 4 4))

(define POSITION51 (list 5 1))
(define POSITION53 (list 5 3))
(define POSITION54 (list 5 4))
(define POSITION55 (list 5 5))

(define POSITION62 (list 6 2))

(define POSITION97 (list 9 7))
(define POSITION98 (list 9 8))

(define POSITION1112 (list 11 12))
(define POSITION1113 (list 11 13))

(define EMPTY-POSITION-SET empty)

(define POSITION-SET1 (list POSITION12 POSITION31 POSITION98))
(define POSITION-SET2 (list POSITION12 POSITION98 POSITION31))
(define POSITION-SET3 (list POSITION12 POSITION98 POSITION32))
(define POSITION-SET4 (list POSITION12 POSITION98 POSITION31 POSITION14))

(define POSITION-SET5 (list POSITION12 POSITION98))
(define POSITION-SET6 (list POSITION12 POSITION97))

(define POSITION-SET7 
  (list POSITION12 POSITION23 POSITION32 POSITION41 POSITION34))

(define POSITION-SET8 
  (list POSITION12 POSITION23 POSITION13 POSITION32 POSITION41 POSITION34))

(define POSITION-SET9
  (list POSITION12 POSITION23 POSITION13 POSITION32
        POSITION41 POSITION44 POSITION34))

(define POSITION-SET10
  (list POSITION32 POSITION43 POSITION54 POSITION25))

(define POSITION-SET11
  (list POSITION33 POSITION44 POSITION51 POSITION22 POSITION42 POSITION62))

(define POSITION-SET12
  (list
   POSITION12 POSITION23 POSITION62 POSITION32 POSITION41 POSITION53
   POSITION1113 POSITION34 POSITION13 POSITION44 POSITION55 POSITION1112))

(define POSITION-SET13
  (list POSITION12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(begin-for-test
  (check-equal? (position-set-equal? EMPTY-POSITION-SET POSITION-SET1)
                false
                "Empty position set not equal to non-empty set")
  (check-equal? (position-set-equal? POSITION-SET1 POSITION-SET2)
                true
                "Two position sets equal even if positions not in same order")
  (check-equal? (position-set-equal? POSITION-SET1 POSITION-SET3)
                false
                "Two position sets not equal if positions are not the same")
  (check-equal? (position-set-equal? POSITION-SET1 POSITION-SET4)
                false
                "Two position sets not equal, sizes aren't even same")
  (check-equal? (position-set-equal? POSITION-SET1 POSITION-SET5)
                false
                "Two position sets not equal, sizes aren't even same")
  (check-equal? (position-set-equal? POSITION-SET1 POSITION-SET6)
                false
                "Two position sets not equal, sizes aren't even same")
  (check-equal? 
   (obstacle? EMPTY-POSITION-SET) false
   "An empty position set has no obstacle")
  (check-equal? 
   (obstacle? POSITION-SET7)
   true
   "PositionSet7 consists of positions that result in an obstacle")
  (check-equal? 
   (obstacle? POSITION-SET8)
   false
   "PositionSet8 consists of positions that aren't an obstacle")
  (check-equal? 
   (obstacle? POSITION-SET9)
   false
   "PositionSet9 consists of positions that aren't an obstacle")
  (check-equal? 
   (obstacle? POSITION-SET10)
   false
   "PositionSet10 consists of positions that aren't an obstacle")
  (check-equal? 
   (obstacle? POSITION-SET11)
   true
   "PositionSet11 consists of positions that result in an obstacle")
  (check-equal? 
   (obstacle? POSITION-SET13)
   true
   "A position set of a single position is an obstacle")
  (check set-equal? 
         (blocks-to-obstacles POSITION-SET12)
         (list
          (list POSITION41 POSITION23 POSITION12 POSITION32 POSITION34)
          (list POSITION53 POSITION62 POSITION44 POSITION55)
          (list POSITION1113)
          (list POSITION1112)
          (list POSITION13))
         "PositionSet12 results in a set of 5 obstacles"))
