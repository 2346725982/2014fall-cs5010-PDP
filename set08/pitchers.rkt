;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pitchers) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;; pitchers.rkt
;; Set08-Question 1-2
;; Zoltan Wang, Pankaj Tripathi
;; There are few pitchers provided with their capacity and the content.
;; The requirement is to fill the given quantity of water in any of these
;; provided pitchers such that the pitcher holds exactly the given quantity of 
;; water.

;; ALGORITHM:
;; In this program, we use DFS to solve the problem.
;; We use this algorithm to iterate through the list of pitchers to
;; check whether the move the goal amount can be moved to a new pitcher that 
;; is the target thereby resulting into list of moves 

(require rackunit)
(require "extras.rkt")

(provide list-to-pitchers)
(provide pitchers-to-list)
(provide pitchers-after-moves)
(provide make-move)
(provide move-src)
(provide move-tgt)
(provide solution)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ZERO 0)
(define ONE 1)
(define TWO 2)
(define FILL "fill")
(define DUMP "dump")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PitcherInternalRep is a (list PosInt PosInt PosInt)
;; 1.PosInt represents order of the pitcher in the list
;; WHERE: the PosInt is in increasing order and (list PosInt PosInt PosInt)
;; is always not empty and has both two elements in it.
;; 2.PosInt represents capacity of the pitcher
;; 3.PosInt represents contents of the pitcher
;; TEMPLATE:
;; pitcherinternalRep-fn : PitcherInternalRep -> ??
;; (define (pitcherinternalRep-fn ir)
;;  (... (first ir)(second ir) (third ir))))

;; A PitchersInternalRep is a ListOfPitcherInternalRep (LOPI), it is one of
;; -- empty                           -- it is empty with no pitchers
;; -- (cons PitcherInternalRep LOPI)  -- First element of the LOPI is
;;                                       PitcherInternalRep and second is LOPI
;; INTERPRETATION:
;; In (list PosInt PosInt PosInt) the first PosInt represents the order of the 
;; pitcher in the PitchersInternalRep. So if PosInt = i, that means the pitcher 
;; is the ith pitcher in PitchersExternalRep and so on.
;; PitchersInternalRep is another representation for PitchersExternalRep
;; Example: 
;; (list
;;  (list 1 8 8)
;;  (list 2 5 0)
;;  (list 3 3 0))

;; TEMPLATE:
;; lopi-fn : LOPI -> ??
;; (define (lopi-fn lopi)
;;  (cond
;;    [(empty? lopi) ...]
;;    [else (... (pitcherinternalRep-fn (first lopi))
;;               (lopi-fn (rest lopi))]))

;; A PitchersInternalRepSet is a ListOfPitchersInternalRep(LIPS), it is one of
;; -- empty                           -- it is empty with no PitchersInternalRep
;; -- (cons PitchersInternalRep LIPS) -- First element of the LIPS is
;;                                       PitchersInternalRep and second is LIPS
;; TEMPLATE:
;; lips-fn : LIPS -> ??
;; (define (lips-fn lips)
;;  (cond
;;    [(empty? lips) ...]
;;    [else (... (lopi-fn(first lips))
;;               (lips-fn (rest lips))]))

;; PitcherExternalRep is a (list PosInt PosInt)
;; it represents the capacity and current contents of the Pitcher
;; TEMPLATE:
;; pitcherexternalRep-fn : PitcherExternalRep -> ??
;; (define (pitcherexternalRep-fn er)
;;  (... (first er)
;;       (second er)))

;-------------------------------------------------------------------------------

;; PitchersExternalRep ::= ((capacity1 contents1)
;;                          (capacity2 contents2)
;;                          ...
;;                          (capacity_n contents_n))
;; WHERE:  n >=1, and for each i, 0 <= contents_i <= capacity_i
;; INTERPRETATION: the list of pitchers, from 1 to n, with their contents
;; and capacity
;; EXAMPLE: ((10 5) (8 7)) is a list of two pitchers.  The first has
;; capacity 10 and currently holds 5; the second has capacity 8 and
;; currently holds 7.
;; EXAMPLES: '((10 8) (5 3))

;; A PitchersExternalRep or Listof<Pitchers>(LOPE) is one of 
;; -- empty                          -- it is empty with no pitchers
;; -- (cons PitcherExternalRep LOPE)  -- the first element is 
;;                                      PitcherExternalRep and second is LOPE
;; TEMPLATE:
;; lope-fn : LOPE -> ??
;; (define (lope-fn lope)
;;   (cond
;;    [(empty? lope)...]
;;    [else (...(pitcherexternalRep-fn (first lope))
;;              (lope-fn (rest lope)))]))

;-------------------------------------------------------------------------------

;; NEListOf<PosInt> (NELOPI) is one of
;; (cons PosInt empty)  --first element is PosInt and second is empty
;; (cons PosInt NELOPI)  --first element is PosInt and second is NELOPI
;; Interp:
;; It is a list that contains a positive integer or a list of positve integer
;; TEMPLATE:
;; nelopi-fn : NELOPI -> ??
;; (define (nelopi-fn nelopi)
;;   (cond 
;;     [(empty? (rest loi)).....]
;;     [else (.....(first nelopi)
;;            ... (nelopi-fn (rest nelopi)))]))

;-------------------------------------------------------------------------------

(define-struct move (src tgt))
;; A Move is a (make-move PosInt PosInt)
;; WHERE: src and tgt are different
;; INTERP: (make-move i j) means pour from pitcher i to pitcher j.
;; 'pitcher i' refers to the i-th pitcher in the PitchersExternalRep.
;; TEMPLATE:
;; move-fn : Move -> ??
;;(define (move-fn m)
;;  (... (move-src m)
;;       (move-tgt m)))

;; A ListOf<Move> or LOM is one of
;; -- empty                     -- No move is possible  
;; -- (cons Move ListOf<Move>)  -- This contains first element as move and next
;;                                 as ListOf<Move>
;; TEMPLATE
;; lom-fn : ListOf<Move> -> ??
;;(define (lom-fn lom)
;;  (cond
;;    [(empty? lom) ...]
;;    [else (..(move-fn (first lom))
;;              (lom-fn (rest lom))]))

;;A Maybe<ListOf<Move>> is either
;; - false        --no ListOf<Move> to fill pitcher with target amount
;;                  of liquid
;; - ListOf<Move> --ListOf<Move> to fill a pitcher with target amount of
;;                  liquid
;; TEMPLATE:
;; mbm-fn : Maybe<ListOf<Move> -> ??
;;(define (mbm-fn mbm)
;;  (cond
;;    [(false? mbm) ...]
;;    [else (lom-fn mbm)]))

;-------------------------------------------------------------------------------

(define-struct status (path lop))
;; A Status is a (make-status ListOf<Move> PitchersInternalRep)
;; INTERP: A status includes a path which is list of moves 
;;         between the original pitchers and the current pitchers;
;;         and list of pitchers(lop) is PitchersInternalRep
;; TEMPLATE:
;; status-fn : Status -> ??
;; (define (status-fn s)
;;   (.....(status-path s)
;;         (status-lop s)))

;; A ListOf<Status> or LOS is one of
;; -- empty                         -- no status
;; -- (cons Status ListOf<Status>)  -- This contains first element as status and
;;                                     next as ListOf<Status>
;; TEMPLATE
;; los-fn : ListOf<Status> -> ??
;;(define (los-fn los)
;;  (cond
;;    [(empty? los) ...]
;;    [else (..(status-fn (first los))
;;              (los-fn (rest los))]))

;; EXAMPLES FOR TEST

(define lope '((10 8) (5 3)))
(define lopi (list (list 1 8 8) (list 2 5 0) (list 3 3 0)))
(define lom (list (make-move 1 2) (make-move 2 3) (make-move 3 1)))
(define solution-res 
  (list
   (make-move 1 2)
   (make-move 2 3)
   (make-move 3 1)
   (make-move 2 3)
   (make-move 1 2)
   (make-move 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        LIST-TO-PITCHERS FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-to-pitchers : PitchersExternalRep -> PitchersInternalRep
;; GIVEN: the PitchersExternalRep
;; RETURNS: your internal representation of the given input.
;; EXAMPLES:(list-to-pitchers (list-to-pitchers '((10 8) (5 3))))
;;           = (list (list 1 10 8) (list 2 5 3))
;; TESTS: See test cases at the end.
;; STRATEGY: Function Composition.

(define (list-to-pitchers pe)
  (local
    ; PitchersExternalRep PosInt -> PitchersInternalRep
    ; GIVEN: the PitchersExternalRep and a positive integer
    ; RETURNS: your internal representation of the given input.
    ; WHERE: n is the positive integer which keeps increasing 
    ; by one at each recursive call starting from one
    ; STRATEGY: Structural Decomposition on lst : PitchersExternalRep
    ((define (helper lst n)
       (cond
         [(empty? lst) empty]
         [else (cons (list n (first (first lst)) (second (first lst)))
                     (helper (rest lst) (add1 n)))])))
    (helper pe ONE)))

;; TESTS:
(begin-for-test
  (check-equal? (list-to-pitchers lope)
                (list (list 1 10 8) (list 2 5 3))
                "Error in test case for list-to-pitchers"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        PITCHERS-TO-LIST FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pitchers-to-list : PitchersInternalRep -> PitchersExternalRep
;; GIVEN: an internal representation of a set of pitchers
;; RETURNS: a PitchersExternalRep that represents them.
;; EXAMPLES: (pitchers-to-list (list (list 1 8 8) (list 2 5 0) (list 3 3 0)))
;;            = (list (list 8 8) (list 5 0) (list 3 0))
;; TESTS: See test cases at the end.
;; STRATEGY: High Order Function Composition.

(define (pitchers-to-list pi)
  (map 
   ; PitcherInternalRep->PitcherExternalRep
   ; GIVEN: an internal representation of a pitcher
   ; RETURNS: an external representation of a pitcher
   (lambda (p) (list (second p) (third p)))
   pi))

;; TESTS:
(begin-for-test
  (check-equal? (pitchers-to-list lopi)
                (list (list 8 8) (list 5 0) (list 3 0))
                "Error in test case for pitchers-to-list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   PITCHERS-AFTER-MOVES FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pitchers-after-moves : PitchersInternalRep ListOf<Move> 
;;                        -> PitchersInternalRep
;; GIVEN: An internal representation of a set of pitchers, and a sequence
;; of moves
;; WHERE: every move refers only to pitchers that are in the set of pitchers.
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given list of moves, in order, on the given
;; set of pitchers.
;; EXAMPLES: (pitchers-after-moves lopi lom) 
;;            = (list (list 1 8 6) (list 2 5 2) (list 3 3 0))
;; TESTS: See test cases at the end.
;; STRATEGY: Genernal Recursion
;; HATLING MEASRUE: the length of lom
;; TERMINATION ARGUMENT: the length of lom decreases every time recursion calls

(define (pitchers-after-moves lop lom)
  (cond
    [(empty? lom) lop]
    [else (pitchers-after-moves (pitchers-after-move lop (first lom))
                                (rest lom))]))

;; pitchers-after-move: PitchersInternalRep Move -> PitchersInternalRep
;; GIVEN: An internal representation of a set of pitchers, and a move
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given move, on the given set of pitchers
;; EXAMPLES: (pitchers-after-move lopi (make-move 1 2)) 
;;            = (list (list 1 8 3) (list 2 5 5) (list 3 3 0))
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on m : Move

(define (pitchers-after-move lop m)
  (pitchers-after-m lop (move-src m) (move-tgt m)))  

;; pitchers-after-m: PitchersInternalRep PosInt PosInt -> PitchersInternalRep
;; GIVEN: An internal representation of a set of pitchers, two positive integers
;; which are the numbered pitchers as source and target.
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given list of moves, in order, on the given
;; set of pitchers
;; EXAMPLES: (pitchers-after-m lopi 1 2) 
;;            = (list (list 1 8 3) (list 2 5 5) (list 3 3 0))
;; TESTS: See test cases at the end.
;; STRATEGY: High Order Function Composition

(define (pitchers-after-m lop i j)
  (if (equal? i j)
      lop
      (local
        ((define s (list-ref lop (sub1 i)))
         (define t (list-ref lop (sub1 j))))
        (foldr
         ; PitcherInternalRep PitchersInternalRep -> PitchersInternalRep
         ; GIVEN: a pitcher and a list of pitchers
         ; RETURNS: A list of pitchers after the move from source 
         ; pitcher to target pitcher
         (lambda (f rest)
           (cons 
            (cond
              [(equal? f s) (s-after s t)]
              [(equal? f t) (t-after s t)]
              [else f])
            rest))
         empty
         lop))))


;; s-after: PitcherInternalRep PitcherInternalRep -> PitcherInternalRep
;; GIVEN: a source pithcer and a target pitcher
;; RETURNS: a source pitcher after the move that is with its content changed
;; EXAMPLES: (s-after (list 1 8 8) (list 2 5 0)) = (list 1 8 3)
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on s and t : PitcherInternalRep

(define (s-after s t)
  (list (first s) (second s)
        (if (> (+ (third s) (third t)) (second t))
            (- (+ (third s) (third t)) (second t))
            ZERO)))

;; t-after: PitcherInternalRep PitcherInternalRep -> PitcherInternalRep
;; GIVEN: a source pithcer and a target pitcher
;; RETURNS: a target pitcher after the move that is with its content changed
;; EXAMPLES: (t-after (list 1 8 8) (list 2 5 0)) = (list 2 5 5)
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on s and t : PitcherInternalRep

(define (t-after s t) 
  (list (first t) (second t) 
        (if (> (+ (third s) (third t)) (second t))
            (second t)
            (+ (third s) (third t)))))

;; TESTS
(begin-for-test
  (check-equal? (pitchers-after-moves lopi lom) 
                (list (list 1 8 6) (list 2 5 2) (list 3 3 0))
                "Error in test case of pitchers-after-moves"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          SOLUTION FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; solution : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;; GIVEN: a list of the capacities of the pitchers and the goal amount
;; RETURNS: a sequence of moves which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists.
;; EXAMPLES: (solution (list 8 5 3) 4) = solution-res
;; TESTS: See test cases at the end.
;; STRATEGY: Function Composition

(define (solution loi goal)
  (sol (loi->lop loi ONE) goal))

;; sol: PitchersInternalRep Integer -> Maybe<Move>
;; GIVEN: a list of pitchers and the goal amount
;; RETURNS: a move which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists. 
;; EXAMPLES: (sol lopi 4)
;;            = (list(make-move 1 2)
;;                   (make-move 2 3)
;;                   (make-move 3 1)
;;                   (make-move 2 3)
;;                   (make-move 1 2)
;;                   (make-move 2 3))
;; TESTS: See test cases at the end.
;; STRATEGY: Function Composition

(define (sol lop goal)
  (dfs (list (make-status empty lop)) (list lop) goal))

;; dfs: ListOf<Status> PitchersInternalRep PosInt -> Maybe<Move>
;; GIVEN: a ListOf<Status>, a PitchersInternalRep and and the goal amount
;; RETURNS: a move which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists.   
;; EXAMPLE: See test cases at the end.
;; STRATEGY: General Recursion
;; HALTING MEASURE: The number of unvisited PitchersInternalRep
;; TERMINATION ARGUMENT: At each recursion, we check one PitchersInternalRep,
;;                       and set it as visited. So as the number of recursion
;;                       increases, the number of unvisited PitchersInternalRep
;;                       decreases.

(define (dfs newest visited goal)
  (cond
    [(empty? newest) false]
    [(contain? goal newest) (status-path (first (containing goal newest)))]
    [else 
     (local
       ((define candidates (set-diff (next (first newest) 
                                           (length (status-lop (first newest))))
                                     visited)))
       (cond
         [(empty? candidates) (dfs (rest newest) visited goal)]
         [else
          (dfs (append  (rest newest) candidates )
               (append (pitchers-separation candidates) visited)
               goal)]))]))

;; TEST
(begin-for-test
(check-equal? (dfs empty empty 4) false
              "Error in test case  of dfs"))

;-------------------------------------------------------------------------------

;; loi->lop: NEListOf<PosInt> PosInt -> PitchersInternalRep
;; GIVEN: a list of the capacities of the pitchers and the goal amount
;; RETURNS: a PitchersInternalRep
;; WHERE: n is the pitcher number, and n >= 1 is always true
;; EXAMPLES:
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on loi : NEListOf<PosInt>

(define (loi->lop loi n)
  (cond
    [(empty? (rest loi)) 
     (list (list n (first loi) (if (equal? ONE n) (first loi) ZERO)))]
    [else 
     (cons (list n (first loi) (if (equal? ONE n) (first loi) ZERO))
                (loi->lop (rest loi) (add1 n)))]))

;; TEST
(check-equal? (loi->lop '(1) ONE) '((1 1 1)))

;; contain? : PosInt ListOf<Status> -> Boolean
;; GIVEN: a goal amount and a ListOf<Status>
;; RETURNS: true iff the goal amount can be moved to the 
;; pitchers in ListOf<Status> 
;; EXAMPLES:
;; (contain? 4 (list (make-status (list (make-move 1 2)) 
;;                                (list (list 1 8 8)))))
;;  = false
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on f : Status

(define (contain? g los)
  (ormap 
   ; Status -> Boolean
   ; GIVEN: a status
   ; RETURNS: true iff the goal amount can be moved to the 
   ; pitchers in a status
   (lambda (f) (contain?-helper g (status-lop f))) 
   los))

;; contain?-helper : PosInt PitchersInternalRep -> Boolean
;; GIVEN: a goal amount and a PitchersInternalRep
;; RETURNS: true iff the goal amount can be moved to the 
;; pitchers in PitchersInternalRep
;; EXAMPLES:
;; (contain?-helper 4 (list (list 1 8 8)))
;; = false
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on f : PitcherInternalRep

(define (contain?-helper g lop)
  (ormap 
   ; PitcherInternalRep -> Boolean
   ; GIVEN: a PitcherInternalRep
   ; RETURNS: true iff the goal amount can be moved to the 
   ; pitcher
   (lambda (f) (equal? g (third f))) 
   lop))

;; containing : Integer ListOf<Status> -> ListOf<Status>
;; GIVEN: a goal amount and a ListOf<Status>
;; RETURNS: a list of status with pitchers where the goal amount can be moved
;; EXAMPLES:
;; (containing 4 (list (make-status (list (make-move 1 2)) 
;;                                  (list (list 1 8 4)))))
;; = (list (make-status (list (make-move 1 2))
;;                      (list (list 1 8 4))))
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on f : Status

(define (containing g los)
  (filter 
   ; Status -> Boolean
   ; GIVEN: a status
   ; RETURNS: true iff the goal amount can be moved to the 
   ; pitchers in a status
   (lambda (f) (contain?-helper g (status-lop f))) 
   los))

;; pitchers-separation : ListOf<Status> -> PitchersInternalRepSet
;; GIVEN: a list of status
;; RETURNS: a list of PitchersInernalRep
;; EXAMPLES:
;; (pitchers-separation (list (make-status (list (make-move 1 2)) 
;;                                         (list (list 1 8 4)))))
;; = (list (list (list 1 8 4)))
;; TESTS: See test cases at the end.
;; STRATEGY: High Order Function Composition

(define (pitchers-separation candidates)
  (map status-lop candidates))

;; set-diff : ListOf<Status> PitcherInternalRepSet -> ListOf<Status> 
;; GIVEN: a list of status, a list of PitcherInternalRep
;; RETURNS: a list of status filtered by the PitcherInternalRepSet
;; EXAMPLES:
;; (set-diff (list (make-status (list (make-move 1 2)) 
;;                              (list (list 1 8 4))))
;;           (list (list (list 1 8 4))))
;; = empty
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on x : Status

(define (set-diff lopp set)
  (filter 
   ; Status -> ListOf<Status>
   ; GIVEN: a status 
   ; RETURNS: a list of status filtered by the PitcherInternalRepSet 
   (lambda (x) (not (my-member? (status-lop x) set))) 
   lopp))

;; my-member? : PitcherInternalRep PitcherInternalRep -> Boolean
;; GIVEN: a list of pitchers and a set of 
;; list of pitchers (without dulplicates)
;; RETURNS: true iff the set is containing that list of pitchers
;; EXAMPLES:
;; (my-member?  (list (list 1 8 4)) (list (list (list 1 8 4))))
;; = true
;; TESTS: See test cases at the end.
;; STRATEGY: High Order Function Composition

(define (my-member? lop set)
  (ormap 
   ; PitcherInternalRep -> Boolean
   ; GIVEN: a PitcherInternalRep
   ; RETURNS: true iff the set is containing the given pitcher
   (lambda (f) (equal? lop f)) set))

;-------------------------------------------------------------------------------

;; next : Status Integer -> ListOf<Status>
;; GIVEN: the current status and the length of the list of pitchers
;; RETURNS: a list of status following the given status
;; EXAMPLES:
;; (next (make-status (list (make-move 1 2)) 
;;                    (list (list 1 8 4))) 1)
;; = (list
;;     (make-status (list (make-move 1 2)
;;                        (make-move 1 1))
;;                  (list (list 1 8 4))))
;; TESTS: See test cases at the end.
;; STRATEGY: Function Composition

(define (next s n)
  (local
    ((define lst (build-list n add1)))
    (move/acc s lst lst)))

;; move/acc : Status ListOf<Int> ListOf<Int> -> ListOf<Status>
;; GIVEN: the current status, an non-empty list of integer lst1,
;;        an non-empty list of integer lst2
;; WHERE: lst1 and lst2 helpe to produce the new status
;; RETURNS: a list of status following the given status after moves
;; EXAMPLES:
;; (move/acc (make-status (list (make-move 1 2)) 
;;                        (list (list 1 8 4))) (list 1) (list 1))
;;  = (list (make-status (list
;;                        (make-move 1 2)
;;                        (make-move 1 1))
;;                       (list (list 1 8 4))))
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on s : Status

(define (move/acc s l1 l2)
  (foldr
   append
   empty
   (map
    ; PosInt -> ListOf<Status>
    ; GIVEN: a positive integer
    ; RETURNS: a ListOf<Status>
    (lambda (n)
      (map
       ; PosInt -> Status
       ; GIVEN: a positive integer
       ; RETURNS: a status
       (lambda (m) 
         (make-status (append (status-path s) (list (make-move n m)))
                      (pitchers-after-move (status-lop s) (make-move n m))))
       l2))
    l1)))

;; TESTS:
(begin-for-test
  (check-equal? (solution (list 8 5 3) 4)
                solution-res
                "Error in test case of solution function"))