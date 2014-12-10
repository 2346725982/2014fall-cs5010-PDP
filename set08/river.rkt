;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname river) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;; river.rkt
;; Set08-Question 3
;; Zoltan Wang, Pankaj Tripathi

;; ALGORITHM:
;; In this program, we use BFS to solve the problem.
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
(provide make-fill)
(provide move?)
(provide fill-pitcher)
(provide fill?)
(provide make-dump)
(provide dump-pitcher)
(provide dump?)
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

;; general-move
(define-struct move (src tgt))
;; A general move is a (make-shift PosInt PosInt)
;; WHERE: src and tgt are different
;; INTERP: (make-shift i j) means pour from pitcher i to pitcher j.
;; 'pitcher i' refers to the i-th pitcher in the PitchersExternalRep.
;; TEMPLATE:
;; gen-move-fn : Gen-Move -> ??
;;(define (gen-move-fn m)
;;  (... (move-src m)
;;       (move-tgt m)))

(define-struct fill (pitcher))
;; A Fill is a (make-fill PosInt)
;; WHERE: i >= 0
;; INTERP: (make-fill i) means pour water to the pitcher i until it's full
;; TEMPLATE:
;; fill-fn : Fill -> ??
;;(define (fill-fn f)
;;  (... (fill-pitcher f))

(define-struct dump (pitcher))
;; A Dump is a (make-dump PosInt)
;; WHERE: i >= 0
;; INTERP: (make-dump i) means pour water out of the pitcher i until it's empty
;; TEMPLATE:
;; dump-fn : Dump -> ??
;;(define (dump-fn f)
;;  (... (dump-pitcher f))

;; A Move is one of
;; -- (make-move i j)   --pour the contents of pitcher i into pitcher j
;; -- (make-fill i)     --fill pitcher i from the river
;; -- (make-dump i)     --dump the contents of pitcher i into the river.

;; A Move is a (make-move PosInt PosInt)
;; WHERE: src and tgt are different
;; INTERP: (make-move i j) means pour from pitcher i to pitcher j.
;; 'pitcher i' refers to the i-th pitcher in the PitchersExternalRep.
;; TEMPLATE:
;; move-fn : Move -> ??
;; (define (move-fn m)
;;   (cond
;;      [(move? m)(...(move-src m)(move-tgt m))]
;;      [(fill? m)(...(fill-pitcher m))]
;;      [(dump? m)(...(dump-pitcher m)]))

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
;;         betweem the the pitchers and list of pitchers that is
;;         PitchersInternalRep
;; TEMPLATE:
;; status-fn : Status -> ??
;; (define (status-fn s)
;;   (.....(status-path s)
;;       (status-lop s)))

;; A ListOf<Status> or LOS is one of
;; -- empty                         --
;; -- (cons Status ListOf<Status>)  -- This contains first element as status and
;;                                     next as ListOf<Status>
;; TEMPLATE
;; los-fn : ListOf<Status> -> ??
;;(define (los-fn los)
;;  (cond
;;    [(empty? los) ...]
;;    [else (..(status-fn (first los))
;;              (los-fn (rest los))]))

;; EXAMPLES FOR TEST:

(define lope '((10 8) (5 3)))
(define lopi (list (list 1 8 8) (list 2 5 0) (list 3 3 0)))
(define lom (list (make-move 1 2) (make-move 2 3) (make-move 3 1)))
(define solution-res
  (list
   (make-move 1 2)
   (make-move 1 3)
   (make-move 2 4)
   (make-move 3 2)
   (make-move 4 1)
   (make-move 1 3)
   (make-move 1 5)
   (make-move 2 1)
   (make-move 3 1)
   (make-move 1 2)
   (make-move 1 4)
   (make-move 2 1)
   (make-move 1 3)
   (make-move 1 5)))

(define sol-res
  (list
   (make-move 1 2)
   (make-move 1 3)
   (make-dump 2)
   (make-move 3 2)
   (make-fill 1)
   (make-move 1 2)
   (make-move 2 3)
   (make-move 3 1)
   (make-move 1 2)
   (make-dump 1)
   (make-move 3 1)
   (make-move 2 3)
   (make-move 3 1)))

(define next-res 
  (list
   (make-status 
    (list 
     (make-move 1 2)
     (make-move 1 1))
    (list (list 1 8 4)))
   (make-status
    (list
     (make-move 1 2)
     (make-fill 1))
    (list (list 1 8 8)))
   (make-status
    (list
     (make-move 1 2)
     (make-dump 1))
    (list (list 1 8 0)))))

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
  (check-equal? (list-to-pitchers (list (list 10 8) (list 5 3)))
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
  (check-equal? (pitchers-to-list '((1 8 8) (2 5 0) (3 3 0)))
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
;;             =  (list (list 1 8 6)
;;                      (list 2 5 2)
;;                      (list 3 3 0)) 
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
;; result after executing the given move, in order, on the given
;; set of pitchers 
;; EXAMPLE: (pitchers-after-move lopi (make-move 1 2))
;;           = (list (list 1 8 3)
;;                   (list 2 5 5)
;;                   (list 3 3 0))
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on m : Move

(define (pitchers-after-move lop m)
  (cond
    [(move? m) (pitchers-after-m lop (move-src m) (move-tgt m))]
    [(fill? m) (pitchers-after-f/d lop (fill-pitcher m) FILL)]
    [(dump? m) (pitchers-after-f/d lop (dump-pitcher m) DUMP)]))

;; pitchers-after-f/d: PitchersInternalRep Integer String -> PitchersInternalRep
;; GIVEN: a PitchersInternalRep, PitcherInternalRep and a move which is either 
;; a fill or a dump
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given move, in order, on the given
;; set of pitchers  
;; WHERE: ins would only be "fill" or "dump"
;; EXAMPLE: (pitchers-after-f/d lopi 2 DUMP)
;;           = (list (list 1 8 8)
;;                   (list 2 5 0)
;;                   (list 3 3 0))            
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on p : PitchersInternalRep

(define (pitchers-after-f/d lop i ins)
  (local 
    ; PitchersInternalRep -> PitchersInternalRep 
    ; GIVEN: a PitchersInternalRep
    ; RETURNS: a PitchersInternalRep depending on the corresponding move that
    ; a fill or a dump
    ((define (pitcher-f/d p)
       (if (equal? (first p) i)
           (list (first p) 
                 (second p) 
                 (if (equal? ins FILL) (second p) ZERO))
           p)))
    (map pitcher-f/d lop)))

;; pitchers-after-m: PitchersInternalRep Integer Integer -> PitchersInternalRep
;; GIVEN: An internal representation of a set of pitchers, two positive integers
;; which are the numbered pitchers as source and target.
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given move, in order, on the given
;; set of pitchers
;; EXAMPLE: (pitchers-after-m lopi 1 2)
;;           = (list (list 1 8 3)
;;                   (list 2 5 5)
;;                   (list 3 3 0))
;; TESTS: See test cases at the end.
;; STRATEGY: High Order Function Composition

(define (pitchers-after-m lop i j)
  (local
    ((define s (list-ref lop (sub1 i)))
     (define t (list-ref lop (sub1 j)))
     (define s-after (if (equal? i j) s (src-pitcher-after-move s t)))
     (define t-after (if (equal? i j) t (tgt-pitcher-after-move s t)))
     ; PitcherInternalRep -> PitcherInternalRep
     ; GIVEN: a PitcherInternalRep
     ; RETURNS: PitcherInternalRep after the move from src to tgt
     (define (pitcher-move p)
       (cond [(equal? p s) s-after]
             [(equal? p t) t-after]
             [else p])))
    (map pitcher-move lop)))

;; src-pitcher-after-move: PitcherInternalRep PitcherInternalRep
;;                         -> PitcherInternalRep
;; GIVEN: a source pithcer and a target pitcher
;; RETURNS: a source pitcher after the move that is with its content changed
;; EXAMPLES: (src-pitcher-after-move (list 1 8 8) (list 2 5 0)) = (list 1 8 3)
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on s & t : PitcherInternalRep

(define (src-pitcher-after-move s t)
  (if (> (+ (third s) (third t)) (second t))
      (list (first s) (second s) (- (+ (third s) (third t)) (second t)))
      (list (first s) (second s) ZERO)))

;; tgt-pitcher-after-move: PitcherInternalRep PitcherInternalRep 
;;                         -> PitcherInternalRep
;; GIVEN: a source pithcer and a target pitcher
;; RETURNS: a target pitcher after the move that is with its content changed
;; EXAMPLES: (tgt-pitcher-after-move (list 1 8 8) (list 2 5 0)) = (list 2 5 5)
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on s & t : PitcherInternalRep

(define (tgt-pitcher-after-move s t)
  (if (> (+ (third s) (third t)) (second t))
      (list (first t) (second t) (second t))
      (list (first t) (second t) (+ (third s) (third t)))))

;; TESTS

(define position-after-moves-res
  (list
   (list 1 8 4)
   (list 2 5 2)
   (list 3 3 0)))

(begin-for-test
  (check-equal? (pitchers-after-moves (loi->lop (list 8 5 3) ONE) 
                                      (solution (list 8 5 3) 4))
                position-after-moves-res
                "Error in test case of pitchers-after-moves"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          SOLUTION FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; solution : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;; GIVEN: a list of the capacities of the pitchers and the goal amount
;; RETURNS: a sequence of moves which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists.
;; EXAMPLES: (solution (list 8 5 3 16 32 65 128) 4) = solution-res
;; TESTS: See test cases at the end.
;; STRATEGY: Function Composition

(define (solution loi goal)
  (sol (loi->lop loi ONE) goal))

;; sol: PitchersInternalRep Integer -> Maybe<Move>
;; GIVEN: a list of pitchers and the goal amount
;; RETURNS: a move which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists. 
;; EXAMPLE: (sol lopi 4) = sol-res 
;; TESTS: See test cases at the end.
;; STRATEGY: Function Composition

(define (sol lop goal)
  (bfs (list (make-status empty lop)) (list lop) goal))

;; bfs: ListOf<Status> PitchersInternalRep PosInt -> Maybe<Move>
;; GIVEN: a ListOf<Status>, a PitchersInternalRep and and the goal amount
;; RETURNS: a move which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists.  
;; EXAMPLE: (bfs empty empty 4) = false
;; STRATEGY: General Recursion
;; HALTING MEASURE: The number of unvisited PitchersInternalRep
;; TERMINATION ARGUMENT: At each recursion, we check one PitchersInternalRep,
;;                       and set it as visited. So as the number of recursion
;;                       increases, the number of unvisited PitchersInternalRep
;;                       decreases.

(define (bfs newest visited goal)
  (cond
    [(empty? newest) false]
    [(contain? goal newest) (status-path (first (containing goal newest)))]
    [else 
     (local
       ((define candidates (set-diff (next (first newest)
                                           (length (status-lop (first newest)))) 
                                     visited)))
       (cond
         [(empty? candidates) (bfs (rest newest) visited goal)]
         [else (bfs (append  candidates (rest newest))
                    (append (pitchers-separation candidates) visited)
                    goal)]))]))
;; TESTS
(begin-for-test
  (check-equal? (bfs empty empty 4)
                false
                "Error in test case of bfs"))

;-------------------------------------------------------------------------------

;; loi->lop: NEListOf<PosInt> PosInt -> PitchersInternalRep
;; GIVEN: a list of the capacities of the pitchers and the goal amount
;; RETURNS: a PitchersInternalRep
;; WHERE: n is the pitcher number, and n >= 1 is always true
;; EXAMPLES: (loi->lop (list 1 2 3) 4)
;;            = (list (list 4 1 0)
;;                    (list 5 2 0)
;;                    (list 6 3 0))
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on loi : NEListOf<PosInt>

(define (loi->lop loi n)
  (cond
    [(empty? loi) empty]
    [else (cons (list n (first loi) (if (equal? ONE n) (first loi) ZERO))
                (loi->lop (rest loi) (add1 n)))]))

;; contain?: PosInt ListOf<Status> -> Boolean
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

;; contain?-helper: PosInt PitchersInternalRep -> Boolean
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

;; containing: PosInt ListOf<Status> -> ListOf<Status>
;; GIVEN: a goal amount and a ListOf<Status>
;; RETURNS: a list of status with pitchers where the goal amount can be moved
;; EXAMPLES:
;; (containing 4 (list (make-status (list (make-move 1 2)) 
;;                                  (list (list 1 8 4)))))
;; = (list (make-status (list (make-move 1 2))
;;                      (list (list 1 8 4))))
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on f : Status

(define (containing g lopp)
  (filter 
   ; Status -> Boolean
   ; GIVEN: a status
   ; RETURNS: true iff the goal amount can be moved to the 
   ; pitchers in a status
   (lambda (f) (contain?-helper g (status-lop f))) 
   lopp))

;; pitchers-separation: ListOf<Status> -> PitchersInternalRepSet
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

;; set-diff: ListOf<Status> PitcherInternalRepSet -> ListOf<Status> 
;; GIVEN: a list of status, a list of PitcherInternalRep
;; RETURNS: a list of status filtered by the PitcherInternalRepSet
;; EXAMPLES:
;; (set-diff (list (make-status (list (make-move 1 2)) 
;;                              (list (list 1 8 4))))
;;           (list (list (list 1 8 4))))
;; = empty
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on x : Status
(define (set-diff los set)
  (filter
   ; Status -> ListOf<Status>
   ; GIVEN: a status 
   ; RETURNS: a list of status filtered by the PitcherInternalRepS
   (lambda (x) (not (my-member? (status-lop x) set))) 
   los))

;; my-member?: PitcherInternalRep PitcherInternalRep -> Boolean
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
;; next: Status Integer -> ListOf<Status>
;; GIVEN: the current status and the length of the list of pitchers
;; RETURNS: a list of status following the given status
;; EXAMPLES:
;; (next (make-status (list (make-move 1 2)) 
;;                    (list (list 1 8 4))) 1)
;; = next-res
;; TESTS: See test cases at the end.
;; STRATEGY: Function Composition

(define (next s n)
  (local
    ((define lst (build-list n add1)))
    (append (move/acc s lst lst)
            (fd/acc s lst FILL)
            (fd/acc s lst DUMP))))

;; move/acc: Status ListOf<Int> ListOf<Int> -> ListOf<Status>
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

;; fd/acc : Status ListOf<Int> String -> ListOf<Status>
;; GIVEN: the current status, an non-empty list of integer, 
;;        which helpes to produce the new status, and an instruction
;; WHERE: the ins will only be "fill" or "dump"
;; RETURNS: a list of status following the given status after fill or dump
;; EXAMPLE: (fd/acc (make-status empty (list(list 1 8 8))) (list 1) FILL)
;;           = (list  (make-status
;;                    (list (make-fill 1))
;;                    (list (list 1 8 8))))    
;; TESTS: See test cases at the end.
;; STRATEGY: Structural Decomposition on s : Status

(define (fd/acc s lst ins)
  (local
    ((define make-f/d (if (equal? ins DUMP) make-dump make-fill)))
    (map
     ; PosInt -> Status
     ; GIVEN: a positive integer
     ; RETURNS: a status
     (lambda (f)
       (make-status (append (status-path s) (list (make-f/d f)))
                    (pitchers-after-f/d (status-lop s) f ins)))
     lst)))

;; TESTS
(begin-for-test
  (check-equal? (solution (list 8 5 3 16 32 65 128) 4)
                solution-res
                "Error in test case of solution"))

(pitchers-after-moves '((1 7 7) (2 3 0)) (solution '(7 3) 2))
(solution '(7 3) 2)

(pitchers-after-moves '((1 7 7) (2 3 0)) (list (make-move 1 2)
(make-dump 1)
(make-move 2 1)
(make-fill 2)
(make-move 2 1)
(make-fill 2)
(make-move 2 1)))


(pitchers-after-moves '((1 10 10) (2 3 3) (3 5 0)) (solution '(10 3 5) 1))
(solution '(10 3 5) 1)

(pitchers-after-moves '((1 2 2) (2 5 0) (3 10 0)) (solution '(2 5 10) 6))
(solution '(2 5 10) 6)

(pitchers-after-moves '((1 3 3) (2 5 0)) (solution '(3 5) 4))
(solution '(3 5) 4)

(pitchers-after-moves '((1 11 11) (2 7 0)) (solution '(11 7) 6))
(solution '(11 7) 6)

(pitchers-after-moves '((1 8 8) (2 8 0) (3 40 0) (4 80 0) (5 120 0) (6 5 0) (7 5 0)) 
                      (solution '(8 8 40 80 120 5 5) 4))
(solution '(8 8 40 80 120 5 5) 4)