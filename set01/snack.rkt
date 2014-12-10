;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(provide
 initial-machine
 machine-next-state
 machine-chocolates
 machine-carrots
 machine-bank)

;Data Definition
;A CustomerInput is one of
;-- a PosInt          interp: insert the specified number of cents
;-- "chocolate"       interp: request a chocolate bar
;-- "carrots"         interp: request a package of carrot sticks
;-- "release"         interp: return all the coins that the customer has put in

;Template
;customer-input-fn: Customer-Input -> ??
;(define (customer-input ci)
;  (if (string? ci)
;     (cond
;       [(string=? ci "chocolate") ...]
;       [(string=? ci "carrots") ...]
;       [(string=? ci "release") ...])
;     (...)))

;Struct Definition
(define-struct machine (chocolates carrots bank))

;Constructor Template
;a Machine is a
;(make-machine NonNegInt NonNegInt NonNegInt)

;Interpretation
; chocolates is an integer indicating the remaining number of 
;   chocolate bars in the machine
; carrots is an interger indicating the remaining number of 
;   carrot sticks in the machine
; bank is an integer of the total money in cents in its bank

;Destructor Template
;machine-fn: Machine -> ??
;(define (machine-fn m)
;  (...
;   (machine-chocolate m)
;   (machine-carrot m)
;   (machine-bank m)))


;initial-machine : NonNegInt NonNegInt -> Machine
;GIVEN: an integer of chocolate bars and an integer of packages of
;carrot sticks
;RETURNS: a machine loaded with the given number of chocolate bars and
;carrot sticks, with an empty bank.
;Examples:
;(initial-machine (10 15)) = (make-machine (10 15 0))
;Design Strategy: Function Composition
;Function Definition
(define (initial-machine chocolates carrots)
  (make-machine chocolates carrots 0))

;enough-money?: Machine Customer-Input -> boolean
;GIVEN: a machine, and a customer-input of erither "chocolates" or "carrots"
;RETURNS: true if the rest money of the bank is enough 
;for one "chocolates" or "carrots", false otherwise
;Examples:
;...
;Design Strategy: Structural Decomposition on customer-input
;Function Definition
(define (enough-money? m ci)
  (cond
    [(string=? ci "chocolates")
     (if (> (machine-bank m) 175) true false)]
    [(string=? ci "carrots")
     (if (> (machine-bank m) 70) true false)]))

;machine-next-state : Machine CustomerInput -> Machine
;GIVEN: a machine state and a customer input
;RETURNS: the state of the machine that should follow the customer's
;input
;Examples:
;(machine-next-state (make-machine 3 1 200) "chocolates") = (make-machine 2 1 25)
;(machine-next-state (make-machine 3 1 120) "carrots") = (make-machine 3 0 45)
;(machine-next-state (make-machine 3 1 50) "release") = (make-machine 3 1 0)
;(machine-next-state (make-machine 3 1 20) 25) = (make-machine 3 1 45)
;Design Strategy: Structural Decompostitioin on Customer-Input
;Function Definition
(define (machine-next-state machine customer-input)
    (if (string? customer-input)
     (cond
       [(string=? customer-input "chocolates")
        (if (enough-money? machine customer-input)
            (make-machine (- (machine-chocolates machine) 1) 
                          (machine-carrots machine) 
                          (- (machine-bank machine) 175))
            machine)]
       [(string=? customer-input "carrots")
        (if (enough-money? machine customer-input)
            (make-machine (machine-chocolates machine)
                          (- (machine-carrots machine) 1)  
                          (- (machine-bank machine) 70))
            machine)]
       [(string=? customer-input "release") (make-machine (machine-chocolates machine)
                                                          (machine-carrots machine)
                                                          0)])
    (make-machine (machine-chocolates machine)
                  (machine-carrots machine) 
                  (+ (machine-bank machine) customer-input))))


;machine-chocolates : Machine ->  NonNegInt
;GIVEN: a machine state
;RETURNS: the number of chocolate bars left in the machine
;Examples:
;(machine-chocolate (make-machine 3 2 0)) = 3
;Design Strategy: Structural Decomposition
;Function Definition
;(define (machine-chocolate machine)
;  (machine-chocolate machine))
;   
;machine-carrots : Machine ->  NonNegInt
;GIVEN: a machine state
;RETURNS: the number of packages of carrot sticks left in the machine
;Examples:
;(machine-chocolate (make-machine 3 2 0)) = 2
;Design Strategy: Structural Decomposition
;Function Definition
;(define (machine-carrots machine)
;  (machine-carrot machine))

;machine-bank : Machine ->  NonNegInt
;GIVEN: a machine state
;RETURNS: the amount of money in the machine's bank, in cents
;Examples:
;(machine-chocolate (make-machine 3 2 75)) = 75
;Design Strategy: Structural Decomposition
;Function Definition
;(define (machine-bank machine)
;  (machine-bank machine))

(begin-for-test
  (check-equal? (initial-machine 10 15) (make-machine 10 15 0))
  
  (check-equal? (machine-next-state (make-machine 3 1 200) "chocolates") (make-machine 2 1 25) 
                "enough money for a chocolates bar")
  (check-equal? (machine-next-state (make-machine 3 1 100) "chocolates") (make-machine 3 1 100) 
                "not enough money for a chocolates bar")
  (check-equal? (machine-next-state (make-machine 3 1 120) "carrots") (make-machine 3 0 50)
                "enough money for a carrots")
  (check-equal? (machine-next-state (make-machine 3 1 30) "carrots") (make-machine 3 1 30)
                "not enough money for a carrots")
  (check-equal? (machine-next-state (make-machine 3 1 50) "release") (make-machine 3 1 0)
                "machine after releasing money")
  (check-equal? (machine-next-state (make-machine 3 1 20) 25) (make-machine 3 1 45)
                "machine after a customer puts in money")
  
  (check-equal? (machine-chocolates (make-machine 3 2 0)) 3
                "numbers of the chocolate bars in the machine")
  (check-equal? (machine-carrots (make-machine 3 2 0)) 2
                "numbers of the carrot sticks in the machine")
  (check-equal? (machine-bank (make-machine 3 2 75)) 75
                "numbers of the money in the bank of the machine"))