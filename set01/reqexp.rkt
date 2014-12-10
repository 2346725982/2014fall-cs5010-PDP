;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname reqexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)

(provide
 initial-state
 next-state
 accepting-state?
 error-state?)

(define AB "start, expect to see an 'a', 'b', 'c' or 'd' next")
(define CD "expect to see: 'c', 'd', or 'e'")
(define EE "encountered a 'e', finished")
(define ER "error, user pressed illegal key")

;Data Definition
;a State is a string, it's one of
;-- AB    interp "start, expect to see an 'a', 'b', 'c' or 'd' next"
;-- CD    interp "expect to see: 'c', 'd', or 'e'"
;-- EE    interp "encountered a 'e', finished"
;-- ER    interp "error, user pressed illegal key"

;Template
;state-fn: State -> ??
;(define (state-fn s)
;  (cond
;    [(sting=? state AB) ...]
;    [(sting=? state CD) ...]
;    [(sting=? state EE) ...]
;    [(sting=? state ER) ...]))

;initial-state : Number -> State
;GIVEN: a number
;RETURNS: a representation of the initial state
;of your machine.  The given number is ignored.
;Design Strategy: Function Composition
;Function Definition
(define (initial-state num)
  AB)

;bad-letter: KeyEvent -> Boolean
;GIVEN: a keyevent
;RETURNS: false if the letter of keyevent is not 'a', ' b', 'c', 'd' and 'e', true otherwise
;Examples:
;...
;Design Strategy: Cases
;Function Definition
(define (bad-letter ke)
  (if (or (string=? ke "a") (string=? ke "b") (string=? ke "c") 
          (string=? ke "d") (string=? ke "e"))
      false true))

;out-of-sequence: state KeyEvent -> boolean
;GIVEN: a machine state and a keyevent
;RETURENS: true if the input of keyevent would make the machine go into an error state, false otherwise
;Example:
;...
;Design Strategy: Cases
;Function Define
(define (out-of-sequence state ke)
  (cond
    [(and (string=? state AB) 
          (or (string=? ke "a") (string=? ke "b") (string=? ke "c") (string=? ke "d"))) 
     false]
    [(and (string=? state CD) 
          (or (string=? ke "c") (string=? ke "d") (string=? ke "e"))) 
     false]
    [else true]))

;next-state : State KeyEvent -> State
;GIVEN: a state of the machine and a key event.
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;Example:
;...
;Design Strategy: Cases
;Function Definition
(define (next-state state ke)
  (cond
    [(> (string-length ke) 1) state]
    [(or (bad-letter ke) (out-of-sequence state ke)) ER]
    [else
     (cond
       [(and (string=? state AB) 
             (or (string=? ke "a") (string=? ke "b")))
        AB]
       [(and (string=? state AB) 
             (or (string=? ke "c") (string=? ke "d"))) 
        CD]
       [(and (string=? state CD)
             (or (string=? ke "c") (string=? ke "d"))) 
        CD]
       [(and (string=? state CD)
             (string=? ke "e"))
        EE])]))

;accepting-state? : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff the given state is a final (accepting) state
;Design Strategy: Structural Decomposition on state
;Function Define
(define (accepting-state? state)
  (if (string=? state EE) true false))

;error-state? : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff the string seen so far does not match the specified
;regular expression and cannot possibly be extended to do so.
;Design Strategy: Structural Decomposition on state
;Function Define
(define (error-state? state)
  (if (string=? state ER) true false))

(begin-for-test
  (check-equal? (initial-state 80) AB "initial a state")
  
  (check-equal? (next-state AB "a") AB "after get an 'a', state AB stays the same")
  (check-equal? (next-state AB "b") AB "after get a 'b', state AB stays the same")
  (check-equal? (next-state AB "c") CD "after get a 'c', state AB turns into CD")
  (check-equal? (next-state AB "d") CD "after get a 'd', state AB turns into CD")
  (check-equal? (next-state CD "c") CD "after get a 'c', state CD stays the same")
  (check-equal? (next-state CD "d") CD "after get a 'd', state CD stays the same")
  (check-equal? (next-state CD "e") EE "after get a 'e', state CD turns into EE")
  
  (check-equal? (next-state AB "e") ER "after get an input out of sequence, state AB turns into ER")
  (check-equal? (next-state CD "a") ER "after get an input out of sequence, state CD turns into ER")
  
  (check-equal? (next-state AB "v") ER "after get an input of bad letter, state AB turns into ER")
  (check-equal? (next-state CD "v") ER "after get an input of bad letter, state CD turns into ER")
  
  (check-equal? (next-state AB "aaa") AB 
                "after get an input whose length is large than 1, state AB stays the same")
  (check-equal? (next-state CD "vvv") CD
                "after get an input whose length is large than 1, state CD stays the same")
  
  (check-equal? (accepting-state? AB) false "test if a state is a accpeting-state")
  (check-equal? (accepting-state? CD) false "test if a state is a accepting-state")
  (check-equal? (accepting-state? EE) true "test if a state is a accepting-state")
  (check-equal? (accepting-state? ER) false "test if a state is a accepting-state")
  
  (check-equal? (error-state? AB) false "test if a state is a error-state")
  (check-equal? (error-state? CD) false "test if a state is a error-state")
  (check-equal? (error-state? EE) false "test if a state is a error-state")
  (check-equal? (error-state? ER) true "test if a state is a error-state")
  )