#lang racket
; quadratic-root : Number Number Number -> Number Number
; GIVEN: the paramounts of a quadratic equation
; RETURNS: the root of it
; Examples:
; (quadratic-root 1 4 4) => -2
; (quadratic-root 1 -4 3) => 3

(define (quadratic-root a b c)
  (/ (+ (* b -1) (sqrt (- (* b b) (* 4 (* a c))))) (* 2 a)))
  