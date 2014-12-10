#lang racket
; sq : NonNegNumber -> Number
; GIVEN: a nonnegative number
; RETURNS: the square of the number
; Example:
; (sq 5) => 25
; (sq 10) => 100

(define (sq x)
  (* x x))
