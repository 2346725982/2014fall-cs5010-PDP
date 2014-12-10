#lang racket
; area : Number -> Number
; GIVEN: the radius r of a circle
; RETURNS: its area, using the formula pi * r^2.
; Examples:
; (area 1) => 3.141592653589793
; (area 5) => 78.53981633974483
; (area 7) => 153.93804002589985

(define (area r)
  (* pi (* r r)))
