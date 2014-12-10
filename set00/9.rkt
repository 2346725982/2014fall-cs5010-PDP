#lang racket
; even : Number -> boolean
; GIVEN: a number
; RETURNS: True if it's a even number, False if it's an odd number
; Example:
; (even 2) => True
; (even 3) => False
; what's the difference?

(define (even x)
  (= (remainder x 2) 0))