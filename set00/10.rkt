#lang racket
; sum : Number Number Number -> Number
; GIVEN: three numbers as arguments
; RETURNS: the sum of the two larger numbers.
; Exmaples:
; (sum 1 2 3) => 5
; (sum 1 3 2) => 5
; (sum 3 2 1) => 5
; (sum 3 1 2) => 5
; (sum 2 1 3) => 5
; (sum 2 3 1) => 5

(define (sum a b c)
  (cond
    [(and (< a b) (< a c)) (+ b c)]
    [(> b c) (+ a b)]
    [else (+ a c)]))