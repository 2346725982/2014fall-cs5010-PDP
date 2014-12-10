#lang racket
(require 2htdp/image)

;; rel-rec-sequence: Number Number -> Rectangle
;; Takes two numbers and returns a solid blue rectangle, where the first number is
;; the width of the rectangle, and the second number is the proportion of width
;; and height of the rectangle to be produced (i.e. height = width * proportion).
;; Example:
;; (rel-rec-sequence 16 0.8) =>  (rectangle 16 10.8 'solid' blue))
;; (rel-rec-sequence 32 1.5) =>  (rectangle 32 48 'solid' blue))

(define (rel-rec-sequence w p)
  (rectangle w (* w p) 'solid' blue))
