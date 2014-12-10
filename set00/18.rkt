#lang racket
(require 2htdp/image)
(require math)

;; rec-sequence : Number -> A Rectangle
;; GIVEN: the arguments n
;; RETURNS: the nth element in this sequence, using the formula width=2^(n+1) height=2^(n+2)
;; Example:
;; (rec-sequence 4) => rectangle(32 64 'solid' blue)
;; (rec-sequence 5) => rectangle(64 128 'solid' blue)

(define (rec-sequence n)
  (rectangle (expt 2 (+ n 1)) (expt 2 (+ n 2)) 'solid' blue))