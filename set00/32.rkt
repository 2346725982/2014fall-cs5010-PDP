#lang racket

;; distannce : List -> Number
;; GIVEN : a list of points
;; RETURNS : distance, using the formula distance = point-x + point-y
;; Example : 
;; (distance (list (make-point 0 0))) => 0
;; (distance (list (make-point 5 3))) => 8
;; (distance (list (make-point 5 3) (make-point 10 8) (make-point 7 3))) => 36

(define-struct point(x y))

(define (distance lst)
  (cond
    [(empty? lst) 0]
    [else (+ (+ (point-x (first lst)) (point-y (first lst))) (distance (rest lst)))]))