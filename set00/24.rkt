#lang racket

(define (product lst)
  (cond
    [(empty? lst) 1]
    [else (* (first lst) (product (rest lst)))]))