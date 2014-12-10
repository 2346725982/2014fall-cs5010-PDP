#lang racket

;15:37
;(reversed-bool (list true false true))
;(#f #f #t)
;53


(define (reversed-bool lst)
  (cond
    [(empty? lst) ]
    [(first lst) (cons false (reversed-bool (rest lst)))]
    [(not (first lst)) (cons true (reversed-bool (rest lst)))]
    ))