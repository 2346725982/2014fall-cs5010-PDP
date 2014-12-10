#lang racket

;; test-bool: ListofBoolean -> Boolean
;; GIVEN: list of boolean
;; RETURNS: true if all the elements of the list is true; false otherwise
;; Example:
;; (test-bool empty) => true
;; (test-bool (list true true true)) => true
;; (test-bool (list true true true false true)) => false

(define (test-bool lst)
  (cond
    [(empty? lst) true]
    [(first lst) (test-bool (rest lst))]
    [else false]))