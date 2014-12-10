#lang racket

;; draw-string : List -> string 
;; GIVEN : list of string
;; RETURNS : a string combine of each string, seperated by " "
;; Example:
;; (draw-string empty)  => ""
;; (draw-string (list "hello")) => "hello "
;; (draw-string (list "hello" "world")) =>"hello world "
;; (draw-string (list "hello" "world" "!" "this" "is" "a" "test")) => "hello world ! this is a test "

(define (draw-string lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (first lst) (string-append " " (draw-string (rest lst))))]))

