;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 27a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; draw-string : List -> string 
;; GIVEN : list of string
;; RETURNS : a string combine of each string, seperated by " "
;; Example:
;; (draw-string empty)  => ""
;; (draw-string (list "hello")) => "hello "
;; (draw-string (list "hello" "world")) =>"world hello  "
;; (draw-string (list "hello" "world" "!" "this" "is" "a" "test")) => "test a is this ! world hello"

(define (draw-string lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (draw-string (rest lst)) (string-append " " (first lst)))]))

