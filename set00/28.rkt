;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |28|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;; Example :
;; (draw-list-of-string empty) => ""
;; (draw-list-of-string (list empty empty empty)) => "   "
;; (draw-list-of-string (list (list "hello" "world!"))) => "hello world!  "
;; (draw-list-of-string (list (list "hello" "world!") (list "this" "is" "a" "test."))) => "hello world!  this is a test.  "

(define (draw-string lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (first lst) (string-append " " (draw-string (rest lst))))]))

(define (draw-list-of-string-text lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (draw-string (first lst)) "\n" (draw-list-of-string-text (rest lst)))]))

(define (draw-list-of-string lst)
  (text (draw-list-of-string-text lst) 20 "blue"))