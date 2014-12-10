;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require srfi/13)

(provide
 make-editor
 editor-pre
 editor-post
 edit)

;Data Definition
(define-struct editor (pre post))

;Constructor Template
;an Editor is a
;(make-editor string string)

;Interpretation
; pre is the pre field of an editor
; post is the rest of an editor

;Destructor Template
;editor-fn: Editor -> ??
;(define (editor-fn e)
;  (...
;   (editor-pre e)
;   (editor-post e)))

;string-first: string -> string
;string-last: string -> string
;GIVEN: a string
;RETURNS: the first or last character of the string as a string
;if a string is empty or "", returns ""
;Examples:
;(string-first "helloworld") = "h"
;(string-last "helloworld") = "d"
;Design Strategy: Function Composition
;Function Definition
(define (string-first str)
  (cond
    [(or (empty? str) (= (string-length str) 0)) ""]
    [else (string-take str 1)]))

(define (string-last str)
  (cond
    [(or (empty? str) (= (string-length str) 0)) ""]
    [else (string-take-right str 1)]))

;string-remove-first: string -> string
;string-remove-last: string -> string
;GIVEN: a string
;RETURNS: a string that is removed its first or last character
;if a string is empty or "", returns ""
;Example:
;(string-remove-first "helloworld") = "elloworld"
;(string-remove-last "helloworld") = "helloworl"
;Design Strategy: Function Composition
;Function Definition
(define (string-remove-first str)
  (cond
    [(or (empty? str) (= (string-length str) 0)) ""]
    [else (string-drop str 1)]))

(define (string-remove-last str)
  (cond
    [(or (empty? str) (= (string-length str) 0)) ""]
    [else (string-drop-right str 1)]))

;edit: Editor string -> Editor
;GIVEN: an editor and a KeyEvent of string type,
;which indicate the operation of the editor
;RETURENS: an editor after the operation
;Design Strategy: Cases
;Function Definition
(define (edit ed ke)
  (cond
    [(= (string-length ke) 1)
     (cond
       [(or (equal? ke "\t") (equal? ke "\u007F"))
        (make-editor (editor-pre ed) (editor-post ed))]
       [(equal? ke "\b")
         (make-editor (string-remove-last (editor-pre ed)) (editor-post ed))]
       [else
        (make-editor (string-append (editor-pre ed) ke) (editor-post ed))])]
    [(equal? ke "left")
     (make-editor (string-remove-last (editor-pre ed)) 
                  (string-append (string-last (editor-pre ed)) (editor-post ed)))]
    [(equal? ke "right")
     (make-editor (string-append (editor-pre ed) (string-first (editor-post ed)))
                  (string-remove-first (editor-post ed)))]))

(begin-for-test
  (check-equal? (string-first "helloworld") "h" 
                "first char of a string")
  (check-equal? (string-first "") "" 
                "first char in a string with no char is a string of no char")
  (check-equal? (string-first empty) "" 
                "first char in an empty string is a string of no char")
  (check-equal? (string-last "helloworld") "d" 
                "last char of a string")
  (check-equal? (string-last "") "" 
                "last char in a string with no char is a string of no char")
  (check-equal? (string-last empty) "" 
                "last char in an empty string is a string of no char")
  (check-equal? (string-remove-first "helloworld") "elloworld"
                "remove the first char of a string")
  (check-equal? (string-remove-first "") ""
                "a string of no char removed its first char would be a string of no char")
  (check-equal? (string-remove-first empty) ""
                "an empty string removed its first char would be a string of no char")
  (check-equal? (string-remove-last "helloworld") "helloworl"
                "remove the last char of a string")
  (check-equal? (string-remove-last "") ""
                "a string of no char removed its last char would be a string of no char")
  (check-equal? (string-remove-last empty) ""
                "an empty string removed its last char would be a string of no char")
  (check-equal? (edit (make-editor "a" "b") "a") (make-editor "aa" "b")
                "input a single character")
  (check-equal? (edit (make-editor "a" "b") "\t") (make-editor "a" "b")
                "if the input is '\t', ignore it")
  (check-equal? (edit (make-editor "a" "b") "\u007F") (make-editor "a" "b")
                "if the input is '\u007F', ignore it")
  (check-equal? (edit (make-editor "aa" "b") "\b") (make-editor "a" "b")
                "backspace one char")
  (check-equal? (edit (make-editor "a" "b") "\b") (make-editor "" "b")
                "backspace one char, and no char left")
  (check-equal? (edit (make-editor "" "b") "\b") (make-editor "" "b")
                "cannot backspace a char because there is no char")
  (check-equal? (edit (make-editor "aa" "b") "left") (make-editor "a" "ab")
                "move cursor to the left")
  (check-equal? (edit (make-editor "a" "b") "left") (make-editor "" "ab")
                "move cursor to the left, and no char left")
  (check-equal? (edit (make-editor "" "b") "left") (make-editor "" "b")
                "cannot move cursor to the left, because there is no char")
  (check-equal? (edit (make-editor "a" "bb") "right") (make-editor "ab" "b")
                "move cursor to the right")
  (check-equal? (edit (make-editor "a" "b") "right") (make-editor "ab" "")
                "move cursor to the right, and no char left")
  (check-equal? (edit (make-editor "a" "") "right") (make-editor "a" "")
                "cannot move cursor to the right, because there is no char"))