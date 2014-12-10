;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |15|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct student (id name major))
    ;; A Student is a (make-point PosInt String String).
    ;; It represents a basic status of a student, including id, name and major.
    ;; Interpretation:
    ;;   id = the student's id number.
    ;;   name = the student's name.
    ;;   major = the student's major.