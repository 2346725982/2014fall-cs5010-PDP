;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; pretty.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FILE REQUIRED

(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION PROVIDE

(provide
 expr-to-strings
 make-sum-exp
 make-mult-exp
 sum-exp-exprs
 mult-exp-exprs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define ZERO 0)
(define THREE 3)
(define SPACE " ")
(define TRIPLE-SPACE "   ")
(define RIGHT-PARENTHESIS ")")
(define RIGHT-PAREN-CHAR #\))
(define SUM-TITLE "(+")
(define MULT-TITLE "(*")
(define ERROR-MESSAGE "not enough room")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DATA DEFINITION

(define-struct sum-exp (exprs))
;; A sum-exp is a (make-sum-exp NELOExpr)
;; Interp:
;;  -- exprs: a non-empty list of Expr

;; Examples:
;(make-sum-exp (list 22 33 44))

;; Template:
;; sum-exp-fn : sum-exp -> ??
;; (define (sum-exp-fn se)
;;    (... (sum-exp-exprs se)))

(define-struct mult-exp (exprs))
;; A mult-exp is a (make-mult-exp NELOExpr)
;; Interp:
;;  -- exprs: a non-empty list of Expr

;; Examples:
;(make-mult-exp (list 22 33 44))

;; Template:
;; mult-exp-fn : mult-exp -> ??
;; (define (mult-exp-fn me)
;;    (... (mult-exp-exprs me)))


;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-mult-exp NELOExpr)

;; Interp: a sum-exp represents a sum operation and
;;         a mult-exp represents a multiplication operation.

;; Template:
;; expr-fn : Expr -> ??
;(define (expr-fn exp)
;  (cond
;    [(integer? exp) ...]
;    [(sum-exp? exp) ... (sum-exp-exprs exp)]
;    [(mult-exp? exp) ... (mult-exp-exprs exp)]))


;; A ListOfExpr (LOE) is one of:
;; -- empty
;; -- (cons Expr LOE)

;; Template:
;; loe-fn : LOE -> ??
;(define (loe-fn loe)
;  (cond
;    [(empty? loe) ...]
;    [else (...
;           (expr-fn (first loe))
;           (loe-fn (rest loe)))]))

;; A NELOExpr (Non-Empty ListOfExpr) is one of:
;; -- (cons Expr empty)
;; -- (cons Expr NELOExpr)

;; Template:
;; neloe-fn : NELOExpr -> ??
;(define (neloe-fn neloe)
;  (cond
;    [(empty? (rest neloe))
;     (... (first neloe))]
;    [else (... (first neloe)
;               (neloe-fn (rest neloe)))]))

;; A ListOfString (LOS) is one of:
;; -- empty
;; -- (cons String LOS)

;; Template:
;; los-fn : ListOf<String> -> ??
;(define (los-fn los)
;  (cond
;    [(empty? los) ...]
;    [else (...
;           (... (first los))
;           (los-fn (rest los)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-to-strings : Expr NonNegInt -> ListOf<String>
;; GIVEN: An expression and a width
;; RETURNS: A representation of the expression as a sequence of lines, 
;;          with each line represented as a string of length 
;;          no longer than the given width.
;; EXAMPLES: see test
;; STRATEGY: function composition
(define (expr-to-strings expr width) 
  (expr-to-strings/acc expr width ZERO))

;; expr-to-strings/acc : Expr NonNegInt NonNegInt -> ListOf<String>
;; GIVEN: an expression expr, a number width and a number end
;; RETURNS: a list of strings representing the given expression
;; WHERE: width is the maximum width of the expression on a single line and
;;        end is the number of end parenthesis for this expression
;; EXAMPLE (tested by expr-to-strings): 
;(expr-to-strings/acc SUM-EXPR1 10 ZERO)
;= '("(+ 22"
;    "   33"
;    "   44)")

;; STRATEGY: function composition
(define (expr-to-strings/acc expr width end)
  (if (<= (+ (string-length (one-line expr)) end) width)
      (list (string-append (one-line expr) 
                           (make-string end RIGHT-PAREN-CHAR)))
      (special-case expr width end)))

;; one-line : Expr -> String
;; GIVEN: an expression
;; RETURNS: the string representation of the given expression on a single line
;; EXAMPLES: see test

;; STRATEGY: struct decomp on expr : Expr
(define (one-line expr)
  (cond 
    [(integer? expr) (number->string expr)]
    [(sum-exp? expr) (expr-one-line SUM-TITLE (sum-exp-exprs expr))]
    [(mult-exp? expr) (expr-one-line MULT-TITLE (mult-exp-exprs expr))]))

;; expr-one-line : String Expr -> String
;; GIVEN: a title and an expression
;; RETURNS: the string representation of the given expression
;;          with the given title prepending it
;; WHERE: the expression would only be sum-exp and mult-exp
;;        and the given title is either "(+" or "(*"
;; EXAMPLES (tested by one-line):
;(expr-one-line SUM-TITLE '(22 33 44)) = "(+ 22 33 44)"
;(expr-one-line MULT-TITLE '(22 33 44)) = "(* 22 33 44)"

;; STRATEGY: HOFC
(define (expr-one-line title expr)
  (string-append 
   title
   (foldr
    ;; Expr String -> String
    ;; GIVEN: an expression and a string of the rest
    ;; RETURNS: a string after adding the first element into the rest 
    (lambda (f rest)
      (string-append SPACE (one-line f) rest))
    RIGHT-PARENTHESIS
    expr)))

;; special-case : Expr NonNegInt NonNegInt -> ListOf<String>
;; GIVEN: a expression expr, a number width and a number end
;; RETURNS: a list of strings representing the given expression
;; WHERE: width is the maximum width of the expression on a single line and
;;        end is the number of end parenthesis for this expression
;; EXAMPLES (tested by expr-to-strings):
;(special-case ZERO 10 0) = error: "not enough room"
;(special-case SUM-EXPR1 10 0) = (list "(+ 22" "   33" "   44)")
;(special-case MULT-EXPR5 10 0) = (list "(* 22" "   33" "   44)")

;; STRATEGY: struct decomp on expr : Expr
(define (special-case expr width end)
  (cond
    [(integer? expr) (error ERROR-MESSAGE)]
    [(sum-exp? expr)
     (add-title SUM-TITLE (next-level (sum-exp-exprs expr) width end))]
    [(mult-exp? expr)
     (add-title MULT-TITLE (next-level (mult-exp-exprs expr) width end))]))


;; next-level : NELOExpr NonNegInt NonNegInt -> ListOf<String>
;; GIVEN: a non-empty list of expressions (neloe), a number w and a number e
;; RETURNS: a list of strings representing a formatted
;;          subexpression
;; WHERE: the expression is either a sum-exp or mult-exp,
;;        w is the maximum width of the line, and
;;        e is the number of end parenthesis
;; EXAMPLES (tested by expr-to-strings):
;(next-level (sum-exp-exprs SUM-EXPR1) 10 ZERO) = '("22" "33" "44)")

;; STRATEGY: struct decomposition on neloe : NELOExpr
(define (next-level neloe w e)
  (cond
    [(empty? (rest neloe))
     (expr-to-strings/acc (first neloe) (- w THREE) (add1 e))]
    [else 
     (append 
      (expr-to-strings/acc (first neloe) (- w THREE) ZERO) 
      (next-level (rest neloe) w e))]))

;; add-title : String ListOf<String> -> ListOf<String>
;; GIVEN: a title and a list of strings
;; RETURNS: a list of strings after added title and space added
;; WHERE: title would only be "(+" and "(*"
;; EXAMPLE (tested by expr-to-strings):
;(add-title SUM-TITLE '("22" "333" "44")) = '("(+ 22" "   333" "   44")

;; STRATEGY: struct decomposition on lst : ListOf<String>
(define (add-title str lst)
  (cons
   (string-append str SPACE (first lst))
   (add-spaces (rest lst))))

;; add-spaces : ListOf<String> -> ListOf<String>
;; GIVEN: a list of strings
;; RETURNS: a list of strings like the given but 
;; with three spaces added to the beginning of each string
;; EXAMPLES: see test

;; STRATEGY: HOFC
(define (add-spaces lst)
  (map
   (lambda (f)
     (string-append TRIPLE-SPACE f))
   lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST

;; EXPR DEFINITIONS
(define SUM-EXPR1 (make-sum-exp (list 22 33 44)))
(define MIXED-EXPR2 (make-sum-exp
                     (list
                      (make-mult-exp (list 22 3333 44))
                      (make-mult-exp
                       (list
                        (make-sum-exp (list 66 67 68))
                        (make-mult-exp (list 42 43))))
                      (make-mult-exp (list 77 88)))))
(define MIXED-EXPR3 (make-sum-exp
                     (list 
                      (make-mult-exp (list 11 22 33))
                      44)))
(define SUM-EXPR4 (make-sum-exp (list 1)))
(define MULT-EXPR5 (make-mult-exp (list 22 33 44)))

;; one-line tests/examples
(begin-for-test (check-equal? (one-line ZERO) "0"
                              "'0' turns to a string '0'")
                (check-equal? (one-line SUM-EXPR1) "(+ 22 33 44)"
                              "SUM-EXPR1 turns to a string '(+ 22 33 44)'")
                (check-equal? (one-line MULT-EXPR5) "(* 22 33 44)"
                              "MULT-EXPR5 turns to a string '(* 22 33 44)'"))

;; add-spaces tests/examples
(begin-for-test 
  (check-equal? (add-spaces empty) 
                empty
                "The empty list doesn't get any spaces added")
  (check-equal? (add-spaces '("a" "b" "c"))
                '("   a" "   b" "   c")
                "The list of 'a', 'b', 'c' gets three spaces for each string"))

;; expr-to-strings tests/examples
(begin-for-test
  (check-equal? (expr-to-strings SUM-EXPR1 25)
                (list "(+ 22 33 44)")
                "the expression should show in one line without line feed")
  (check-equal? (expr-to-strings 3 1)
                (list "3")
                "still has enough width for the Expr whose type is Integer")
  (check-equal? (expr-to-strings SUM-EXPR1 10)
                (list "(+ 22" "   33" "   44)")
                "does not has enough width for the wholde expression, so it
                 should be separated into three lines")
  (check-equal? (expr-to-strings MIXED-EXPR2 100)
                (list "(+ (* 22 3333 44) (* (+ 66 67 68) (* 42 43)) (* 77 88))")
                "the expression should show in one line without line feed")
  (check-equal? (expr-to-strings MIXED-EXPR2 15)
                (list
                 "(+ (* 22"
                 "      3333"
                 "      44)"
                 "   (* (+ 66"
                 "         67"
                 "         68)"
                 "      (* 42"
                 "         43))"
                 "   (* 77 88))")
                "does not have enough width and should display into 9 lines")
  (check-equal? (expr-to-strings MIXED-EXPR3 10)
                (list "(+ (* 11" "      22" "      33)" "   44)")
                "does not have enough width and should display into 4 lines")
  (check-equal? (expr-to-strings SUM-EXPR4 10)
                (list "(+ 1)")
                "only has one parameter for the sum operation")
  (check-error (expr-to-strings MIXED-EXPR3 5)
               "width is too small to convert"))