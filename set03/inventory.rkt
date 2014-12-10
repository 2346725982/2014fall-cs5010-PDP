;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; inventory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FILE REQUIRED

(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION PROVIDE

(provide
 inventory-potential-profit
 inventory-total-volume
 price-for-line-item 
 fillable-now? 
 days-til-fillable
 price-for-order
 inventory-after-order
 increase-prices
 make-book  
 make-line-item 
 reorder-present? 
 make-empty-reorder
 make-reorder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define ZERO 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; Book
(define-struct book (isbn title author publisher unit-price unit-cost 
                          on-hand reorder-status volume))
;; A Book is a 
;;  (make-book Integer String String String NonNegInt NonNegInt 
;;                     NonNegInt ReorderStatus Real)
;; Interpretation:
;; isbn is an integer serving as a unique identifier for this book. 
;; title is a string representing the book's title
;; author is a string, it is the author’s name
;; publisher is a string, it is the publisher's name
;; unit-price is a non-negative integer, the price at which we will 
;;  sell the book, in USD*100, ie $14.99 is represented as 1499
;; unit-cost is a a non-negative integer, the cost of the book to the 
;;  bookstore in USD*100, also a non-negative integer
;; on-hand is a non-negative integer, it means number of copies on hand
;; Reorder-Status is a compound data, it means the reorder of the book, 
;;  defined below
;; volume is a real number, it means the volume taken up  by this item, 
;;  in cubic feet.

;; book-fn : Book -> ??
;; (define (book-fn b)
;;  (... 
;;   (book-isbn b)
;;   (book-title b)
;;   (book-author b)
;;   (book-publisher b)
;;   (book-unit-price b)
;;   (book-unit-cost b)
;;   (book-on-hand b)
;;   (book-volumn b)
;;   (book-re-order b))

;; ListofBooks
;; A ListOfBooks (LOB) is either
;; -- empty
;; -- (cons Book LOB)

;; lob-fn : LOB -> ??
;; (define (lob-fn lob)
;;   (cond
;;     [(empty? lob) ...]
;;     [else (...
;;             (book-fn (first lob))
;;             (lob-fn (rest lob)))]))

;; Inventory
;; An Inventory is a ListOfBooks.
;; Interp: the list of books that the bookstore carries, in any order.

;; ReorderStatus
(define-struct reorder-status (present? days copies))
;; A Reorder-Status is a
;;  (make-reorder Boolean NonNegInt NonNegInt)
;; Interpretation
;; presennt? is a boolean, it is true iff the given ReorderStatus 
;;  shows a pending re-order
;; days is a non-negative integer, it's the number of days until the the 
;;  next shipment
;; copies is a non-negative integer, the number of copies expected to arrive

;; reorder-status-fn : Reorder -> ??
;; (define (reorder-status-fn r)
;;  (...
;;   (reorder-status-present? r)
;;   (reorder-status-days r)
;;   (reorder-status-copies r))

;; LineItem
(define-struct line-item (isbn quantity))
;; A LineItem is a 
;;  (make-lineitem Integer PosInt)
;; Interpretation:
;; isbn is an integer serving as a unique identifier for this book. 
;; quantitiy is the quantity ordered of a book.

;; lineitem-fn : LineItem -> ??
;; (define (lineitem-fn l)
;;  (...
;;   (lineitem-isbn l)
;;   (lineitem-quantity l))

;; ListofLineitems
;; A ListOfLineitems (LOL) is either
;; -- empty
;; -- (cons LineItem LOL)

;; lol-fn : LOL -> ??
;; (define (lol-fn lol)
;;   (cond
;;     [(empty? lol) ...]
;;     [else (...
;;             (lineitem-fn (first lol))
;;             (lol-fn (rest lol)))]))

;; Order
;; An Order is a ListOfLineitems.
;; Interp: the list of line item that the order carries.

;; A MaybeInteger is one of:
;; -- Integer : returns an interger
;; -- false : returns false if it cannot return an integer

;; mi-fn : MaybeInteger -> ??
;; (define (mi-fn mi)
;;   (cond
;;     [(integer? mi) ...]
;;     [(false? mi) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-book  (9 arguments)
;; make-line-item (2 arguments)
;; The arguments to these functions should appear in the same order as
;; they do in the problem statement.

;; reorder-present? : ReorderStatus -> Boolean
;; GIVEN: a reorder status
;; RETURNS: true iff the given ReorderStatus shows a pending re-order.
;; EXAMPLE: 
;; (reorder-present? (make-reorder-status true 0 0)) => true
;; STRATEGY: structural decomposition on rs : ReorderStatus
(define (reorder-present? rs)
  (reorder-status-present? rs))

;; make-empty-reorder : Any -> ReorderStatus
;; GIVEN: anything, its argument is ignored
;; RETURNS: a ReorderStatus showing no pending re-order.
;; EXAMPLE:
;; (make-empty-reorder 0) => (make-reorder-status false 0 0)
;; STRATEGY: function composition
(define (make-empty-reorder any)
  (make-reorder-status false ZERO ZERO))

;; make-reorder : PosInt PosInt -> ReorderStatus
;; GIVEN: a number of days and a number of copies
;; RETURNS: a ReorderStatus with the given data.
;; EXAMPLE:
;; (make-reorder 1 1) => (make-reorder-status true 1 1)
;; STRATEGY: function composition
(define (make-reorder days copies)
  (make-reorder-status true days copies))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; inventory-potential-profit : Inventory ->  Integer
;; GIVEN: an inventory
;; RETURNS: the total profit, in USD*100, for all the items in stock 
;;  (i.e., how much the bookstore would profit if it sold every book 
;;  in the inventory).
;; EXAMPLE:
;; (inventory-potential-profit 
;;  (list (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;        (make-book 12346 "computer" "Stephen" "ABC" 1300 800 30 r1 13)))
;; =21000
;; STRATEGY: structural decompostion on in : Inventory
(define (inventory-potential-profit in)
  (cond
    [(empty? in) ZERO]
    [else (+ (book-potential-profit (first in))
             (inventory-potential-profit (rest in)))]))

;; book-potential-profit : Book ->  Integer
;; GIVEN: a book
;; RETURNS: the total profit of one kind of book
;; EXAMPLE:
;; (book-potential-profit 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12))
;; =6000
;; STRATEGY: structural decompostion on b : Book
(define (book-potential-profit b)
  (* (- (book-unit-price b) (book-unit-cost b))
     (book-on-hand b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; inventory-total-volume : Inventory -> Real
;; GIVEN: an inventory
;; RETURNS: the total volume needed to store all the books in stock.
;; EXAMPLE: 
;; (inventory-total-volume 
;;  (list (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;        (make-book 12346 "computer" "Stephen" "ABC" 1300 800 30 r1 13)))
;; =630
;; STRATEGY: structural decompostion on in : Inventory
(define (inventory-total-volume in)
  (cond
    [(empty? in) ZERO]
    [else (+ (book-total-volume (first in))
             (inventory-total-volume (rest in)))]))

;; book-total-volume : Book ->  Real
;; GIVEN: a book
;; RETURNS: the total volumen needed by one kind of book
;; EXAMPLE:
;; (book-total-volume 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12))
;; =240
;; STRATEGY: structural decompostion on b : Book
(define (book-total-volume b)
  (* (book-volume b)
     (book-on-hand b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; increase-prices : Inventory String Real -> Inventory
;; GIVEN: an inventory, a publisher, and a percentage,
;; RETURNS: an inventory like the original, except that all items by that
;; publisher have their unit prices increased by the specified
;; percentage.
;; EXAMPLE: (increase-prices inventory1 "MIT Press" 10)
;; returns an inventory like the original, except that all MIT Press
;; books in the inventory have had their prices increased by 10%.
;; STRATEGY: structural decomposition on in : Inventory
(define (increase-prices in pub per)
  (cond
    [(empty? in) empty]
    [else (cons (book-increase-prices (first in) pub per) 
                (increase-prices (rest in) pub per))]))

;; book-increase-prices : Book String PosInt -> Book
;; GIVEN: a book, a publisher, and a percentage
;; RETURNS: a book increased prices by percentage if its publisher is
;; the publisher, otherwise it would be just the same book
;; EXAMPLE: 
;; (updated-book-unit-price
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;  "ABC" 10)
;; =(make-book 12345 "computer" "Wand" "ABC" 1320 900 20 r1 12)
;; STRATEGY: structural decomposition on b : Book
(define (book-increase-prices b pub per)
  (if (string=? (book-publisher b) pub)
      (make-book (book-isbn b) 
                 (book-title b) 
                 (book-author b) 
                 (book-publisher b) 
                 (* (book-unit-price b) (+ 1 (/ per 100)))
                 (book-unit-cost b) 
                 (book-on-hand b) 
                 (book-reorder-status b) 
                 (book-volume b))
      b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; price-for-order : Inventory Order -> NonNegInteger
;; GIVEN: an inventory and an order
;; RETURNS: the total price for the given order, in USD*100.  
;; The price does not depend on whether any particular line item is in stock.
;; Line items for an ISBN that is not in the inventory count as 0.
;; EXAMPLE
;; (price-for-order 
;;  (list (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;        (make-book 12346 "computer" "Stephen" "ABC" 1300 800 30 r1 13))
;;  (list (make-line-item 12345 20)
;;        (make-line-item 12 10)))
;; =24000
;; STRATEGY: structural decompostion on ord : Order
(define (price-for-order in ord)
  (cond
    [(empty? ord) ZERO]
    [else (+ (mi->integer (price-for-line-item in (first ord)))
             (price-for-order in (rest ord)))]))

;; mi->integer : MaybeInteger -> Integer
;; GIVEN: a MaybeInteger data
;; RETURNS: a number if the data was a number, 0 if it was false
;; EXAMPLE
;; (mi->integer false) = 0
;; STRATEGY: structural decompostion on mi : MaybeInteger
(define (mi->integer mi)
  (cond
    [(integer? mi) mi]
    [(false? mi) ZERO]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; price-for-line-item : Inventory LineItem -> MaybeInteger
;; GIVEN: an inventory and a line item
;; RETURNS: the price for that line item (the quantity times the unit
;; price for that item).  Returns false if that isbn does not exist in
;; the inventory.
;; EXAMPLE
;; (book-after-line-item 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 30 r1 12) 
;;  (make-line-item 12345 15))
;; =15
;; STRATEGY: structural decompostion on in : Inventory
(define (price-for-line-item in li)
  (cond
    [(empty? in) false]
    [else (if (same-isbn? (first in) li)
              (price-each-book (first in) li)
              (price-for-line-item (rest in) li))]))

;; same-isbn? : Book LineItem -> Boolean
;; GIVEN: a book and a lineitem
;; RETURNS: true iff their same isbn are the same
;; EXAMPLE
;; (same-isbn?
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12) 
;;  (make-line-item 12345 10))
;; =true
;; STRATEGY: structural decompostion on li : LineItem
(define (same-isbn? b li)
  (same-isbn?-helper b (line-item-isbn li)))

;; same-isbn?-helper : Book Integer -> Boolean
;; GIVEN: a book and the isbn of a lineitem
;; RETURNS: true iff they are the same
;; EXAMPLE
;; (same-isbn?-helper
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12) 
;;  12345) => ture
;; STRATEGY: structural decompostion on b : Book
(define (same-isbn?-helper b li-isbn)
  (= (book-isbn b) li-isbn))

;; price-each-book : Book LineItem -> Integer
;; GIVEN: a book and a lineitem
;; RETURNS: the price of the book in the lineitem
;; EXAMPLE
;; (price-each-book
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12) 
;;  (make-line-item 12345 10))
;; = 12000
;; STRATEGY: structural decompostion on li : LineItem
(define (price-each-book b li)
  (price-eachbook-helper b (line-item-quantity li)))

;; price-each-book-helper : Book PosInt -> Integer
;; GIVEN: a book and the quantity of a lineitem
;; RETURNS: the price of the book in the lineitem
;; EXAMPLE
;; (price-each-book-helper 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12) 10)
;; = 12000
;; STRATEGY: structural decompostion on b : Book
(define (price-eachbook-helper b li-quantity)
  (* (book-unit-price b) li-quantity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; inventory-after-order : Inventory Order -> Inventory.
;; GIVEN: an inventory, an order
;; WHERE: the order is fillable now
;; RETURNS: the inventory after the order has been filled.
;; EXAMPLE
;; (inventory-after-order i1 o2)
;; =(list
;;   (make-book 12345 "computer" "Wand" "ABC" 1200 900 0 r1 12)
;;   (make-book 12346 "computer" "Stephen" "ABC" 1300 800 15 r1 13))
;; STRATEGY: structural decompostion on in : Inventory
(define (inventory-after-order in ord)
  (cond
    [(empty? in) empty]
    [else (cons (book-after-order (first in) ord)
                (inventory-after-order (rest in) ord))]))

;; book-after-order : Book Order -> Book.
;; GIVEN: a book, an order
;; WHERE: the order is fillable now
;; RETURNS: the book after the order has been filled.
;; EXAMPLE
;; (book-after-order 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 30 r1 12) 
;;  (list (make-line-item 12345 15)))
;; =15
;; STRATEGY: structural decompostion on ord : Order
(define (book-after-order b ord)
  (cond
    [(empty? ord) b]
    [else (if (same-isbn? b (first ord))
              (book-after-lineitem b (first ord))
              (book-after-order b (rest ord)))]))

;; book-after-lineitem : Book LineItem -> Book.
;; GIVEN: a book, a line item
;; RETURNS: the book after the line item has been filled.
;; EXAMPLE
;; (book-after-line-item 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 30 r1 12) 
;;  (make-line-item 12345 15))
;; =15
;; STRATEGY: structural decompostion on li : LineItem
(define (book-after-lineitem b li)
  (book-after-lineitem-helper b (line-item-quantity li)))

;; book-after-lineitem-helper : Book PosInt -> Book.
;; GIVEN: a book, the quantity of a line item
;; RETURNS: the book after the line item has been filled.
;; EXAMPLE
;; (book-after-line-item 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 30 r1 12) 15) => 15
;; STRATEGY: structural decompostion on b : Book
(define (book-after-lineitem-helper b li-quantity)
  (make-book (book-isbn b) 
             (book-title b) 
             (book-author b) 
             (book-publisher b) 
             (book-unit-price b) 
             (book-unit-cost b) 
             (-  (book-on-hand b) li-quantity)
             (book-reorder-status b) 
             (book-volume b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fillable-now? : Order Inventory -> Boolean.
;; GIVEN: an order and an inventory
;; RETURNS: true iff there are enough copies of each book on hand to fill
;; the order.  If the order contains a book that is not in the inventory,
;; then the order is not fillable.
;; EXAMPLE
;; (fillable-now? 
;;  (list (make-line-item 12345 20) (make-line-item 12 10))  
;;  (list (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;        (make-book 12346 "computer" "Stephen" "ABC" 1300 800 30 r1 13)))
;; = false
;; STRATEGY: structural decompostion on ord : Order
(define (fillable-now? ord in)
  (cond
    [(empty? ord) true]
    [else (and (lineitem-fillable-now? (first ord) in)
               (fillable-now? (rest ord) in))]))

;; lineitem-fillable-now? : LineItem Inventory -> Boolean.
;; GIVEN: a line item and an inventory
;; RETURNS: true iff there are enough copies of each book on hand to fill
;; the line item.  If the line item contains a book that is not in the 
;; inventory, then the line item is not fillable.
;; EXAMPLE
;; (lineitem-fillable-now? 
;;  (list (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;        (make-book 12346 "computer" "Stephen" "ABC" 1300 800 30 r1 13))
;;  (make-line-item 12345 20))
;; = true
;; STRATEGY: structural decompostion on in : Inventory
(define (lineitem-fillable-now? li in)
  (cond
    [(empty? in) false]
    [else (if (same-isbn? (first in) li)
              (book-lineitem-fillale? li (first in))
              (lineitem-fillable-now? li (rest in)))]))

;; book-lineitem-fillale? : LineItem Book -> Boolean.
;; GIVEN: a line item and a book
;; RETURNS: true iff there are enough copies of each book on hand to fill
;; the line item.  If the line item contains a book that is not in the 
;; inventory, then the line item is not fillable.
;; EXAMPLE
;; (book-lineitem-fillale?
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;  (make-line-item 12345 20))
;; = true
;; STRATEGY: structural decompostion on li : LineItem
(define (book-lineitem-fillale? li b)
  (book-lineitem-fillale?-helper (line-item-quantity li) b))

;; book-lineitem-fillale?-helper : PosInt Book -> Boolean.
;; GIVEN: the quantity of a line item and a book
;; RETURNS: true iff there are enough copies of each book on hand to fill
;; the line item.  If the line item contains a book that is not in the 
;; inventory, then the line item is not fillable.
;; EXAMPLE:
;; (book-lineitem-fillale?-helper
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;  20)
;; =true
;; STRATEGY: structural decompostion on b : Book
(define (book-lineitem-fillale?-helper li-quantity b)
  (>= (book-on-hand b) li-quantity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; days-til-fillable : Order Inventory -> MaybeInteger
;; GIVEN: an order and an inventory
;; RETURNS: the number of days until the order is fillable, assuming all
;; the shipments come in on time.  Returns false if there won't be enough
;; copies of some book, even after the next shipment of that book comes
;; in.
;; EXAMPLES: if the order contains one book that is out of stock, with a
;; reorder status showing 2 days until delivery, then the order is
;; fillable in 2 days.  If the order is for 10 copies of the book, and
;; the next order consists of only 5 books, then the function should 
;;  return false.
;; STRATEGY: structural decompostion on ord : Order
(define (days-til-fillable ord in)
  (cond
    [(empty? ord) ZERO]
    [else (days-needed (days-til-fillable-lineitem (first ord) in)
                       (days-til-fillable (rest ord) in))]))

;; days-til-fillable-lineitem : LineItem Inventory -> MaybeInteger
;; GIVEN: a lineitem and an inventory
;; RETURNS: the number of days until the lineitem is fillable
;; EXAMPLE
;; (days-til-fillable-line-item-inventory 
;;  (list (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12)
;;        (make-book 12346 "computer" "Stephen" "ABC" 1300 800 30 r1 13))
;;  (make-line-item 12345 30))
;; =2
;; STRATEGY: structural decompostion on in : Inventory
(define (days-til-fillable-lineitem li in)
  (cond
    [(empty? in) false]
    [else (if (same-isbn? (first in) li)
              (days-til-fillable-li-book (first in) li)
              (days-til-fillable-lineitem li (rest in)))]))

;; days-til-fillable-li-book : Book LineItem -> MaybeInteger
;; GIVEN: a book and a lineitem
;; RETURNS: the number of days until the lineitem is fillable
;; STRATEGY: structural decompostion on li : LineItem
(define (days-til-fillable-li-book b li)
  (days-til-fillable-li-book-helper b (line-item-quantity li)))

;; days-til-fillable-li-book-helper : Book PosInt -> MaybeInteger
;; GIVEN: a book and the quantity a lineitem
;; RETURNS: the number of days until the lineitem is fillable
;; EXAMPLE
;; (days-til-fillable-line-item-book 
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12) 
;;  (make-line-item 12345 30))
;; =2
;; STRATEGY: structural decompostion on b : Book
(define (days-til-fillable-li-book-helper b li-quantity)
  (days-til-fillable-li-book-helper2 (book-on-hand b) 
                                     (book-reorder-status b)
                                     li-quantity))

;; days-til-fillable-li-book-helper2 : 
;;                  NonNegInt ReorderStatus PosInt -> MaybeInteger
;; GIVEN: the on-hand number of a book, the reorder status of a book,
;;  and the quantity a lineitem
;; RETURNS: the number of days until the lineitem is fillable
;; EXAMPLE
;; (days-til-fillable-li-book-helper2
;;  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12) 30)
;; =2
;; STRATEGY: structural decompostion on rs : ReorderStatus
(define (days-til-fillable-li-book-helper2 on-hand rs li-quantity)
  (cond
    [(>= on-hand li-quantity) ZERO]
    [(>= (+ on-hand (reorder-status-copies rs)) li-quantity)
     (reorder-status-days rs)]
    [else false]))

;; days-needed : MaybeInteger MaybeInteger -> MaybeInteger
;; GIVEN: two parameters of MaybeInteger
;; RETURNS: a result of MaybeInteger
;; EXAMPLE
;; (days-needed 2 false) = false
;; STRATEGY: function composition
(define (days-needed mi1 mi2)
  (cond
    [(not (int? mi1)) false]
    [(not (int? mi2)) false]
    [else (max mi1 mi2)]))

;; int? : MaybeInteger -> Boolean
;; GIVEN: a MaybeInteger data
;; RETURNS: true if it is a integer
;; EXAMPLE:
;; (int? 1) = true
;; STRATEGY: structural decompostion on mi : MaybeInteger
(define (int? mi)
  (cond
    [(integer? mi) true]
    [(false? mi) false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFINE FOR TESTING

;; REORDER STATUS

(define r1 (make-reorder-status true 2 30))
(define r2 (make-reorder-status false 0 0))

;; BOOK

(define b1 
  (make-book 12345 "computer" "Wand" "ABC" 1200 900 20 r1 12))
(define b2 
  (make-book 12346 "computer" "Stephen" "ABC" 1300 800 30 r1 13))
(define b3 
  (make-book 12347 "computer" "John" "ABC" 1400 900 30 r2 13))
(define b4 
  (make-book 12348 "computer" "Andy" "ABC" 1600 1200 30 r1 13))
(define b5 
  (make-book 12348 "computer" "Marshall" "America" 1600 1200 30 r1 13))

;;INVENTORY

(define i1 (list b1 b2))
(define i2 (list b2 b3))
(define i3 (list b3 b4))
(define i4 (list b1 b2 b5))

;; LINE ITEM

(define li1 (make-line-item 12345 20))
(define li2 (make-line-item 12 10))
(define li3 (make-line-item 12346 15))
(define li4 (make-line-item 12345 40))
(define li5 (make-line-item 12347 15))
(define li6 (make-line-item 12348 35))
(define li7 (make-line-item 12347 40))

;; ORDER
(define o1 (list li1 li2))
(define o2 (list li1 li3))
(define o3 (list li1 li3))
(define o4 (list li5 li6))
(define o5 (list li5 li7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  ;; Test make-empty-reorder
  (check-equal? (make-empty-reorder 0) (make-reorder-status false 0 0)
                "Test make-empty-reorder, return a false reorder")
  ;; Test make-reorder
  (check-equal? (make-reorder 2 20)
                (make-reorder-status true 2 20)
                "Test make-reorder, should return a valid ReorderStatus")
  ;; Test reorder-present?
  (check-equal? (reorder-present? r1) true
                "Test reorder-present?, result should be true")
  
  ;; Test inventory-potential-profit
  (check-equal? (inventory-potential-profit i1) 21000 
                "Test calcuate inventory potential profit, 
                result should be 21000")
  ;; Test inventory-total-volume
  (check-equal? (inventory-total-volume i1) 630
                "Thst calcuate invetory total volumn
                result should be 630")
  ;; Test increase-prices
  (check-equal? (increase-prices i4 "ABC" 10)
                (list
                 (make-book 12345 
                            "computer" 
                            "Wand" 
                            "ABC" 
                            1320 
                            900 
                            20 
                            r1 
                            12)
                 (make-book 12346 
                            "computer" 
                            "Stephen" 
                            "ABC" 
                            1430 
                            800 
                            30 
                            r1 
                            13)
                 b5)
                "Test increase-prices, the unit-price for ABC 
                publisher should be increate 0.1")
  ;; Test price-for-order
  (check-equal? (price-for-order i1 o1) 24000
                "Test price-for-order, result should be 24000")
  ;; Test price-for-line-item
  (check-equal? (price-for-line-item i1 li1) 24000
                "Test price-for-line-item,result should be 24000")
  (check-equal? (price-for-line-item i1 li2) false
                "Test price-for-line-item, this book doesn't exist 
                result should be false")
  
  ;; Test inventory-after-order
  (check-equal? (inventory-after-order i1 o2)
                (list
                 (make-book 12345 "computer" "Wand" "ABC" 1200 900 0 r1 12)
                 (make-book 12346 
                            "computer" 
                            "Stephen" 
                            "ABC" 
                            1300 
                            800 
                            15 
                            r1 
                            13))
                "Test inventory-after-order when order is fillable")
  (check-equal? (inventory-after-order i1 empty) i1
                "Test inventory-after-order when order empty")
  
  ;; Test fillable-now?
  (check-equal? (fillable-now? o1 i1) false
                "Test fillable-now?, result should be false")
  (check-equal? (fillable-now? o2 i1) true
                "Test fillable-now?, result should be true")
  ;; Test days-til-fillable
  (check-equal? (days-til-fillable o1 i1) false
                "Test days-til-fillable , result should be false"
                )
  (check-equal? (days-til-fillable o3 i1) 0
                "Test days-til-fillable , result should be false,
                 because b2 is unfillable")
  (check-equal? (days-til-fillable o4 i3) 2
                "Test days-til-fillable , result should be 2")
  (check-equal? (days-til-fillable o5 i3) false
                "Test days-til-fillable , result should be false
                 because b3 out of store"))

