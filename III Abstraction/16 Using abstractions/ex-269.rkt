#lang htdp/isl+

; An Inventory is a [List-of IR]

(define-struct IR [name description acquisition-price sale-price])
; An IR is a structure:
; (make-IR String String Number Number)
; interpretation an IR (inventory record) associates
; the name of an item with its description,
; acquisition price, and recommended sales price

; --- test data ---
(define IR-cheap     (make-IR "book" "novel" 5 10))
(define IR-mid       (make-IR "phone" "smart" 200 250))
(define IR-expensive (make-IR "tv" "flat screen" 300 450))

; Number Inventory -> Inventory
; produces an Inventory from inv where sales price
; is below ua
(check-expect (eliminate-expensive 500 '()) '())
(check-expect (eliminate-expensive 500 (list IR-cheap IR-mid IR-expensive))
              (list IR-cheap IR-mid IR-expensive))
(check-expect (eliminate-expensive 10 (list IR-cheap IR-mid IR-expensive))
              '())
(check-expect (eliminate-expensive 300 (list IR-cheap IR-mid IR-expensive))
              (list IR-cheap IR-mid))
(define (eliminate-expensive ua inv)
  (local (; IR -> Boolean
          ; determines whether sales price of given ir
          ; is below ua
          (define (cheap-enough? ir)
            (< (IR-sale-price ir) ua)))
    (filter cheap-enough? inv)))

; String Inventory -> Inventory
; produces an Inventory from inv with items that don't use name ty
(check-expect (recall "book" '()) '())
(check-expect (recall "laptop" (list IR-cheap IR-mid IR-expensive))
              (list IR-cheap IR-mid IR-expensive))
(check-expect (recall "book" (list IR-cheap IR-mid IR-expensive))
              (list IR-mid IR-expensive))
(check-expect (recall "book" (list IR-cheap IR-cheap))
              '())
(define (recall ty inv)
  (local (; IR -> Boolean
          ; determines whether IR item name doesn't use ty
          (define (keep-name? ir)
            (not (string=? (IR-name ir) ty))))
    (filter keep-name? inv)))

; [List-of String] [List-of String] -> [List-of String]
; returns list of names from l2 which are also present in l1
(check-expect (selection '() '()) '())
(check-expect (selection '() (list "a" "b")) '())
(check-expect (selection (list "a" "b") '()) '())
(check-expect (selection (list "a" "b") (list "c" "d")) '())
(check-expect (selection (list "a" "b") (list "a" "b")) (list "a" "b"))
(check-expect (selection (list "a" "c") (list "a" "b" "c")) (list "a" "c"))
(check-expect (selection (list "a") (list "a" "a")) (list "a" "a"))
(define (selection l1 l2)
  (local (; String -> Boolean
          ; determines whether l1 contains name
          (define (occurs-in-first-list? name)
            (member? name l1)))
    (filter occurs-in-first-list? l2)))
