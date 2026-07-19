#lang htdp/isl+

(define-struct IR [name description acquisition-price sale-price])
; An IR is a structure:
; (make-IR String String Number Number)
; interpretation an IR (inventory record) associates
; the name of an item with its description,
; acquisition price, and recommended sales price

; An Inventory is a [List-of IR]

; Inventory -> Inventory
; Sorts inventory by the value of sale-price - acquisition-price
; in descending order
(check-expect (sort-by-price-diff '()) '())

(check-expect (sort-by-price-diff
               (list (make-IR "widget" "a widget" 5 8)))
              (list (make-IR "widget" "a widget" 5 8)))

(check-expect (sort-by-price-diff
               (list (make-IR "cheap" "low margin" 10 12)    ; diff 2
                     (make-IR "pricey" "high margin" 4 20)   ; diff 16
                     (make-IR "mid" "mid margin" 6 14)))     ; diff 8
              (list (make-IR "pricey" "high margin" 4 20)
                    (make-IR "mid" "mid margin" 6 14)
                    (make-IR "cheap" "low margin" 10 12)))

; negative diffs (selling at a loss) should sort after positive ones
(check-expect (sort-by-price-diff
               (list (make-IR "loss" "" 10 5)    ; diff -5
                     (make-IR "gain" "" 5 10)))   ; diff 5
              (list (make-IR "gain" "" 5 10)
                    (make-IR "loss" "" 10 5)))

(define (sort-by-price-diff inv)
  (local (; IR -> Number
          ; calculates difference between sale price
          ; and acquisition price
          (define (margin ir)
            (- (IR-sale-price ir) (IR-acquisition-price ir))))
    (sort inv (lambda (ir-1 ir-2)
                (> (margin ir-1) (margin ir-2))))))
