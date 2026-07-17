#lang htdp/isl+
(require 2htdp/image)

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

; consumes a number and decides whether it is less than 10
(lambda (n) (< n 10))

; multiplies two given numbers and turns the result into a string
(lambda (a b) (number->string (* a b)))

; consumes a natural number and returns 0 for evens and 1 for odds;
(lambda (n) (if (even? n) 0 1))

; consumes two inventory records and compares them by price
(lambda (ir-1 ir-2) (= (IR-price ir-1) (IR-price ir-2)))

; adds a red dot at a given Posn to a given Image
(lambda (p i)
  (place-image (circle 10 "solid" "red")
               (posn-x p)
               (posn-y p)
               i))
