#lang htdp/isl+
(require 2htdp/image)

; [List-of X] [List-of X] -> [List-of X]
; concatenates two lists together
(check-expect (append-from-fold (list 1 2 3)
                                (list 4 5 6 7 8))
              (append (list 1 2 3)
                      (list 4 5 6 7 8)))
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))
; if foldr is replaced with foldl, the first part
; of the result will be reveresed


; [List-of Number] -> Number
; computes sum of numbers in l
(check-expect (sum (list 1 2 3 4)) 10)
(define (sum l)
  (foldl + 0 l))

; [List-of Number] -> Number
; computes product of numbers in l
(check-expect (product (list 1 2 3 4)) 24)
(define (product l)
  (foldl * 1 l))

; [List-of Image] -> Image
; composes images in loi horizontally
(check-expect (compose-horizontally '()) empty-image)
(check-expect (compose-horizontally (list (square 10 "solid" "red")))
              (square 10 "solid" "red"))
(check-expect (compose-horizontally (list (square 10 "solid" "red")
                                          (square 20 "solid" "blue")))
              (beside (square 10 "solid" "red")
                      (square 20 "solid" "blue")))
(check-expect (compose-horizontally (list (square 10 "solid" "red")
                                          (square 20 "solid" "blue")
                                          (square 30 "solid" "green")))
              (beside (square 10 "solid" "red")
                      (square 20 "solid" "blue")
                      (square 30 "solid" "green")))
(define (compose-horizontally loi)
  (foldr beside empty-image loi))
; can't use foldl because the order of images will be reversed

; [List-of Image] -> Image
; composes images in loi vertically
(check-expect (compose-vertically '()) empty-image)
(check-expect (compose-vertically (list (square 10 "solid" "red")))
              (square 10 "solid" "red"))
(check-expect (compose-vertically (list (square 10 "solid" "red")
                                        (square 20 "solid" "blue")))
              (above (square 10 "solid" "red")
                     (square 20 "solid" "blue")))
(check-expect (compose-vertically (list (square 10 "solid" "red")
                                        (square 20 "solid" "blue")
                                        (square 30 "solid" "green")))
              (above (square 10 "solid" "red")
                     (square 20 "solid" "blue")
                     (square 30 "solid" "green")))
(define (compose-vertically loi)
  (foldr above empty-image loi))
; can't use foldl because the order of images will be reversed
