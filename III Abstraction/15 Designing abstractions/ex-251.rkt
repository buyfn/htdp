#lang htdp/isl

; [X Y -> Y] Y [List-of X] -> Y
; aggregates all the values from l into
; a single value using f pairwise
(define (fold1 f initial-value l)
  (cond
    [(empty? l) initial-value]
    [else (f (first l)
             (fold1 f initial-value (rest l)))]))

; [List-of Number] -> Number
; computes the sum of the numbers in l
(check-expect (sum '()) 0)
(check-expect (sum '(1)) 1)
(check-expect (sum '(1 2 3)) 6)
(define (sum l) (fold1 + 0 l))

; [List-of Number] -> Number
; computes the product of the numbers in l
(check-expect (product '()) 1)
(check-expect (product '(2)) 2)
(check-expect (product '(2 3 4)) 24)
(define (product l) (fold1 * 1 l))
