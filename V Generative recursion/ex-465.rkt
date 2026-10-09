#lang htdp/isl+
(require 2htdp/abstraction)

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
; Equation Equation -> Equation
; Subtracts a multiple of the second equation from the first,
; so that the resulting Equation has a 0 in the first position
; assume equations are of equal lengths
; assume (first e2) is not zero
(check-expect (subtract (list 2 5 12 31)
                        (list 2 2 3 10))
              (list 3 9 21))
(check-expect (subtract (list 3 9 21)
                        (list -3 -8 -19))
              (list 1 2))
(define (subtract e1 e2)
  (local ((define factor (/ (first e1) (first e2))))
    (rest
     (map (lambda (x y) (- x (* y factor))) e1 e2))))
