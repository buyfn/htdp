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
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; SOE Solution -> Boolean
; checks if solution is correct
(check-satisfied (list M S)
                 (lambda (input)
                   (check-solution (first input) (second input))))
(check-satisfied (list M (list 1 4 0))
                 (lambda (input)
                   (not (check-solution (first input) (second input)))))
(define (check-solution soe solution)
  (local (; Equation -> Boolean
          ; checks if equation is solved correctly
          (define (check-equation e)
            (= (calc (lhs e) solution) (rhs e))))
    (andmap check-equation soe)))

; [List-of Number] [List-of Number] -> Number
; multiplies numers in two lists pairwise and adds the results together
(check-expect (calc (list 1 2 3) (list 3 4 5)) 26)
(define (calc xs ys)
  (for/sum ((x xs) (y ys)) (* x y)))

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))
