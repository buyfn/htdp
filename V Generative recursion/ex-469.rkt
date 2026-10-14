#lang htdp/isl+
(require 2htdp/abstraction)

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
; n + 1, n, n - 1, ..., 2.
; interpretation represents a triangular matrix

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

; TM -> Solution
; solves a triangular matrix
(check-expect (solve '((2 2 3 10)
                       (3 9 21)
                       (1 2)))
              '(1 1 2))
(define (solve soe)
  (foldr (lambda (e solved) (cons (solve-eq e solved) solved))
         '()
         soe))

; Equation [List-of Number] -> Number
; solves an equation
(check-expect (solve-eq '(2 2 3 10) '(1 2)) 1)
(define (solve-eq equation vars)
  (/ (- (rhs equation)
        (for/sum ((c (rest (lhs equation)))
                  (v vars))
          (* c v)))
     (first equation)))

; SOE -> TM
; triangulates the given system of equations
; Signals an error when all of equations have 0 leading coefficients
(check-expect (triangulate '((2 2 3 10)
                             (2 5 12 31)
                             (4 1 -2 1)))
              '((2 2 3 10)
                  (3 9 21)
                  (1 2)))
(check-expect (triangulate '((2 3 3 8)
                             (2 3 -2 3)
                             (4 -2 2 4)))
              '((2 3 3 8)
                (-8 -4 -12)
                (-5 -5)))
(define (triangulate M)
  (cond
    [(all-first-zero? M) (error "All leading coefficients are 0")]
    [(empty? (rest M)) M]
    [else
     (local ((define rotated (rotate M))
             (define first-eq (first rotated))
             (define rest-eqs (rest rotated)))
       (cons first-eq
             (triangulate (map (lambda (r)
                                 (subtract r first-eq)) rest-eqs))))]))

; SOE -> Boolean
; determines whether first coefficients of all Equations are 0
(check-expect (all-first-zero? '((2 3 3 8)
                                 (2 3 -2 3)))
              #false)
(check-expect (all-first-zero? '((0 3 3 8)
                                 (2 3 -2 3)))
              #false)
(check-expect (all-first-zero? '((0 3 3 8)
                                 (0 3 -2 3)))
              #true)
(define (all-first-zero? soe)
  (andmap eq-first-zero? soe))

; SOE -> Boolean
; determines whether the first coeff in the first Equation of soe
; equals 0
(define (first-zero? soe) (eq-first-zero? (first soe)))

; Equation -> Boolean
; determines whether the first coefficient of the Equation is 0
(check-expect (eq-first-zero? '(2 3 8)) #false)
(check-expect (eq-first-zero? '(0 3 8)) #true)
(define (eq-first-zero? e) (= 0 (first e)))

; SOE -> SOE
; rotates a SOE until the first coeff
; of the first Equation is not zero.
; Terminates unless first coeffs in all equations
; equal zero
(define (rotate L)
  (if (first-zero? L)
      (rotate (append (rest L) (list (first L))))
      L))
 
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
