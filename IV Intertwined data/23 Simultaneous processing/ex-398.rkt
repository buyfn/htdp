#lang htdp/isl+

; A LinearCombination is a [List-of Number]
; items in the list represent coefficients

; [List-of Number] [List-of Number] -> Number
; consumes a linear combination and a list of variable values
; produces the value of the combination for these values
; Assumption: both input lists are of the same length
(check-expect (value '(5) '(1)) 5)
(check-expect (value '(5 17) '(2 1)) 27)
(define (value coeffs values)
  (cond
    [(empty? coeffs) 0]
    [else (+ (* (first coeffs)
                (first values))
             (value (rest coeffs)
                    (rest values)))]))
