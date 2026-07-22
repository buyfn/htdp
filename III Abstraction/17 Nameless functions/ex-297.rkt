#lang htdp/isl+

; A Shape is a function:
; [Posn -> Boolean]
; interpretation: if s is a shape and p a Posn, (s p)
; produces #true if p is in s, #false otherwise

; Shape Posn -> Boolean
(define (inside? s p) (s p))

; Number Number Posn -> Number
; computes the distance between points (x, y) and p
(check-expect (distance-between 0 0 (make-posn 3 4)) 5)
(check-expect (distance-between 3 4 (make-posn 0 0)) 5)
(check-expect (distance-between 1 1 (make-posn 1 1)) 0)
(check-expect (distance-between 0 0 (make-posn 0 5)) 5)
(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))

; Number Number Number -> Shape
; creates a representation for a circle of radius r
; located at (center-x, center-y)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn 0 0)) #true)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn 0 9)) #false)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn -1 3)) #true)
(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))
