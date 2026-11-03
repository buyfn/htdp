#lang htdp/isl+

(define QUEENS 8)

; A QP is a structure:
; (make-posn CI CI)
; A CI is an N in [0, QUEENS).
; interpretation: (make-posn r c) denotes the square at
; the r-th row and c-th column

; QP QP -> Boolean
; determines whether two queens threaten each other
(check-expect (threatening? (make-posn 3 0) (make-posn 3 7)) #true)
(check-expect (threatening? (make-posn 0 2) (make-posn 7 2)) #true)
(check-expect (threatening? (make-posn 0 0) (make-posn 7 7)) #true)
(check-expect (threatening? (make-posn 0 7) (make-posn 7 0)) #true)
(check-expect (threatening? (make-posn 1 2) (make-posn 4 6)) #false)
(define (threatening? q1 q2)
  (local ((define same-row? (= (posn-x q1) (posn-x q2)))
          (define same-col? (= (posn-y q1) (posn-y q2)))
          (define same-diagonal? (= (abs (- (posn-x q1) (posn-x q2)))
                                    (abs (- (posn-y q1) (posn-y q2))))))
    (or same-row? same-col? same-diagonal?)))
