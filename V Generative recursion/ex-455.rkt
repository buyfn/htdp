#lang htdp/isl+

(define ε 0.01)

(define (f1 x) (- x 2))

; [Number -> Number] Number -> Number
; maps function f and a number r1 to the slope
; of f at r1
(check-expect (slope f1 3) 1)
(define (slope f r1)
  (* (/ 1 (* 2 ε))
     (- (f (+ r1 ε))
        (f (- r1 ε)))))

; [Number -> Number] Number -> Number
; maps function f and r1 to the root of the tangent
; through (r1, (f r1))
(check-expect (root-of-tangent f1 3) 2)
(define (root-of-tangent f r1)
  (- r1 (/ (f r1) (slope f r1))))
