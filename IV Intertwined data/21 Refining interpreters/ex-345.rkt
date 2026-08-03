#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)
; interpretation: represents a valid BSL expression

(define e1 (make-add 10 -10))
(define e2 (make-add (make-mul 20 3) 33))
(define e3 (make-add (make-mul 3.14 (make-mul 2 3))
                     (make-mul 3.14 (make-mul -1 -9))))

(make-add -1 2) ; (+ -1 2)
(make-add (make-mul -2 -3) 33) ; (+ (* -2 -3) 33)
(make-mul (make-add 1 (make-mul 2 3)) 3.14) ; (* (+ 1 (* 2 3)) 3.14)
