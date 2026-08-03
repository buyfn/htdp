#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)
; interpretation: represents a valid BSL expression

; A BSL-value is a Number

(define e1 (make-add 10 -10))
(define e2 (make-add (make-mul 20 3) 33))
(define e3 (make-add (make-mul 3.14 (make-mul 2 3))
                     (make-mul 3.14 (make-mul -1 -9))))

(make-add -1 2) ; (+ -1 2)
(make-add (make-mul -2 -3) 33) ; (+ (* -2 -3) 33)
(make-mul (make-add 1 (make-mul 2 3)) 3.14) ; (* (+ 1 (* 2 3)) 3.14)

; BSL-expr -> BSL-value
; evaluates a BSL-expr
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression e1) 0)
(check-expect (eval-expression e2) 93)
(check-expect (eval-expression e3) 47.1)
(define (eval-expression bsl-expr)
  (match bsl-expr
    [(? number?) bsl-expr]
    [(add left right) (+ (eval-expression left) (eval-expression right))]
    [(mul left right) (* (eval-expression left) (eval-expression right))]))
