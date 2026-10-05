#lang htdp/isl+

(define ε 0.01)
(define δ 0.1)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; using divide-and-conquer strategy
(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)
(define (integrate-dc f a b)
  (cond
    [(< (- b a) δ) (integrate-kepler f a b)]
    [else
     (local ((define mid (+ a (/ (- b a) 2))))
       (+ (integrate-dc f a mid)
          (integrate-dc f mid b)))]))

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε) ; fails by 125
(define (integrate-kepler f a b)
  (local ((define mid (+ a (/ (- b a) 2))))
    (+ (trapezoid-area f a mid)
       (trapezoid-area f mid b))))

; [Number -> Number] Number Number -> Number
; computes the area of a trapezoid
(define (trapezoid-area f a b)
  (/ (* (- b a) (+ (f a) (f b))) 2))
