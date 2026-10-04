#lang htdp/isl+
(require 2htdp/abstraction)

(define R 1000)
(define ε 0.01)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)
(define (integrate-rectangles f a b)
  (local ((define W (/ (- b a) R))
          (define S (/ W 2)))
    (for/sum ([i R])
      (* W (f (+ a (* i W) S))))))
