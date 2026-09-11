#lang htdp/isl+

(define TOLERANCE 0.0001)

; N -> Inex
; adds up n copies of #i1/185
(check-within (add 0) 0 TOLERANCE)
(check-within (add 1) (/ 1 185) TOLERANCE)
(check-within (add 185) 1 TOLERANCE)
(define (add n)
  (if (= n 0) 0 (+ #i1/185 (add (- n 1)))))

; N -> N
; counts how often 1/185 can be subtracted from
; the argument until it is 0
(check-expect (sub 0) 0)
(check-expect (sub 1) 185)
(check-expect (sub #i1.0) 186)
(define (sub n)
  (cond
    [(<= n 0) 0]
    [else (+ (sub (- n (/ 1 185)))
             1)]))
