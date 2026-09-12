#lang htdp/isl+

; Any -> N
; finds the smallest n for which (expt #i10.0 n)
; is #i+inf.0
(check-expect (find-smallest-overflow 0) 309)
(define (find-smallest-overflow _)
  (find-solution
   (lambda (n) (= #i+inf.0 (expt #i10.0 n)))
   (lambda (n) (+ n 1))))

; Any -> Number
; finds the smalles n for which (expt #i10.0 n) is
; still an inexact ISL+ number and (expt #i10.0 (- n 1))
; is approximated with 0
(check-expect (find-smallest-underflow 0) -323)
(define (find-smallest-underflow _)
  (find-solution
   (lambda (n) (= #i0.0 (expt #i10.0 (- n 1))))
   (lambda (n) (- n 1))))

; [Number -> Boolean] [Number -> Number] -> Number
; finds the first n that satisfies predicate p
; by succesively apply get-next to n
(define (find-solution p get-next)
  (local ((define (iter n)
            (if (p n) n (iter (get-next n)))))
    (iter 0)))
