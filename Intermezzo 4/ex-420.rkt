#lang htdp/isl+

(define (oscillate n)
  (local  ((define (O i)
              (cond
                [(> i n) '()]
                [else
                 (cons (expt #i-0.99 i)
                       (O (+ i 1)))])))
    (O 1)))

; [List-of Number] -> Number
; computes sum of numbers in l
(check-expect (sum (list 1 2 3 4)) 10)
(define (sum l)
  (foldl + 0 l))

(- (* 1e16 (sum (oscillate #i1000.0)))
   (* 1e16 (sum (reverse (oscillate #i1000.0)))))
