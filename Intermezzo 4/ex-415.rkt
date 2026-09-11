#lang htdp/isl+

; Void -> N
; finds the smallest n for which (expt #i10.0 n)
; is #i+inf.0
(define (find-n _)
  (local ((define (iter n)
            (if (= #i+inf.0 (expt #i10.0 n))
                n
                (iter (+ n 1)))))
    (iter 0)))
