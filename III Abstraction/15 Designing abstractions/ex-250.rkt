#lang htdp/isl
; [Number -> X] Number -> [List-of X]
; tabulates f between n and 0 (incl.) in a list
(define (tabulate f n)
  (cond
    [(= n 0) (list (f 0))]
    [else (cons (f n)
                (tabulate f (- n 1)))]))

; Number -> [List-of Number]
; tabulates sin between n and 0 (incl.) in a list
(define (tab-sin n) (tabulate sin n))

; Number -> [List-of Number]
; tabulates sqrt between n and 0 (incl.) in a list
(define (tab-sqrt n) (tabulate sqrt n))
