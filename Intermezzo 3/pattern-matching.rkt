#lang htdp/isl+
(require 2htdp/abstraction)

; [Non-empty-list X] -> X
; returns the last item of a non-empty list
(check-expect (last-item (list 1)) 1)
(check-expect (last-item (list 1 2)) 2)
(define (last-item nel)
  (match nel
    [(cons head '()) head]
    [(cons head tail) (last-item tail)]))
