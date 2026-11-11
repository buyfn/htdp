#lang htdp/isl+

; For n > 13, f(n) = 2^n is always more expensive than
; g(n) = 1000n since f(14) > g(14) and at every next step
; f(n) = 2^n is increased by more than 1000, while g(n) = 1000n
; is always increased by 1000 at every next step.
; g belongs to O(f) because for all n > 13 it is true that
; f(n) > g(n)
; If the input is guaranteed to be between 3 and 12, then
; f(n) = 2^n is better, because for 1 <= n <= 13 f(n) < g(n)
