#lang htdp/isl

(define l1 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define l2 (reverse l1))

; Since extremum-in-rest is computed once per function call inside local,
; the result is computed almost instantly

; [Number Number -> Boolean] Nelon -> Number
; produces the item from l that beats every other item
; according to comparison beats?
(define (extremum beats? l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (local ((define extremum-in-rest
                    (extremum beats? (rest l))))
            (if (beats? (first l) extremum-in-rest)
                (first l)
                extremum-in-rest))]))

; Nelon -> Number
; determines the smallest number on l
(check-expect (inf-1 l1) 1)
(check-expect (inf-1 l2) 1)
(define (inf-1 l) (extremum < l))

; Nelon -> Number
; determines the largest number on l
(check-expect (sup-1 l1) 25)
(check-expect (sup-1 l2) 25)
(define (sup-1 l) (extremum > l))
