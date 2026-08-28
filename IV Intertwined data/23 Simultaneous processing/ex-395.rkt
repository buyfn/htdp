#lang htdp/isl+

; [X] [List-of X] N -> [List-of X]
; produces the first n items of l, or all of l
; if it's too short
(check-expect (take '(1 2 3) 0) '())
(check-expect (take '(1 2 3) 1) '(1))
(check-expect (take '(1 2 3) 3) '(1 2 3))
(check-expect (take '(1 2 3) 4) '(1 2 3))
(check-expect (take '() 3) '())
(define (take l n)
  (cond
    [(or (= n 0) (empty? l)) '()]
    [else
     (cons (first l) (take (rest l) (- n 1)))]))

; [X] [List-of X] N -> [List-of X]
; produces l with first n items removed or
; just '() if l is too short
(check-expect (drop '(1 2 3) 0) '(1 2 3))
(check-expect (drop '() 0) '())
(check-expect (drop '(1 2 3) 1) '(2 3))
(check-expect (drop '(1 2 3) 3) '())
(check-expect (drop '(1 2 3) 4) '())
(check-expect (drop '() 3) '())
(define (drop l n)
  (cond
    [(or (= n 0) (empty? l)) l]
    [else (drop (rest l) (- n 1))]))
