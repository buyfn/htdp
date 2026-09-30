#lang htdp/isl+

; N [List-of Number] -> [List-of [List-of Number]]
; consumes a number n and a list of n^2 numbers lon,
; produces n x n matrix
(check-expect (create-matrix 2 (list 1 2 3 4))
              (list (list 1 2)
                    (list 3 4)))
(check-expect (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
              (list (list 1 2 3)
                    (list 4 5 6)
                    (list 7 8 9)))
(check-expect (create-matrix 1 (list 1))
              (list (list 1)))
(define (create-matrix n lon)
  (cond
    [(empty? lon) '()]
    [else (cons (take lon n)
                (create-matrix n (drop lon n)))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l)
                (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
