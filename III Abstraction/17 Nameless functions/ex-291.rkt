#lang htdp/isl+

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; produces a list by applying f to each element of l
(check-expect (map-via-fold add1 (list 1 2 3))
              (map add1 (list 1 2 3)))
(define (map-via-fold f l)
  (foldr (lambda (x res)
           (cons (f x) res))
         '()
         l))
