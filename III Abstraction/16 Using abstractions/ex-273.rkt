#lang htdp/isl+
; [X -> Y] [List-of X] -> [List-of Y]
; produces a list by applying f to each element of l
(check-expect (my-map add1 (list 1 2 3))
              (map add1 (list 1 2 3)))
(define (my-map f l)
  (local (; X [List-of Y] -> [List-of Y]
          ; applies f to x and prepends to res
          (define (cons-next x res)
            (cons (f x) res)))
    (foldr cons-next '() l)))
