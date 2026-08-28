#lang htdp/isl+

; [List-of Number] [List-of Number] -> [List-of Number]
; consumes two sorted lists of numbers and merges them into one
; preserving sorting
(check-expect (merge '() '()) '())
(check-expect (merge '(1 2) '()) '(1 2))
(check-expect (merge '() '(1 2)) '(1 2))
(check-expect (merge '(1 2) '(1 2)) '(1 1 2 2))
(check-expect (merge '(1 2) '(3 4)) '(1 2 3 4))
(check-expect (merge '(1 3) '(2 4)) '(1 2 3 4))
(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(< (first l1) (first l2))
     (cons (first l1)
           (merge (rest l1) l2))]
    [else (cons (first l2)
                (merge l1 (rest l2)))]))

