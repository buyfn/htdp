#lang htdp/isl

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
; (build-list n f) == (list (f 0) ... (f (- n 1)))
(check-expect (build-l*st 0 add1)
              (build-list 0 add1))
(check-expect (build-l*st 3 add1)
              (build-list 3 add1))
(define (build-l*st n f)
  (cond
    [(zero? n) '()]
    [else (add-at-end (f (- n 1))
                      (build-l*st (- n 1) f))]))

; [X] X [List-of X] -> [List-of X]
; adds p at the end of list l
(check-expect (add-at-end 1 '()) '(1))
(check-expect (add-at-end 3 '(1 2)) '(1 2 3))
(define (add-at-end p l)
  (cond
    [(empty? l) (list p)]
    [else (cons (first l) (add-at-end p (rest l)))]))
