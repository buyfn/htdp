#lang htdp/isl+

; A Son is a [List-of Number]
; Constraint If s is a Son,
; no number occurs twice in s

; Son Son -> Son
; produces a set that contains numbers from both input sets
(check-expect (union '() '()) '())
(check-expect (union '(1 2) '(1 2)) '(1 2))
(check-expect (union '() '(1 2)) '(1 2))
(check-expect (union '(1 2) '()) '(1 2))
(check-expect (union '(1 2) '(3 4)) '(1 2 3 4))
(check-expect (union '(1 2) '(2 3)) '(1 2 3))
(define (union s1 s2)
  (cond
    [(and (empty? s1) (empty? s2)) '()]
    [(and (empty? s1) (cons? s2)) s2]
    [(and (cons? s1) (empty? s2)) s1]
    [(and (cons? s1) (cons? s2))
     (if (member (first s1) s2)
         (union (rest s1) s2)
         (cons (first s1) (union (rest s1) s2)))]))

; Son Son -> Son
; produces a set that contains numbers occurring in both input sets
(check-expect (intersect '() '()) '())
(check-expect (intersect '(1 2) '()) '())
(check-expect (intersect '() '(1 2)) '())
(check-expect (intersect '(1 2) '(1 2)) '(1 2))
(check-expect (intersect '(1 2) '(3 4)) '())
(check-expect (intersect '(1 2) '(2 3)) '(2))
(define (intersect s1 s2)
  (cond
    [(or (empty? s1) (empty? s2)) '()]
    [(member (first s1) s2)
     (cons (first s1) (intersect (rest s1) s2))]
    [else
     (intersect (rest s1) s2)]))
