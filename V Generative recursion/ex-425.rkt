#lang htdp/isl+

(define THRESHOLD 3)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< '(1 9 2 8 3 7 4 6 5))
              '(1 2 3 4 5 6 7 8 9))
(check-expect (quick-sort< '(1 9 2 1 8 3 7 4 6 5 5))
              '(1 1 2 3 4 5 5 6 7 8 9))
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [(<= (length alon) THRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers (rest alon) pivot))
                    (list pivot)
                    (quick-sort< (largers (rest alon) pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
; keeps only those numbers from alon, which a larger or equal to n
(define (largers alon n)
  (filter (lambda (x) (>= x n)) alon))
 
; [List-of Number] Number -> [List-of Number]
; keeps only those numbers from alon, which are smaller than n
(define (smallers alon n)
  (filter (lambda (x) (< x n)) alon))

; List-of-numbers -> List-of-numbers
; rearranges alon in ascending order
(check-expect (sort< '()) '())
(check-expect (sort< (list 3 2 1)) (list 1 2 3))
(check-expect (sort< (list 1 2 3)) (list 1 2 3))
(check-expect (sort< (list 12 20 -5)) (list -5 12 20))
(define (sort< alon)
  (cond
    [(empty? alon) '()]
    [else (insert (first alon) (sort< (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 5 6))
(check-expect (insert 5 (list 4)) (list 4 5))
(check-expect (insert 12 (list -5 20)) (list -5 12 20))
(check-expect (insert 5 (list 5)) (list 5 5))
(check-expect (insert 5 (list 4 5 6)) (list 4 5 5 6))
(check-expect (insert 25 (list -5 12 20)) (list -5 12 20 25))
(check-expect (insert -10 (list -5 12 20)) (list -10 -5 12 20))
(check-expect (insert 5 (list 5 5 5)) (list 5 5 5 5))
(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [else (if (< n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))
