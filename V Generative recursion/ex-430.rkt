#lang htdp/isl+

; [X] [List-of X] [X X -> Boolean] -> [List-of X]
; produces a sorted version of alon according to comparator function
(check-expect (quick-sort '(1 9 2 8 3 7 4 6 5) <)
              '(1 2 3 4 5 6 7 8 9))
(check-expect (quick-sort '(1 9 2 1 8 3 7 4 6 5 5) <)
              '(1 1 2 3 4 5 5 6 7 8 9))
(define (quick-sort alon comparator)
  (local (; [List-of X] X -> [List-of X]
          ; keeps only those elements from alon, which are smaller than n
          ; according to comparator
          (define (smallers alon n)
            (filter (lambda (x) (comparator x n)) alon))
          ; [List-of X] X -> [List-of X]
          ; keeps only those elements from alon, which are not smaller than n
          ; according to comparator
          (define (largers alon n)
            (filter (lambda (x) (not (comparator x n))) alon)))
    (cond
      [(empty? alon) '()]
      [(empty? (rest alon)) alon]
      [else (local ((define pivot (first alon)))
              (append (quick-sort (smallers (rest alon)
                                            pivot)
                                  comparator)
                      (list pivot)
                      (quick-sort (largers (rest alon)
                                           pivot)
                                  comparator)))])))
 
