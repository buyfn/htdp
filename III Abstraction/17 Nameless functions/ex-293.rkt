#lang htdp/isl+

; N [List-of X] -> [List-of X]
; removes last n items from l
(check-expect (remove-last 3 (list 1 2 3 4 5))
              (list 1 2))
(define (remove-last n l)
  (local ((define to-keep (- (length l) n)))
    (build-list to-keep (lambda (i) (list-ref l i)))))

; N [List-of X] -> [List-of X]
; removes first n items from l
(check-expect (drop-first 3 (list 1 2 3 4 5))
              (list 4 5))
(define (drop-first n l)
  (reverse (remove-last n (reverse l))))

; [List-of X] [List-of X] -> Boolean
; is l2 a suffix of l1?
(check-expect (suffix-of? (list 1 2 3) '())
              #true)
(check-expect (suffix-of? (list 1)
                           (list 1))
              #true)
(check-expect (suffix-of? (list 1 2)
                           (list 2))
              #true)
(check-expect (suffix-of? (list 1 2 3)
                           (list 2 3))
              #true)
(check-expect (suffix-of? (list 1 2 3)
                          (list 3 4))
              #false)
(check-expect (suffix-of? (list 2 3 2)
                          (list 2 2))
              #false)
(check-expect (suffix-of? (list 5 9 2)
                          (list 5 2))
              #false)
(check-expect (suffix-of? (list 1 1 2)
                          (list 1 2))
              #true)
(define (suffix-of? l1 l2)
  (equal? (drop-first (- (length l1) (length l2)) l1)
          l2))

; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
; specification for `find`
(define (found? x l)
  (lambda (l0)
    (cond
      [(false? l0) (not (member? x l))]
      [else (and (equal? (first l0) x)
                 (suffix-of? l l0)
                 (not (member? x (remove-last (length l0) l))))])))

; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(check-satisfied (find 2 (list 1 2 3))
                 (found? 2 (list 1 2 3)))
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

;; (check-satisfied (find/bad 2 (list 1 2 3))
;;                  (found? 2 (list 1 2 3)))
;; (define (find/bad x l)
;;   (if (member x l) (list x) #false))
