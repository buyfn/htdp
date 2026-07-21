#lang htdp/isl+

; [X] N [List-of X] -> [List-of X]
; returns a sublist of l consisting of its first
; n elements
(check-expect (take-first 0 (list 1 2 3))
              '())
(check-expect (take-first 2 (list 1 2 3))
              (list 1 2))
(define (take-first n l)
  (build-list n (lambda (i) (list-ref l i))))

; [X] X [List-of X] -> [Maybe N -> Boolean]
; a specification for `index`
(define (is-index? x l)
  (lambda (i)
    (cond
      [(false? i) (not (member? x l))]
      [else (and (equal? (list-ref l i) x)
                 (not (member? x (take-first i l))))])))

; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(check-satisfied (index 1 (list 1 2 3))
                 (is-index? 1 (list 1 2 3)))
(check-satisfied (index 4 (list 1 2 3))
                 (is-index? 4 (list 1 2 3)))
(check-satisfied (index 2 (list 1 2 2))
                 (is-index? 2 (list 1 2 2)))
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))
