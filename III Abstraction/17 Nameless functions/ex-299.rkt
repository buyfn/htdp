#lang htdp/isl+

; A [Set-of X] is a function
; [X -> Boolean]
; interpretation if s is a set and ed is an X,
; (s ed) produces #true if ed is in s, #false otherwise

; [X] [Set-of X] X -> Boolean
; determines whether s contains ed
(define (contains? s ed) (s ed))

; [Set-of Number]
(define odds odd?)
(check-expect (contains? odds 4) #false)
(check-expect (contains? odds 5) #true)

; [Set-of Number]
(define evens even?)
(check-expect (contains? evens 3) #false)
(check-expect (contains? evens 4) #true)

; [Set-of Number]
(define divisible-by-10
  (lambda (n) (= 0 (modulo n 10))))
(check-expect (contains? divisible-by-10 100) #true)
(check-expect (contains? divisible-by-10 101) #false)

; Empty set
(define empty-set
  (lambda (x) #false))

; [X] X [Set-of X] -> [Set-of X]
; adds element to a set
(check-expect (contains? (add-element "hello" empty-set) "hello") #true)
(define (add-element elt s)
  (cond
    [(s elt) s]
    [else
     (lambda (x)
       (or (s x)
           (equal? x elt)))]))

; [X] [Set-of X] [Set-of X] -> [Set-of X]
; combines the elements of two sets
(check-expect (contains? (union odds empty-set) 5) #true)
(check-expect (contains? (union evens empty-set) 5) #false)
(define (union set-1 set-2)
  (lambda (x)
    (or (set-1 x)
        (set-2 x))))

; [X] [Set-of X] [Set-of X] -> [Set-of X]
; collects all elements common in two sets
(check-expect (contains? (intersect odds evens) 10) #false)
(check-expect (contains? (intersect odds divisible-by-10) 10) #false)
(check-expect (contains? (intersect evens divisible-by-10) 10) #true)
(define (intersect set-1 set-2)
  (lambda (x)
    (and (set-1 x)
         (set-2 x))))
