#lang htdp/isl+

; N -> [List-of N]
; creates a list (list 0 ... (- n 1))
(check-expect (range-from-0 0) '())
(check-expect (range-from-0 1) (list 0))
(check-expect (range-from-0 2) (list 0 1))
(define (range-from-0 n)
  (build-list n (lambda (x) x)))

; N -> [List-of N]
; creates a list (list 1 ... n)
(check-expect (range-from-1 0) '())
(check-expect (range-from-1 1) (list 1))
(check-expect (range-from-1 2) (list 1 2))
(define (range-from-1 n)
  (build-list n (lambda (x) (+ 1 x))))

; N -> [List-of Number]
; creates a list (list 1 1/2 ... 1/n)
(check-expect (range-by-n 0) '())
(check-expect (range-by-n 2)
              (list 1 (/ 1 2)))
(define (range-by-n n)
  (build-list n (lambda (x) (/ 1 (add1 x)))))

; N -> [List-of N]
; creates the list of n first even numbers
(check-expect (even 0) '())
(check-expect (even 3) (list 0 2 4))
(define (even n)
  (build-list n (lambda (x) (* x 2))))

; N -> [List-of [List-of Number]]
; Creates identity matrix of size s
(check-expect (identityM 0) '())
(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 2) (list (list 1 0)
                                  (list 0 1)))
(check-expect (identityM 3) (list (list 1 0 0)
                                  (list 0 1 0)
                                  (list 0 0 1)))
(define (identityM s)
  (build-list s (lambda (i)
                  (build-list s (lambda (j)
                                  (if (= i j) 1 0))))))

; [X] [N -> X] N -> [List-of X]
; tabulates f between n and 0 (incl.) in a list
(check-expect (tabulate add1 0) (list 1))
(check-expect (tabulate identity 3)
              (list 3 2 1 0))
(define (tabulate f n)
  (build-list (+ n 1) (lambda (i) (f (- n i)))))
