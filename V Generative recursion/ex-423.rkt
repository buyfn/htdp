#lang htdp/isl+

; String N -> [List-of String]
; produces a list of string chunks of size n from given string s
; precondition: n > 0
(check-expect (partition "abcdefg" 2)
              (bundle (explode "abcdefg") 2))
(define (partition s n)
  (cond
    [(string=? "" s) '()]
    [(> n (string-length s)) (list s)]
    [else (cons (substring s 0 n)
                (partition (substring s n) n))]))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea: take n items and drop n at a time
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l)
                (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
