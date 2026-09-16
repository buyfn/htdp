#lang htdp/isl+

; [List-of X] N -> [List-of [List-of X]]
; transforms list l into a list of chunks of length n
; precondition: n > 0
(check-expect (list->chunks '(1 2 3 4 5) 2)
              '((1 2) (3 4) (5)))
(define (list->chunks l n)
  (cond
    [(empty? l) l]
    [else (cons (take l n)
                (list->chunks (drop l n) n))]))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; precondition: n > 0
; idea: map `implode` over the result of list->chunks
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(define (bundle l n)
  (map implode
       (list->chunks l n)))

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
