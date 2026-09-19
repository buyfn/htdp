#lang htdp/isl+

(define ZERO-CHUNK-SIZE-ERROR "Cannot bundle in chunks of size 0")

; [List-of 1String] N -> [List-of String]
; checked version of bundle
(check-error (checked-bundle (explode "abcdefg") 0) ZERO-CHUNK-SIZE-ERROR)
(check-expect (checked-bundle '() 0) '())
(check-expect (checked-bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(define (checked-bundle s n)
  (if (and (= n 0) (cons? s))
      (error ZERO-CHUNK-SIZE-ERROR)
      (bundle s n)))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea: take n items and drop n at a time
; termination: loops for n = 0 unless s is empty
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
