#lang htdp/isl+
(require htdp/dir)

(define P (create-dir "/Users/ignat/Learn/htdp"))

; data examples
(define f1 (make-file "a.txt" 10 "hello"))
(define f2 (make-file "b.txt" 20 "world"))
(define d-empty (make-dir "empty" '() '()))
(define d-leaf (make-dir "leaf" '() (list f1)))
(define d-nested (make-dir "root" (list d-leaf) (list f2)))

; Dir -> Number
; computes the total size of all the files in the entire directory tree
(check-expect (du d-empty) 1)
(check-expect (du d-leaf) 11)
(check-expect (du d-nested) 32)
(define (du d)
  (+ 1
     (sum (map du (dir-dirs d)))
     (sum (map file-size (dir-files d)))))

; [List-of Number] -> Number
; computes the sum of all numbers in a list
(define (sum lon)
  (foldl + 0 lon))
