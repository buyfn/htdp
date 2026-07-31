#lang htdp/isl+
(require htdp/dir)

; data examples
(define f1 (make-file "a.txt" 10 "hello"))
(define f2 (make-file "b.txt" 20 "world"))
(define d-empty (make-dir "empty" '() '()))
(define d-leaf (make-dir "leaf" '() (list f1)))
(define d-nested (make-dir "root" (list d-leaf) (list f2)))

; Dir -> [List-of String]
; Lists the names of all files and directories in a given dir
(check-expect (ls d-empty) '())
(check-expect (ls d-leaf) (list "a.txt"))
(define (ls d)
  (append
   (map dir-name (dir-dirs d))
   (map file-name (dir-files d))))
