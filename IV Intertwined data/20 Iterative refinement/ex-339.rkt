#lang htdp/isl+
(require htdp/dir)

; data examples
(define f1 (make-file "a.txt" 10 "hello"))
(define f2 (make-file "b.txt" 20 "world"))
(define d-empty (make-dir "empty" '() '()))
(define d-leaf (make-dir "leaf" '() (list f1)))
(define d-nested (make-dir "root" (list d-leaf) (list f2)))

; Dir String -> Boolean
; determines whether a file with given name occurs in the directory tree
(check-expect (find? d-empty "a.txt") #false)
(check-expect (find? d-leaf "a.txt") #true)
(check-expect (find? d-leaf "x.txt") #false)
(check-expect (find? d-nested "a.txt") #true)
(check-expect (find? d-nested "b.txt") #true)
(check-expect (find? d-nested "z.txt") #false)
(define (find? a-dir name)
  (or (ormap (lambda (f) (string=? (file-name f) name))
             (dir-files a-dir))
      (ormap (lambda (d) (find? d name))
             (dir-dirs a-dir))))
