#lang htdp/isl+
(require htdp/dir)

; A Path is [List-of String].
; interpretation directions into a directory tree

(define P (create-dir "/Users/ignat/Learn/htdp"))

; data examples
(define f1 (make-file "a.txt" 10 "hello"))
(define f2 (make-file "b.txt" 20 "world"))
(define d-empty (make-dir "empty" '() '()))
(define d-leaf (make-dir "leaf" '() (list f1)))
(define d-nested (make-dir "root" (list d-leaf) (list f2)))
(define d-siblings (make-dir "top" (list d-empty d-leaf) '()))
(define d-dup (make-dir "dup" (list d-leaf) (list f1)))
(define d-leaf2 (make-dir "leaf2" '() (list f1)))
(define d-twins (make-dir "twins" (list d-leaf d-leaf2) '()))

; Dir -> [List-of Path]
; lists the paths to all files contained in a given Dir
(check-expect (ls-R d-empty) '())
(check-expect (ls-R d-leaf) (list (list "leaf" "a.txt")))
(check-expect (ls-R d-siblings) (list (list "top" "leaf" "a.txt")))
(check-expect (ls-R d-dup) (list (list "dup" "a.txt")
                                 (list "dup" "leaf" "a.txt")))
(check-expect (ls-R d-twins) (list (list "twins" "leaf" "a.txt")
                                   (list "twins" "leaf2" "a.txt")))
(check-expect (ls-R d-nested) (list (list "root" "b.txt")
                                    (list "root" "leaf" "a.txt")))
(define (ls-R dir)
  (append (map (lambda (f) (list (dir-name dir) (file-name f))) (dir-files dir))
          (foldr (lambda (d acc)
                   (append (map (lambda (p) (cons (dir-name dir) p))
                                (ls-R d))
                           acc))
                 '()
                 (dir-dirs dir))))
