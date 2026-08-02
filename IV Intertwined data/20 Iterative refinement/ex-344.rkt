#lang htdp/isl+
(require htdp/dir)

; A Path is [List-of String].
; interpretation directions into a directory tree

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

; Dir String -> [List-of Path]
; produces a list of paths to all files in the whole
; directory tree with given name
(check-expect (find-all d-empty "a.txt") '())
(check-expect (find-all d-leaf "a.txt") (list (list "leaf" "a.txt")))
(check-expect (find-all d-siblings "a.txt") (list (list "top" "leaf" "a.txt")))
(check-expect (find-all d-dup "a.txt")
              (list (list "dup" "a.txt")
                    (list "dup" "leaf" "a.txt")))
(check-expect (find-all d-twins "a.txt")
              (list (list "twins" "leaf" "a.txt")
                    (list "twins" "leaf2" "a.txt")))
(check-expect (find-all d-twins "nothing") '())
(define (find-all a-dir name)
  (filter (lambda (path) (string=? (last path) name))
          (ls-R a-dir)))

; [X] [List-of X] -> [Maybe X]
; returns last element of a list,
; or #false if the list is empty
(define (last l)
  (if (empty? l)
      #false
      (first (reverse l))))
