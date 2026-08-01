#lang htdp/isl+
(require htdp/dir)
(require 2htdp/abstraction)

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

; Dir String -> [Maybe Path]
; produces path to a file with name,
; or #false if it doesn't exist
(check-expect (find d-empty "a.txt") #false)
(check-expect (find d-leaf "a.txt") (list "leaf" "a.txt"))
(check-expect (find d-nested "a.txt") (list "root" "leaf" "a.txt"))
(check-expect (find d-siblings "a.txt") (list "top" "leaf" "a.txt"))
(define (find a-dir name)
  (local ((define found-in-files?
            (ormap (lambda (f) (string=? (file-name f) name))
                   (dir-files a-dir))))
    (cond
      [found-in-files? (list (dir-name a-dir) name)]
      [else (local ((define path-in-child-dirs
                      (for/or ([d (dir-dirs a-dir)])
                        (find d name))))
              (if (false? path-in-child-dirs)
                  #false
                  (cons (dir-name a-dir)
                        path-in-child-dirs)))])))

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
(define (find-all a-dir name)
  (local ((define found-in-files?
            (ormap (lambda (f) (string=? (file-name f) name))
                   (dir-files a-dir))))
    (append (if found-in-files?
                (list (list (dir-name a-dir) name))
                '())
            (map (lambda (p) (cons (dir-name a-dir) p))
                 (foldr (lambda (d acc) (append (find-all d name) acc))
                        '()
                        (dir-dirs a-dir))))))
