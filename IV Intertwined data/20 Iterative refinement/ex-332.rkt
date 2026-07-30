#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct dir [name content])

; A Dir.v2 is a structure:
; (make-dir String LOFD)

; An LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String

(define dir.v2
  (make-dir "TS"
            (list "read!"
                  (make-dir "Text"
                            (list "part1"
                                  "part2"
                                  "part3"))
                  (make-dir "Libs"
                            (list (make-dir "Code"
                                            (list "hang"
                                                  "draw"))
                                  (make-dir "Docs"
                                            (list "read!")))))))

; Dir.v2 -> N
; determines how many files there are in a given directory
(check-expect (how-many (make-dir "empty" '())) 0)
(check-expect (how-many dir.v2) 7)
(define (how-many dir)
  (foldl + 0
         (map (lambda (item)
                (cond
                  [(string? item) 1]
                  [else (how-many item)]))
              (dir-content dir))))
