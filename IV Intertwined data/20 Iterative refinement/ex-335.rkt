#lang htdp/isl+

(define-struct file [name size content])

; A File.v3 is a structure:
; (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure:
; (make-dir.v3 String Dir* File*)

; A Dir* is a [List-of Dir.v3]

; A File* is a [List-of File.v3]


(define a-dir.v3
  (make-dir.v3 "TS"
               (list (make-dir.v3 "Text"
                                  '()
                                  (list (make-file "part1"
                                                   99
                                                   "")
                                        (make-file "part2"
                                                   52
                                                   "")
                                        (make-file "part3"
                                                   17
                                                   "")))
                     (make-dir.v3 "Libs"
                                  (list (make-dir.v3 "Code"
                                                     '()
                                                     (list (make-file "hang"
                                                                      8
                                                                      "")
                                                           (make-file "draw"
                                                                      2
                                                                      "")))
                                        (make-dir.v3 "Docs"
                                                     '()
                                                     (list (make-file "read!"
                                                                      19
                                                                      ""))))
                               '()))
               (list (make-file "read!" 10 ""))))

; Dir.v3 -> N
; determines how many files a given dir contains
(check-expect (how-many a-dir.v3) 7)
(define (how-many dir)
  (+ (foldl (lambda (d acc) (+ (how-many d) acc))
            0
            (dir.v3-dirs dir))
     (length (dir.v3-files dir))))
