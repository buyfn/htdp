#lang htdp/isl+
(require htdp/dir)

(define P (create-dir "/Users/ignat/Learn/htdp"))

; Dir -> N
; determines how many files a given dir contains
(define (how-many dir)
  (+ (foldl (lambda (d acc) (+ (how-many d) acc))
            0
            (dir-dirs dir))
     (length (dir-files dir))))

