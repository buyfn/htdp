#lang htdp/isl+
(require 2htdp/abstraction)

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a String

(define dir.v1 (list "read!"
                     (list "part1" "part2" "part3")
                     (list (list "hang" "draw")
                           (list "read!"))))

; Dir.v1 -> N
; determines how many files given dir contains
(check-expect (how-many '()) 0)
(check-expect (how-many dir.v1) 7)
(define (how-many dir)
  (match dir
    ['() 0]
    [(cons (? string?) rest) (+ 1 (how-many rest))]
    [(cons head tail) (+ (how-many head)
                         (how-many tail))]))
