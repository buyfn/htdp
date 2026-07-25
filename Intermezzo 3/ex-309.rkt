#lang htdp/isl+
(require 2htdp/abstraction)

; [List-of [List-of String]] -> [List-of N]
; determines the number of Strings per item in lines
(check-expect (words-on-line (list (list "hi" "bob")
                                   '()
                                   (list "bye")))
              (list 2 0 1))
(check-expect (words-on-line '()) '())
(define (words-on-line lines)
  (match lines
    ['() '()]
    [(cons head tail) (cons (length head)
                            (words-on-line tail))]))

