#lang htdp/isl+

; [X] [List-of X] [List-of X] -> [List-of X]
; replaces the final '() in front with end
(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with '(a b) '()) '(a b))
(check-expect (replace-eol-with '(a b) '(c d)) '(a b c d))
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [(cons? front)
     (cons (first front)
           (replace-eol-with (rest front) end))]))
