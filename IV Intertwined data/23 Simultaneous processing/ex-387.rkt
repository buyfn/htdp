#lang htdp/isl+
(require 2htdp/abstraction)

; [List-of Symbol] [List-of Number] -> [List-of (list Symbol Number)]
; produces all possible ordered pairs of symbols and numbers
(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
;; (define (cross symbols numbers)
;;   (local ((; Symbol [List-of Number] -> [List-of (list Symbol Number)]
;;            ; produces a list of pairs where to every number in ns
;;            ; added symbol s
;;            define (pairs-with s ns)
;;             (map (lambda (n) (list s n)) ns)))
;;     (foldl (lambda (s acc) (append acc (pairs-with s numbers)))
;;            '()
;;            symbols)))
(define (cross symbols numbers)
  (for*/list ((s symbols)
              (n numbers))
    (list s n)))
