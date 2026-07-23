#lang htdp/isl+
(require 2htdp/abstraction)

(define EUR/USD 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of USD to a list of EUR
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 1.06)) (list 1))
(check-expect (convert-euro (list 1.06 2.12))
              (list 1 2))
(define (convert-euro lod)
  (for/list ([usd-amount lod])
    (/ usd-amount EUR/USD)))
